package healthsnap

import (
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// JSONPath is a compiled JSONPath expression starting at $.
type JSONPath struct {
	raw   string
	steps []jpStep
}

type jpStepKind int

const (
	jpField jpStepKind = iota
	jpIndex
	jpWildcard
	jpFilter
	jpAgg
)

type jpStep struct {
	kind   jpStepKind
	field  string
	index  int
	filter jpFilterNode
	agg    string
}

type jpFilterNode interface {
	eval(item any) bool
}

type jpFilterSeg struct {
	isIndex bool
	field   string
	index   int
}

type jpFilterOperand interface {
	eval(item any) any
}

type jpFilterPathOperand struct {
	segs []jpFilterSeg
}

func (o jpFilterPathOperand) eval(item any) any {
	return lookupFilterPath(item, o.segs)
}

type jpFilterLitOperand struct {
	val any
}

func (o jpFilterLitOperand) eval(any) any {
	return o.val
}

type jpFilterLogicNode struct {
	op    string
	left  jpFilterNode
	right jpFilterNode
}

func (n jpFilterLogicNode) eval(item any) bool {
	if n.op == "&&" {
		return n.left.eval(item) && n.right.eval(item)
	}
	return n.left.eval(item) || n.right.eval(item)
}

type jpFilterNotNode struct {
	child jpFilterNode
}

func (n jpFilterNotNode) eval(item any) bool {
	return !n.child.eval(item)
}

type jpFilterCmpNode struct {
	op    string
	left  jpFilterOperand
	right jpFilterOperand
}

func (n jpFilterCmpNode) eval(item any) bool {
	matched, err := compareValues(n.left.eval(item), n.op, n.right.eval(item))
	return err == nil && matched
}

type jpFilterPathBoolNode struct {
	segs []jpFilterSeg
}

func (n jpFilterPathBoolNode) eval(item any) bool {
	b, ok := lookupFilterPath(item, n.segs).(bool)
	return ok && b
}

type jpFilterBoolLitNode struct {
	val bool
}

func (n jpFilterBoolLitNode) eval(any) bool {
	return n.val
}

// CompileJSONPath parses a JSONPath expression beginning with $.
func CompileJSONPath(raw string) (JSONPath, error) {
	s := strings.TrimSpace(raw)
	if !strings.HasPrefix(s, "$") {
		return JSONPath{}, fmt.Errorf("jsonpath %q must start with $", raw)
	}
	pos := 1
	var steps []jpStep
	for pos < len(s) {
		step, nextPos, err := parseNextJPStep(s, pos)
		if err != nil {
			return JSONPath{}, fmt.Errorf("jsonpath %q: %w", raw, err)
		}
		steps = append(steps, step)
		pos = nextPos
	}
	return JSONPath{raw: raw, steps: steps}, nil
}

func parseNextJPStep(s string, pos int) (jpStep, int, error) {
	if s[pos] == '.' {
		pos++
		if pos >= len(s) {
			return jpStep{}, 0, errors.New("trailing dot")
		}
		if s[pos] == '[' {
			return parseJPBracket(s, pos)
		}
		return parseJPDot(s, pos)
	}
	if s[pos] == '[' {
		return parseJPBracket(s, pos)
	}
	return jpStep{}, 0, fmt.Errorf("unexpected character %q at offset %d", s[pos], pos)
}

func parseJPDot(s string, pos int) (jpStep, int, error) {
	end := pos
	for end < len(s) && s[end] != '.' && s[end] != '[' && s[end] != '(' {
		end++
	}
	if end == pos {
		return jpStep{}, 0, fmt.Errorf("empty field at offset %d", pos)
	}
	name := s[pos:end]
	if end < len(s) && s[end] == '(' {
		if end+1 >= len(s) || s[end+1] != ')' {
			return jpStep{}, 0, fmt.Errorf("invalid call %q", s[pos:])
		}
		switch name {
		case "length", "count", "min", "max", "sum":
			return jpStep{kind: jpAgg, agg: name}, end + 2, nil
		default:
			return jpStep{}, 0, fmt.Errorf("unsupported jsonpath function %q", name)
		}
	}
	return jpStep{kind: jpField, field: name}, end, nil
}

func parseJPBracket(s string, pos int) (jpStep, int, error) {
	closeIdx, err := findMatchingBracket(s, pos)
	if err != nil {
		return jpStep{}, 0, err
	}
	inner := strings.TrimSpace(s[pos+1 : closeIdx])
	if inner == "*" {
		return jpStep{kind: jpWildcard}, closeIdx + 1, nil
	}
	if strings.HasPrefix(inner, "?") {
		f, err := parseJPFilter(strings.TrimSpace(inner[1:]))
		if err != nil {
			return jpStep{}, 0, err
		}
		return jpStep{kind: jpFilter, filter: f}, closeIdx + 1, nil
	}
	if key, ok, err := parseBracketStringKey(inner); err != nil {
		return jpStep{}, 0, err
	} else if ok {
		return jpStep{kind: jpField, field: key}, closeIdx + 1, nil
	}
	idx, err := strconv.Atoi(inner)
	if err != nil {
		return jpStep{}, 0, fmt.Errorf("invalid bracket selector %q", inner)
	}
	return jpStep{kind: jpIndex, index: idx}, closeIdx + 1, nil
}

func findMatchingBracket(s string, openPos int) (int, error) {
	var quote byte
	escaped := false
	depth := 0
	for i := openPos + 1; i < len(s); i++ {
		c := s[i]
		if escaped {
			escaped = false
			continue
		}
		if quote != 0 {
			switch c {
			case '\\':
				escaped = true
			case quote:
				quote = 0
			}
			continue
		}
		if c == '\'' || c == '"' {
			quote = c
			continue
		}
		if c == '[' {
			depth++
			continue
		}
		if c == ']' {
			if depth == 0 {
				return i, nil
			}
			depth--
		}
	}
	return 0, errors.New("unclosed '['")
}

func parseBracketStringKey(inner string) (string, bool, error) {
	if len(inner) == 0 || (inner[0] != '"' && inner[0] != '\'') {
		return "", false, nil
	}
	val, next, err := scanJPStringLiteral(inner, 0)
	if err != nil {
		return "", false, err
	}
	if next != len(inner) {
		return "", false, fmt.Errorf("invalid bracket key %q", inner)
	}
	return val, true, nil
}

type jpTokKind int

const (
	jpTokEOF jpTokKind = iota
	jpTokLParen
	jpTokRParen
	jpTokBang
	jpTokLogicOp
	jpTokCmpOp
	jpTokPath
	jpTokString
	jpTokNumber
	jpTokBool
	jpTokNil
)

type jpToken struct {
	kind jpTokKind
	text string
	num  float64
	b    bool
	segs []jpFilterSeg
}

func parseJPFilter(raw string) (jpFilterNode, error) {
	if strings.TrimSpace(raw) == "" {
		return nil, errors.New("empty filter expression")
	}
	tokens, err := tokenizeJPFilter(raw)
	if err != nil {
		return nil, err
	}
	p := jpFilterParser{tokens: tokens}
	root, err := p.parseOr()
	if err != nil {
		return nil, err
	}
	if p.peek().kind != jpTokEOF {
		return nil, fmt.Errorf("unexpected token %q in filter", p.peek().text)
	}
	return root, nil
}

func tokenizeJPFilter(s string) ([]jpToken, error) {
	var tokens []jpToken
	for i := 0; i < len(s); {
		if isJPSpace(s[i]) {
			i++
			continue
		}
		tok, next, err := nextJPFilterToken(s, i)
		if err != nil {
			return nil, err
		}
		tokens = append(tokens, tok)
		i = next
	}
	tokens = append(tokens, jpToken{kind: jpTokEOF})
	return tokens, nil
}

func isJPSpace(c byte) bool {
	return c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

func nextJPFilterToken(s string, i int) (jpToken, int, error) {
	if tok, next, ok := scanJPOpOrPunct(s, i); ok {
		return tok, next, nil
	}
	switch {
	case s[i] == '@':
		return scanJPFilterPath(s, i)
	case s[i] == '"' || s[i] == '\'':
		val, next, err := scanJPStringLiteral(s, i)
		if err != nil {
			return jpToken{}, 0, err
		}
		return jpToken{kind: jpTokString, text: val}, next, nil
	case isJPNumberStart(s, i):
		return scanJPNumberLiteral(s, i)
	case isJPIdentStart(s[i]):
		return scanJPKeyword(s, i)
	default:
		return jpToken{}, 0, fmt.Errorf("unexpected character %q in filter at offset %d", s[i], i)
	}
}

func scanJPOpOrPunct(s string, i int) (jpToken, int, bool) {
	if i+1 < len(s) {
		two := s[i : i+2]
		switch two {
		case "&&", "||":
			return jpToken{kind: jpTokLogicOp, text: two}, i + 2, true
		case "==", "!=", "<=", ">=":
			return jpToken{kind: jpTokCmpOp, text: two}, i + 2, true
		}
	}
	switch s[i] {
	case '(':
		return jpToken{kind: jpTokLParen, text: "("}, i + 1, true
	case ')':
		return jpToken{kind: jpTokRParen, text: ")"}, i + 1, true
	case '!':
		return jpToken{kind: jpTokBang, text: "!"}, i + 1, true
	case '<', '>':
		return jpToken{kind: jpTokCmpOp, text: s[i : i+1]}, i + 1, true
	default:
		return jpToken{}, 0, false
	}
}

func scanJPFilterPath(s string, i int) (jpToken, int, error) {
	j := i + 1
	var segs []jpFilterSeg
	for j < len(s) {
		switch s[j] {
		case '.':
			j++
			if j >= len(s) || !isJPIdentStart(s[j]) {
				return jpToken{}, 0, fmt.Errorf("expected field name after '.' in filter at offset %d", j)
			}
			start := j
			for j < len(s) && isJPIdentCont(s[j]) {
				j++
			}
			segs = append(segs, jpFilterSeg{field: s[start:j]})
		case '[':
			closeIdx, err := findMatchingBracket(s, j)
			if err != nil {
				return jpToken{}, 0, err
			}
			inner := strings.TrimSpace(s[j+1 : closeIdx])
			if key, ok, err := parseBracketStringKey(inner); err != nil {
				return jpToken{}, 0, err
			} else if ok {
				segs = append(segs, jpFilterSeg{field: key})
			} else if idx, convErr := strconv.Atoi(inner); convErr == nil {
				segs = append(segs, jpFilterSeg{isIndex: true, index: idx})
			} else {
				return jpToken{}, 0, fmt.Errorf("invalid bracket selector %q in filter", inner)
			}
			j = closeIdx + 1
		default:
			return jpToken{kind: jpTokPath, text: s[i:j], segs: segs}, j, nil
		}
	}
	return jpToken{kind: jpTokPath, text: s[i:j], segs: segs}, j, nil
}

func scanJPStringLiteral(s string, i int) (string, int, error) {
	quote := s[i]
	var b strings.Builder
	escaped := false
	for j := i + 1; j < len(s); j++ {
		c := s[j]
		if escaped {
			b.WriteByte(unescapeChar(c))
			escaped = false
			continue
		}
		if c == '\\' {
			escaped = true
			continue
		}
		if c == quote {
			return b.String(), j + 1, nil
		}
		b.WriteByte(c)
	}
	return "", 0, errors.New("unterminated string literal")
}

func isJPNumberStart(s string, i int) bool {
	if s[i] >= '0' && s[i] <= '9' {
		return true
	}
	return s[i] == '-' && i+1 < len(s) && s[i+1] >= '0' && s[i+1] <= '9'
}

func scanJPNumberLiteral(s string, i int) (jpToken, int, error) {
	j := i
	if s[j] == '-' {
		j++
	}
	for j < len(s) {
		c := s[j]
		if (c == 'e' || c == 'E') && j+1 < len(s) && (s[j+1] == '+' || s[j+1] == '-') {
			j += 2
			continue
		}
		if isJPIdentCont(c) || c == '.' {
			j++
			continue
		}
		break
	}
	raw := s[i:j]
	n, err := strconv.ParseFloat(raw, 64)
	if err != nil {
		return jpToken{}, 0, fmt.Errorf("invalid numeric literal %q in filter", raw)
	}
	return jpToken{kind: jpTokNumber, text: raw, num: n}, j, nil
}

func isJPIdentStart(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func isJPIdentCont(c byte) bool {
	return isJPIdentStart(c) || (c >= '0' && c <= '9')
}

func scanJPKeyword(s string, i int) (jpToken, int, error) {
	j := i
	for j < len(s) && isJPIdentCont(s[j]) {
		j++
	}
	word := s[i:j]
	switch word {
	case "true":
		return jpToken{kind: jpTokBool, text: word, b: true}, j, nil
	case "false":
		return jpToken{kind: jpTokBool, text: word, b: false}, j, nil
	case "null", "nil":
		return jpToken{kind: jpTokNil, text: word}, j, nil
	default:
		return jpToken{}, 0, fmt.Errorf("unexpected identifier %q in filter (paths must start with @.)", word)
	}
}

type jpFilterParser struct {
	tokens []jpToken
	pos    int
}

func (p *jpFilterParser) peek() jpToken {
	return p.tokens[p.pos]
}

func (p *jpFilterParser) next() jpToken {
	tok := p.tokens[p.pos]
	p.pos++
	return tok
}

func (p *jpFilterParser) parseOr() (jpFilterNode, error) {
	left, err := p.parseAnd()
	if err != nil {
		return nil, err
	}
	for p.peek().kind == jpTokLogicOp && p.peek().text == "||" {
		op := p.next().text
		right, err := p.parseAnd()
		if err != nil {
			return nil, err
		}
		left = jpFilterLogicNode{op: op, left: left, right: right}
	}
	return left, nil
}

func (p *jpFilterParser) parseAnd() (jpFilterNode, error) {
	left, err := p.parseNot()
	if err != nil {
		return nil, err
	}
	for p.peek().kind == jpTokLogicOp && p.peek().text == "&&" {
		op := p.next().text
		right, err := p.parseNot()
		if err != nil {
			return nil, err
		}
		left = jpFilterLogicNode{op: op, left: left, right: right}
	}
	return left, nil
}

func (p *jpFilterParser) parseNot() (jpFilterNode, error) {
	if p.peek().kind == jpTokBang {
		p.next()
		child, err := p.parseNot()
		if err != nil {
			return nil, err
		}
		return jpFilterNotNode{child: child}, nil
	}
	return p.parsePrimary()
}

func (p *jpFilterParser) parsePrimary() (jpFilterNode, error) {
	tok := p.next()
	if tok.kind == jpTokLParen {
		inner, err := p.parseOr()
		if err != nil {
			return nil, err
		}
		if p.peek().kind != jpTokRParen {
			return nil, errors.New("expected ')' in filter")
		}
		p.next()
		return inner, nil
	}
	left, ok := jpTokenToOperand(tok)
	if !ok {
		return nil, fmt.Errorf("unexpected token %q in filter", tok.text)
	}
	if p.peek().kind == jpTokCmpOp {
		op := p.next().text
		rightTok := p.next()
		right, ok := jpTokenToOperand(rightTok)
		if !ok {
			return nil, fmt.Errorf("expected operand after %s in filter, got %q", op, rightTok.text)
		}
		return jpFilterCmpNode{op: op, left: left, right: right}, nil
	}
	switch tok.kind {
	case jpTokPath:
		return jpFilterPathBoolNode{segs: tok.segs}, nil
	case jpTokBool:
		return jpFilterBoolLitNode{val: tok.b}, nil
	default:
		return nil, fmt.Errorf("expected comparison operator after %q in filter", tok.text)
	}
}

func jpTokenToOperand(tok jpToken) (jpFilterOperand, bool) {
	switch tok.kind {
	case jpTokPath:
		return jpFilterPathOperand{segs: tok.segs}, true
	case jpTokString:
		return jpFilterLitOperand{val: tok.text}, true
	case jpTokNumber:
		return jpFilterLitOperand{val: tok.num}, true
	case jpTokBool:
		return jpFilterLitOperand{val: tok.b}, true
	case jpTokNil:
		return jpFilterLitOperand{val: nil}, true
	default:
		return nil, false
	}
}

// EvalBytes unmarshals raw JSON bytes and evaluates the JSONPath expression.
func (p JSONPath) EvalBytes(raw []byte) (any, error) {
	var doc any
	if err := json.Unmarshal(raw, &doc); err != nil {
		return nil, fmt.Errorf("decode json: %w", err)
	}
	return p.Eval(doc)
}

// Eval evaluates the JSONPath expression against a decoded JSON document.
func (p JSONPath) Eval(doc any) (any, error) {
	cur := doc
	multi := false
	for _, step := range p.steps {
		var err error
		cur, multi, err = applyJPStep(cur, multi, step)
		if err != nil {
			return nil, fmt.Errorf("jsonpath %q: %w", p.raw, err)
		}
	}
	return cur, nil
}

func applyJPStep(cur any, multi bool, step jpStep) (any, bool, error) {
	if step.kind == jpAgg {
		v, err := evalJPAgg(cur, step.agg)
		return v, false, err
	}
	if !multi {
		return evalJPSingle(cur, step)
	}
	items, ok := cur.([]any)
	if !ok {
		return nil, false, fmt.Errorf("expected slice for projection, got %T", cur)
	}
	out := make([]any, 0, len(items))
	for _, item := range items {
		val, itemMulti, err := evalJPSingle(item, step)
		if err != nil {
			return nil, false, err
		}
		if itemMulti {
			if sub, ok := val.([]any); ok {
				out = append(out, sub...)
				continue
			}
		}
		out = append(out, val)
	}
	return out, true, nil
}

func evalJPSingle(cur any, step jpStep) (any, bool, error) {
	switch step.kind {
	case jpField:
		obj, ok := cur.(map[string]any)
		if !ok {
			return nil, false, fmt.Errorf("cannot read field %q from %T", step.field, cur)
		}
		val, exists := obj[step.field]
		if !exists {
			return nil, false, fmt.Errorf("key %q not found", step.field)
		}
		return val, false, nil
	case jpIndex:
		return evalJPIndex(cur, step.index)
	case jpWildcard:
		arr, ok := cur.([]any)
		if !ok {
			return nil, false, fmt.Errorf("wildcard [*] requires array, got %T", cur)
		}
		return arr, true, nil
	case jpFilter:
		return evalJPFilter(cur, step.filter)
	default:
		return nil, false, fmt.Errorf("unexpected step kind %d", step.kind)
	}
}

func evalJPIndex(cur any, idx int) (any, bool, error) {
	arr, ok := cur.([]any)
	if !ok {
		return nil, false, fmt.Errorf("index [%d] requires array, got %T", idx, cur)
	}
	resolved := idx
	if resolved < 0 {
		resolved = len(arr) + resolved
	}
	if resolved < 0 || resolved >= len(arr) {
		return nil, false, fmt.Errorf("index %d out of bounds (length %d)", idx, len(arr))
	}
	return arr[resolved], false, nil
}

func evalJPFilter(cur any, f jpFilterNode) (any, bool, error) {
	arr, ok := cur.([]any)
	if !ok {
		return nil, false, fmt.Errorf("filter requires array, got %T", cur)
	}
	out := make([]any, 0)
	for _, item := range arr {
		if f.eval(item) {
			out = append(out, item)
		}
	}
	return out, true, nil
}

func lookupFilterPath(item any, segs []jpFilterSeg) any {
	cur := item
	for _, seg := range segs {
		if seg.isIndex {
			arr, ok := cur.([]any)
			if !ok {
				return nil
			}
			idx := seg.index
			if idx < 0 {
				idx += len(arr)
			}
			if idx < 0 || idx >= len(arr) {
				return nil
			}
			cur = arr[idx]
			continue
		}
		obj, ok := cur.(map[string]any)
		if !ok {
			return nil
		}
		val, exists := obj[seg.field]
		if !exists {
			return nil
		}
		cur = val
	}
	return cur
}

func evalJPAgg(cur any, agg string) (any, error) {
	if agg == "length" || agg == "count" {
		switch v := cur.(type) {
		case []any:
			return float64(len(v)), nil
		case map[string]any:
			return float64(len(v)), nil
		case string:
			return float64(len(v)), nil
		default:
			return nil, fmt.Errorf("%s() unsupported on %T", agg, cur)
		}
	}
	arr, ok := cur.([]any)
	if !ok {
		return nil, fmt.Errorf("%s() requires array, got %T", agg, cur)
	}
	if len(arr) == 0 {
		return float64(0), nil
	}
	return aggregateNumbers(arr, agg)
}

func aggregateNumbers(arr []any, agg string) (float64, error) {
	var acc float64
	hasVal := false
	for _, item := range arr {
		if item == nil {
			continue
		}
		n, ok := toFloat(item)
		if !ok {
			return 0, fmt.Errorf("%s() found non-numeric value %T", agg, item)
		}
		if !hasVal {
			acc = n
			hasVal = true
			continue
		}
		switch agg {
		case "min":
			if n < acc {
				acc = n
			}
		case "max":
			if n > acc {
				acc = n
			}
		case "sum":
			acc += n
		}
	}
	return acc, nil
}
