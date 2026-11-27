package healthsnap

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"unicode"
)

// Expr is a compiled boolean threshold expression.
type Expr struct {
	raw  string
	root exprNode
}

type exprNode interface {
	eval(vars map[string]any) (any, error)
}

type literalNode struct {
	val any
}

func (n literalNode) eval(map[string]any) (any, error) {
	return n.val, nil
}

type identNode struct {
	name string
}

func (n identNode) eval(vars map[string]any) (any, error) {
	v, ok := vars[n.name]
	if !ok {
		return nil, fmt.Errorf("unknown variable %q", n.name)
	}
	return v, nil
}

type notNode struct {
	child exprNode
}

func (n notNode) eval(vars map[string]any) (any, error) {
	v, err := n.child.eval(vars)
	if err != nil {
		return nil, err
	}
	b, ok := v.(bool)
	if !ok {
		return nil, fmt.Errorf("operator ! requires bool, got %T", v)
	}
	return !b, nil
}

type logicNode struct {
	op    string
	left  exprNode
	right exprNode
}

func (n logicNode) eval(vars map[string]any) (any, error) {
	lv, err := n.left.eval(vars)
	if err != nil {
		return nil, err
	}
	lb, ok := lv.(bool)
	if !ok {
		return nil, fmt.Errorf("operator %s requires bool, got %T", n.op, lv)
	}
	if n.op == "&&" && !lb {
		return false, nil
	}
	if n.op == "||" && lb {
		return true, nil
	}
	rv, err := n.right.eval(vars)
	if err != nil {
		return nil, err
	}
	rb, ok := rv.(bool)
	if !ok {
		return nil, fmt.Errorf("operator %s requires bool, got %T", n.op, rv)
	}
	return rb, nil
}

type cmpNode struct {
	op    string
	left  exprNode
	right exprNode
	re    *regexp.Regexp
}

func (n cmpNode) eval(vars map[string]any) (any, error) {
	lv, err := n.left.eval(vars)
	if err != nil {
		return nil, err
	}
	rv, err := n.right.eval(vars)
	if err != nil {
		return nil, err
	}
	if n.re != nil {
		s, ok := lv.(string)
		if !ok {
			if lv == nil {
				return false, nil
			}
			s = fmt.Sprint(lv)
		}
		return n.re.MatchString(s), nil
	}
	return compareValues(lv, n.op, rv)
}

type durationSeconds float64

type tokKind int

const (
	tokEOF tokKind = iota
	tokIdent
	tokNumber
	tokDuration
	tokString
	tokBool
	tokNil
	tokOp
	tokLParen
	tokRParen
	tokBang
)

type exprToken struct {
	kind tokKind
	text string
	num  float64
	b    bool
}

// CompileExpr parses and validates a rule threshold expression.
func CompileExpr(raw string) (Expr, error) {
	tokens, err := tokenizeExpr(raw)
	if err != nil {
		return Expr{}, fmt.Errorf("compile rule expr %q: %w", raw, err)
	}
	if len(tokens) == 1 && tokens[0].kind == tokEOF {
		return Expr{}, errors.New("compile rule expr: empty expression")
	}
	p := exprParser{tokens: tokens}
	root, err := p.parseOr()
	if err != nil {
		return Expr{}, fmt.Errorf("compile rule expr %q: %w", raw, err)
	}
	if p.peek().kind != tokEOF {
		return Expr{}, fmt.Errorf("compile rule expr %q: unexpected token %q", raw, p.peek().text)
	}
	return Expr{raw: raw, root: root}, nil
}

// Eval evaluates the compiled expression against vars and returns a boolean.
func (e Expr) Eval(vars map[string]any) (bool, error) {
	v, err := e.root.eval(vars)
	if err != nil {
		return false, fmt.Errorf("eval rule %q: %w", e.raw, err)
	}
	b, ok := v.(bool)
	if !ok {
		return false, fmt.Errorf("eval rule %q: expected boolean result, got %T", e.raw, v)
	}
	return b, nil
}

func tokenizeExpr(raw string) ([]exprToken, error) {
	var tokens []exprToken
	for i := 0; i < len(raw); {
		if unicode.IsSpace(rune(raw[i])) {
			i++
			continue
		}
		tok, next, err := nextExprToken(raw, i)
		if err != nil {
			return nil, err
		}
		tokens = append(tokens, tok)
		i = next
	}
	tokens = append(tokens, exprToken{kind: tokEOF})
	return tokens, nil
}

func nextExprToken(s string, i int) (exprToken, int, error) {
	if tok, next, ok := scanPunctOrOp(s, i); ok {
		return tok, next, nil
	}
	if s[i] == '"' || s[i] == '\'' {
		return scanStringToken(s, i)
	}
	if isNumberStart(s, i) {
		return scanNumberOrDuration(s, i)
	}
	if isIdentStart(s[i]) {
		return scanIdentOrKeyword(s, i), advanceIdent(s, i), nil
	}
	return exprToken{}, 0, fmt.Errorf("unexpected character %q at offset %d", s[i], i)
}

func scanPunctOrOp(s string, i int) (exprToken, int, bool) {
	if i+1 < len(s) {
		two := s[i : i+2]
		if two == "&&" || two == "||" || two == "==" || two == "!=" || two == "<=" || two == ">=" {
			return exprToken{kind: tokOp, text: two}, i + 2, true
		}
	}
	switch s[i] {
	case '(':
		return exprToken{kind: tokLParen, text: "("}, i + 1, true
	case ')':
		return exprToken{kind: tokRParen, text: ")"}, i + 1, true
	case '!':
		return exprToken{kind: tokBang, text: "!"}, i + 1, true
	case '<', '>':
		return exprToken{kind: tokOp, text: s[i : i+1]}, i + 1, true
	default:
		return exprToken{}, 0, false
	}
}

func scanStringToken(s string, i int) (exprToken, int, error) {
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
			return exprToken{kind: tokString, text: b.String()}, j + 1, nil
		}
		b.WriteByte(c)
	}
	return exprToken{}, 0, errors.New("unterminated string literal")
}

func unescapeChar(c byte) byte {
	switch c {
	case 'n':
		return '\n'
	case 't':
		return '\t'
	case 'r':
		return '\r'
	default:
		return c
	}
}

func isNumberStart(s string, i int) bool {
	if s[i] >= '0' && s[i] <= '9' {
		return true
	}
	return s[i] == '-' && i+1 < len(s) && s[i+1] >= '0' && s[i+1] <= '9'
}

func scanNumberOrDuration(s string, i int) (exprToken, int, error) {
	j := i
	if s[j] == '-' {
		j++
	}
	for j < len(s) && (isAlnum(s[j]) || s[j] == '.') {
		j++
	}
	raw := s[i:j]
	if n, err := strconv.ParseFloat(raw, 64); err == nil {
		return exprToken{kind: tokNumber, text: raw, num: n}, j, nil
	}
	d, err := ParseDuration(raw)
	if err != nil {
		return exprToken{}, 0, fmt.Errorf("invalid number or duration %q", raw)
	}
	return exprToken{kind: tokDuration, text: raw, num: d.Seconds()}, j, nil
}

func isIdentStart(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func isAlnum(c byte) bool {
	return isIdentStart(c) || (c >= '0' && c <= '9')
}

func advanceIdent(s string, i int) int {
	for i < len(s) && isAlnum(s[i]) {
		i++
	}
	return i
}

func scanIdentOrKeyword(s string, i int) exprToken {
	word := s[i:advanceIdent(s, i)]
	switch word {
	case "true":
		return exprToken{kind: tokBool, text: word, b: true}
	case "false":
		return exprToken{kind: tokBool, text: word, b: false}
	case "nil", "null":
		return exprToken{kind: tokNil, text: word}
	case "matches", "contains":
		return exprToken{kind: tokOp, text: word}
	default:
		return exprToken{kind: tokIdent, text: word}
	}
}

type exprParser struct {
	tokens []exprToken
	pos    int
}

func (p *exprParser) peek() exprToken {
	return p.tokens[p.pos]
}

func (p *exprParser) next() exprToken {
	tok := p.tokens[p.pos]
	p.pos++
	return tok
}

func (p *exprParser) parseOr() (exprNode, error) {
	left, err := p.parseAnd()
	if err != nil {
		return nil, err
	}
	for p.peek().kind == tokOp && p.peek().text == "||" {
		op := p.next().text
		right, err := p.parseAnd()
		if err != nil {
			return nil, err
		}
		left = logicNode{op: op, left: left, right: right}
	}
	return left, nil
}

func (p *exprParser) parseAnd() (exprNode, error) {
	left, err := p.parseNot()
	if err != nil {
		return nil, err
	}
	for p.peek().kind == tokOp && p.peek().text == "&&" {
		op := p.next().text
		right, err := p.parseNot()
		if err != nil {
			return nil, err
		}
		left = logicNode{op: op, left: left, right: right}
	}
	return left, nil
}

func (p *exprParser) parseNot() (exprNode, error) {
	if p.peek().kind == tokBang {
		p.next()
		child, err := p.parseNot()
		if err != nil {
			return nil, err
		}
		return notNode{child: child}, nil
	}
	return p.parseCmp()
}

func (p *exprParser) parseCmp() (exprNode, error) {
	left, err := p.parsePrimary()
	if err != nil {
		return nil, err
	}
	tok := p.peek()
	if tok.kind != tokOp || tok.text == "&&" || tok.text == "||" {
		return left, nil
	}
	op := p.next().text
	right, err := p.parsePrimary()
	if err != nil {
		return nil, err
	}
	var re *regexp.Regexp
	if op == "matches" {
		lit, ok := right.(literalNode)
		if !ok {
			return nil, errors.New("matches requires a string literal pattern")
		}
		pat, ok := lit.val.(string)
		if !ok {
			return nil, errors.New("matches requires a string literal pattern")
		}
		re, err = regexp.Compile(pat)
		if err != nil {
			return nil, fmt.Errorf("invalid regex %q: %w", pat, err)
		}
	}
	return cmpNode{op: op, left: left, right: right, re: re}, nil
}

func (p *exprParser) parsePrimary() (exprNode, error) {
	tok := p.next()
	switch tok.kind {
	case tokLParen:
		inner, err := p.parseOr()
		if err != nil {
			return nil, err
		}
		if p.peek().kind != tokRParen {
			return nil, errors.New("expected ')'")
		}
		p.next()
		return inner, nil
	case tokIdent:
		return identNode{name: tok.text}, nil
	case tokNumber:
		return literalNode{val: tok.num}, nil
	case tokDuration:
		return literalNode{val: durationSeconds(tok.num)}, nil
	case tokString:
		return literalNode{val: tok.text}, nil
	case tokBool:
		return literalNode{val: tok.b}, nil
	case tokNil:
		return literalNode{val: nil}, nil
	default:
		return nil, fmt.Errorf("unexpected token %q", tok.text)
	}
}

func compareValues(lhs any, op string, rhs any) (bool, error) {
	if op == "contains" {
		return strings.Contains(fmt.Sprint(lhs), fmt.Sprint(rhs)), nil
	}
	if lhs == nil || rhs == nil {
		return compareNil(lhs, op, rhs)
	}
	if ln, okL := numericValue(lhs, rhs); okL {
		if rn, okR := numericValue(rhs, lhs); okR {
			return compareFloat(ln, op, rn)
		}
	}
	if lb, okL := lhs.(bool); okL {
		if rb, okR := rhs.(bool); okR {
			return compareBool(lb, op, rb)
		}
	}
	if ls, okL := lhs.(string); okL {
		if rs, okR := rhs.(string); okR {
			return compareString(ls, op, rs)
		}
	}
	return false, fmt.Errorf("cannot compare %T and %T with %s", lhs, rhs, op)
}

func compareNil(lhs any, op string, rhs any) (bool, error) {
	switch op {
	case "==":
		return lhs == nil && rhs == nil, nil
	case "!=":
		return (lhs == nil) != (rhs == nil), nil
	default:
		return false, fmt.Errorf("missing value for operator %s", op)
	}
}

func numericValue(v any, peer any) (float64, bool) {
	switch val := v.(type) {
	case durationSeconds:
		return float64(val), true
	case string:
		if _, isDur := peer.(durationSeconds); isDur {
			if d, err := ParseDuration(val); err == nil {
				return d.Seconds(), true
			}
		}
		return 0, false
	default:
		return toFloat(v)
	}
}

func toFloat(v any) (float64, bool) {
	switch n := v.(type) {
	case float64:
		return n, true
	case float32:
		return float64(n), true
	case int:
		return float64(n), true
	case int64:
		return float64(n), true
	case uint64:
		return float64(n), true
	case durationSeconds:
		return float64(n), true
	default:
		return 0, false
	}
}

func compareFloat(a float64, op string, b float64) (bool, error) {
	switch op {
	case "==":
		return a == b, nil
	case "!=":
		return a != b, nil
	case "<":
		return a < b, nil
	case "<=":
		return a <= b, nil
	case ">":
		return a > b, nil
	case ">=":
		return a >= b, nil
	default:
		return false, fmt.Errorf("unsupported numeric operator %q", op)
	}
}

func compareBool(a bool, op string, b bool) (bool, error) {
	switch op {
	case "==":
		return a == b, nil
	case "!=":
		return a != b, nil
	default:
		return false, fmt.Errorf("unsupported bool operator %q", op)
	}
}

func compareString(a, op, b string) (bool, error) {
	switch op {
	case "==":
		return a == b, nil
	case "!=":
		return a != b, nil
	case "<":
		return a < b, nil
	case "<=":
		return a <= b, nil
	case ">":
		return a > b, nil
	case ">=":
		return a >= b, nil
	default:
		return false, fmt.Errorf("unsupported string operator %q", op)
	}
}
