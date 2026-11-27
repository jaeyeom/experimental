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
	filter jpFilterExpr
	agg    string
}

type jpFilterExpr struct {
	field string
	op    string
	rhs   any
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
	if key, ok := unquoteBracketKey(inner); ok {
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
		if c == ']' {
			return i, nil
		}
	}
	return 0, errors.New("unclosed '['")
}

func unquoteBracketKey(inner string) (string, bool) {
	if len(inner) >= 2 && ((inner[0] == '"' && inner[len(inner)-1] == '"') || (inner[0] == '\'' && inner[len(inner)-1] == '\'')) {
		return inner[1 : len(inner)-1], true
	}
	return "", false
}

func parseJPFilter(raw string) (jpFilterExpr, error) {
	s := strings.TrimSpace(raw)
	if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
		s = strings.TrimSpace(s[1 : len(s)-1])
	}
	if !strings.HasPrefix(s, "@.") {
		return jpFilterExpr{}, fmt.Errorf("filter %q lacks @. prefix", raw)
	}
	s = s[2:]
	op, lhs, rhs, err := splitFilterOp(s)
	if err != nil {
		return jpFilterExpr{}, err
	}
	val, err := parseLiteral(strings.TrimSpace(rhs))
	if err != nil {
		return jpFilterExpr{}, fmt.Errorf("filter rhs %q: %w", rhs, err)
	}
	return jpFilterExpr{field: strings.TrimSpace(lhs), op: op, rhs: val}, nil
}

func splitFilterOp(s string) (op, lhs, rhs string, err error) {
	for _, candidate := range []string{"==", "!=", "<=", ">=", "<", ">"} {
		if idx := strings.Index(s, candidate); idx > 0 {
			return candidate, s[:idx], s[idx+len(candidate):], nil
		}
	}
	return "", "", "", fmt.Errorf("missing comparison operator in filter %q", s)
}

func parseLiteral(s string) (any, error) {
	if unq, ok := unquoteBracketKey(s); ok {
		return unq, nil
	}
	switch s {
	case "true":
		return true, nil
	case "false":
		return false, nil
	case "null", "nil":
		return nil, nil
	}
	if n, err := strconv.ParseFloat(s, 64); err == nil {
		return n, nil
	}
	return nil, fmt.Errorf("invalid literal %q", s)
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

func evalJPFilter(cur any, f jpFilterExpr) (any, bool, error) {
	arr, ok := cur.([]any)
	if !ok {
		return nil, false, fmt.Errorf("filter requires array, got %T", cur)
	}
	var out []any
	for _, item := range arr {
		lhs := lookupFilterField(item, f.field)
		matched, err := compareValues(lhs, f.op, f.rhs)
		if err != nil {
			continue
		}
		if matched {
			out = append(out, item)
		}
	}
	if out == nil {
		out = []any{}
	}
	return out, true, nil
}

func lookupFilterField(item any, path string) any {
	cur := item
	for _, seg := range strings.Split(path, ".") {
		obj, ok := cur.(map[string]any)
		if !ok {
			return nil
		}
		val, exists := obj[seg]
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
