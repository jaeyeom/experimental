package catalog

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"math/big"
	"regexp"
	"unicode/utf8"
)

const maxArgsStringLength = 256

type schemaNode struct {
	Type                 string                 `json:"type"`
	Description          string                 `json:"description"`
	Properties           map[string]*schemaNode `json:"properties"`
	Required             []string               `json:"required"`
	AdditionalProperties *bool                  `json:"additionalProperties"`
	Items                *schemaNode            `json:"items"`
	MaxItems             *int                   `json:"maxItems"`
	MinItems             *int                   `json:"minItems"`
	MaxLength            *int                   `json:"maxLength"`
	MinLength            *int                   `json:"minLength"`
	Pattern              *string                `json:"pattern"`
	Enum                 []any                  `json:"enum"`
	Minimum              *json.Number           `json:"minimum"`
	Maximum              *json.Number           `json:"maximum"`
}

// ValidateArgs validates raw JSON arguments against the skill's strict
// ArgsSchema and returns canonicalized compact JSON bytes.
func (s Skill) ValidateArgs(raw json.RawMessage) ([]byte, error) {
	schema, err := validateArgsSchema(s.ArgsSchema)
	if err != nil {
		return nil, fmt.Errorf("invalid argsSchema: %w", err)
	}
	val, err := decodeStrictValue(raw)
	if err != nil {
		return nil, fmt.Errorf("decode args: %w", err)
	}
	if err := validateValue("$", schema, val); err != nil {
		return nil, err
	}
	canonical, err := json.Marshal(val)
	if err != nil {
		return nil, fmt.Errorf("marshal canonical args: %w", err)
	}
	return canonical, nil
}

// ValidateOutput checks that raw does not exceed the skill's
// MaxDeliverableBytes limit and validates it against OutputSchema.
func (s Skill) ValidateOutput(raw json.RawMessage) error {
	maxBytes := s.Limits.MaxDeliverableBytes
	if maxBytes <= 0 {
		maxBytes = DefaultMaxDeliverableBytes
	}
	if len(raw) > maxBytes {
		return fmt.Errorf("deliverable size %d exceeds maxDeliverableBytes %d", len(raw), maxBytes)
	}
	schema, err := validateOutputSchema(s.OutputSchema)
	if err != nil {
		return fmt.Errorf("invalid outputSchema: %w", err)
	}
	val, err := decodeStrictValue(raw)
	if err != nil {
		return fmt.Errorf("decode output: %w", err)
	}
	return validateValue("$", schema, val)
}

func decodeStrictSchema(raw json.RawMessage) (*schemaNode, error) {
	trimmed := bytes.TrimSpace(raw)
	if len(trimmed) == 0 || bytes.Equal(trimmed, []byte("null")) {
		return nil, errors.New("schema must be a JSON object")
	}
	dec := json.NewDecoder(bytes.NewReader(trimmed))
	dec.DisallowUnknownFields()
	dec.UseNumber()

	var node schemaNode
	if err := dec.Decode(&node); err != nil {
		return nil, fmt.Errorf("decode schema: %w", err)
	}
	if err := ensureEOF(dec); err != nil {
		return nil, fmt.Errorf("schema trailing data: %w", err)
	}
	return &node, nil
}

func decodeStrictValue(raw json.RawMessage) (any, error) {
	trimmed := bytes.TrimSpace(raw)
	if len(trimmed) == 0 {
		return nil, errors.New("empty JSON payload")
	}
	dec := json.NewDecoder(bytes.NewReader(trimmed))
	dec.UseNumber()

	var val any
	if err := dec.Decode(&val); err != nil {
		return nil, fmt.Errorf("decode JSON value: %w", err)
	}
	if err := ensureEOF(dec); err != nil {
		return nil, fmt.Errorf("trailing JSON data: %w", err)
	}
	return val, nil
}

func validateArgsSchema(raw json.RawMessage) (*schemaNode, error) {
	root, err := decodeStrictSchema(raw)
	if err != nil {
		return nil, err
	}
	if root.Type != "object" {
		return nil, fmt.Errorf("root type must be %q, got %q", "object", root.Type)
	}
	if root.AdditionalProperties == nil || *root.AdditionalProperties {
		return nil, errors.New("root must declare additionalProperties: false")
	}
	if root.Properties == nil {
		return nil, errors.New("root must declare properties")
	}
	if err := rejectNonObjectKeywords("$", root); err != nil {
		return nil, err
	}
	if err := validateRequiredNames(root); err != nil {
		return nil, err
	}
	for name, prop := range root.Properties {
		if err := validateArgsProperty(name, prop); err != nil {
			return nil, err
		}
	}
	return root, nil
}

func rejectNonObjectKeywords(path string, node *schemaNode) error {
	if node.Items != nil || node.MinItems != nil || node.MaxItems != nil {
		return fmt.Errorf("%s: array keywords not allowed on object", path)
	}
	if node.MinLength != nil || node.MaxLength != nil || node.Pattern != nil {
		return fmt.Errorf("%s: string keywords not allowed on object", path)
	}
	if node.Minimum != nil || node.Maximum != nil || len(node.Enum) > 0 {
		return fmt.Errorf("%s: scalar keywords not allowed on object", path)
	}
	return nil
}

func validateRequiredNames(node *schemaNode) error {
	for _, req := range node.Required {
		if _, ok := node.Properties[req]; !ok {
			return fmt.Errorf("required property %q is not defined in properties", req)
		}
	}
	return nil
}

func validateArgsProperty(name string, prop *schemaNode) error {
	if prop == nil {
		return fmt.Errorf("property %q must not be null", name)
	}
	if prop.Properties != nil || prop.Required != nil || prop.AdditionalProperties != nil {
		return fmt.Errorf("property %q: object keywords not allowed on scalar", name)
	}
	if prop.Items != nil || prop.MinItems != nil || prop.MaxItems != nil {
		return fmt.Errorf("property %q: array keywords not allowed on scalar", name)
	}
	switch prop.Type {
	case "string":
		return validateArgsStringProp(name, prop)
	case "integer", "number":
		return validateArgsNumberProp(name, prop)
	case "boolean":
		return validateArgsBoolProp(name, prop)
	default:
		return fmt.Errorf("property %q: unsupported argsSchema type %q", name, prop.Type)
	}
}

func validateArgsStringProp(name string, prop *schemaNode) error {
	if prop.Minimum != nil || prop.Maximum != nil {
		return fmt.Errorf("property %q: numeric bounds not allowed on string", name)
	}
	if err := validateStringConstraints(name, prop); err != nil {
		return err
	}
	if len(prop.Enum) > 0 {
		if err := validateEnumTypes(name, "string", prop.Enum); err != nil {
			return err
		}
		return nil
	}
	if prop.Pattern == nil || *prop.Pattern == "" {
		return fmt.Errorf("property %q: string must declare enum or pattern with maxLength <= %d", name, maxArgsStringLength)
	}
	if prop.MaxLength == nil || *prop.MaxLength <= 0 || *prop.MaxLength > maxArgsStringLength {
		return fmt.Errorf("property %q: string pattern requires maxLength between 1 and %d", name, maxArgsStringLength)
	}
	return nil
}

func validateArgsNumberProp(name string, prop *schemaNode) error {
	if prop.MinLength != nil || prop.MaxLength != nil || prop.Pattern != nil {
		return fmt.Errorf("property %q: string keywords not allowed on %s", name, prop.Type)
	}
	if err := validateNumberConstraints(name, prop); err != nil {
		return err
	}
	if len(prop.Enum) > 0 {
		return validateEnumTypes(name, prop.Type, prop.Enum)
	}
	return nil
}

func validateArgsBoolProp(name string, prop *schemaNode) error {
	if prop.MinLength != nil || prop.MaxLength != nil || prop.Pattern != nil || prop.Minimum != nil || prop.Maximum != nil {
		return fmt.Errorf("property %q: string/numeric keywords not allowed on boolean", name)
	}
	if len(prop.Enum) > 0 {
		return validateEnumTypes(name, "boolean", prop.Enum)
	}
	return nil
}

func validateOutputSchema(raw json.RawMessage) (*schemaNode, error) {
	root, err := decodeStrictSchema(raw)
	if err != nil {
		return nil, err
	}
	if root.Type != "object" {
		return nil, fmt.Errorf("outputSchema root type must be %q, got %q", "object", root.Type)
	}
	if err := validateOutputNode("$", root); err != nil {
		return nil, err
	}
	return root, nil
}

func validateOutputNode(path string, node *schemaNode) error {
	if node == nil {
		return fmt.Errorf("%s: schema node must not be null", path)
	}
	switch node.Type {
	case "object":
		return validateOutputObjectNode(path, node)
	case "array":
		return validateOutputArrayNode(path, node)
	case "string":
		return validateStringConstraints(path, node)
	case "integer", "number":
		return validateNumberConstraints(path, node)
	case "boolean", "null":
		return nil
	default:
		return fmt.Errorf("%s: unsupported schema type %q", path, node.Type)
	}
}

func validateOutputObjectNode(path string, node *schemaNode) error {
	for propName, child := range node.Properties {
		if err := validateOutputNode(path+"."+propName, child); err != nil {
			return err
		}
	}
	return nil
}

func validateOutputArrayNode(path string, node *schemaNode) error {
	if node.MinItems != nil && *node.MinItems < 0 {
		return fmt.Errorf("%s: minItems must be non-negative", path)
	}
	if node.MaxItems != nil && *node.MaxItems < 0 {
		return fmt.Errorf("%s: maxItems must be non-negative", path)
	}
	if node.MinItems != nil && node.MaxItems != nil && *node.MinItems > *node.MaxItems {
		return fmt.Errorf("%s: minItems exceeds maxItems", path)
	}
	if node.Items != nil {
		return validateOutputNode(path+"[]", node.Items)
	}
	return nil
}

func validateStringConstraints(path string, node *schemaNode) error {
	if node.MinLength != nil && *node.MinLength < 0 {
		return fmt.Errorf("%s: minLength must be non-negative", path)
	}
	if node.MaxLength != nil && *node.MaxLength < 0 {
		return fmt.Errorf("%s: maxLength must be non-negative", path)
	}
	if node.MinLength != nil && node.MaxLength != nil && *node.MinLength > *node.MaxLength {
		return fmt.Errorf("%s: minLength exceeds maxLength", path)
	}
	if node.Pattern != nil {
		if _, err := regexp.Compile(*node.Pattern); err != nil {
			return fmt.Errorf("%s: invalid regex pattern %q: %w", path, *node.Pattern, err)
		}
	}
	return nil
}

func validateNumberConstraints(path string, node *schemaNode) error {
	var minRat, maxRat *big.Rat
	if node.Minimum != nil {
		r, ok := parseNumberRat(*node.Minimum)
		if !ok {
			return fmt.Errorf("%s: invalid minimum %q", path, *node.Minimum)
		}
		minRat = r
	}
	if node.Maximum != nil {
		r, ok := parseNumberRat(*node.Maximum)
		if !ok {
			return fmt.Errorf("%s: invalid maximum %q", path, *node.Maximum)
		}
		maxRat = r
	}
	if minRat != nil && maxRat != nil && minRat.Cmp(maxRat) > 0 {
		return fmt.Errorf("%s: minimum exceeds maximum", path)
	}
	return nil
}

func validateEnumTypes(path, expectedType string, values []any) error {
	for i, v := range values {
		if err := checkScalarType(fmt.Sprintf("%s.enum[%d]", path, i), expectedType, v); err != nil {
			return err
		}
	}
	return nil
}

func checkScalarType(path, expectedType string, v any) error {
	switch expectedType {
	case "string":
		if _, ok := v.(string); !ok {
			return fmt.Errorf("%s: expected string", path)
		}
	case "boolean":
		if _, ok := v.(bool); !ok {
			return fmt.Errorf("%s: expected boolean", path)
		}
	case "integer":
		if _, ok := parseExactInt(v); !ok {
			return fmt.Errorf("%s: expected integer", path)
		}
	case "number":
		if _, ok := parseAnyNumber(v); !ok {
			return fmt.Errorf("%s: expected number", path)
		}
	}
	return nil
}

func validateValue(path string, node *schemaNode, val any) error {
	switch node.Type {
	case "object":
		return validateObjectValue(path, node, val)
	case "array":
		return validateArrayValue(path, node, val)
	case "string":
		return validateStringValue(path, node, val)
	case "integer":
		return validateNumberValue(path, node, val, true)
	case "number":
		return validateNumberValue(path, node, val, false)
	case "boolean":
		return validateBoolValue(path, node, val)
	case "null":
		return validateNullValue(path, val)
	default:
		return fmt.Errorf("%s: unsupported schema type %q", path, node.Type)
	}
}

func validateObjectValue(path string, node *schemaNode, val any) error {
	obj, ok := val.(map[string]any)
	if !ok {
		return fmt.Errorf("%s: expected object", path)
	}
	for _, req := range node.Required {
		if _, exists := obj[req]; !exists {
			return fmt.Errorf("%s: missing required property %q", path, req)
		}
	}
	disallowAdditional := node.AdditionalProperties != nil && !*node.AdditionalProperties
	for key, childVal := range obj {
		propSchema, defined := node.Properties[key]
		if !defined {
			if disallowAdditional {
				return fmt.Errorf("%s: unknown property %q", path, key)
			}
			continue
		}
		if err := validateValue(path+"."+key, propSchema, childVal); err != nil {
			return err
		}
	}
	return nil
}

func validateArrayValue(path string, node *schemaNode, val any) error {
	arr, ok := val.([]any)
	if !ok {
		return fmt.Errorf("%s: expected array", path)
	}
	if node.MinItems != nil && len(arr) < *node.MinItems {
		return fmt.Errorf("%s: array length %d below minItems %d", path, len(arr), *node.MinItems)
	}
	if node.MaxItems != nil && len(arr) > *node.MaxItems {
		return fmt.Errorf("%s: array length %d exceeds maxItems %d", path, len(arr), *node.MaxItems)
	}
	if node.Items != nil {
		for i, elem := range arr {
			if err := validateValue(fmt.Sprintf("%s[%d]", path, i), node.Items, elem); err != nil {
				return err
			}
		}
	}
	return nil
}

func validateStringValue(path string, node *schemaNode, val any) error {
	s, ok := val.(string)
	if !ok {
		return fmt.Errorf("%s: expected string", path)
	}
	length := utf8.RuneCountInString(s)
	if node.MinLength != nil && length < *node.MinLength {
		return fmt.Errorf("%s: string length %d below minLength %d", path, length, *node.MinLength)
	}
	if node.MaxLength != nil && length > *node.MaxLength {
		return fmt.Errorf("%s: string length %d exceeds maxLength %d", path, length, *node.MaxLength)
	}
	if node.Pattern != nil {
		re, err := regexp.Compile(*node.Pattern)
		if err != nil {
			return fmt.Errorf("%s: compile pattern: %w", path, err)
		}
		if !re.MatchString(s) {
			return fmt.Errorf("%s: string %q does not match pattern %q", path, s, *node.Pattern)
		}
	}
	return validateEnumValue(path, node.Enum, s)
}

func validateNumberValue(path string, node *schemaNode, val any, requireInt bool) error {
	var r *big.Rat
	var ok bool
	if requireInt {
		r, ok = parseExactInt(val)
		if !ok {
			return fmt.Errorf("%s: expected integer", path)
		}
	} else {
		r, ok = parseAnyNumber(val)
		if !ok {
			return fmt.Errorf("%s: expected number", path)
		}
	}
	if node.Minimum != nil {
		minRat, _ := parseNumberRat(*node.Minimum)
		if r.Cmp(minRat) < 0 {
			return fmt.Errorf("%s: value %v below minimum %s", path, val, *node.Minimum)
		}
	}
	if node.Maximum != nil {
		maxRat, _ := parseNumberRat(*node.Maximum)
		if r.Cmp(maxRat) > 0 {
			return fmt.Errorf("%s: value %v exceeds maximum %s", path, val, *node.Maximum)
		}
	}
	return validateEnumValue(path, node.Enum, val)
}

func validateBoolValue(path string, node *schemaNode, val any) error {
	b, ok := val.(bool)
	if !ok {
		return fmt.Errorf("%s: expected boolean", path)
	}
	return validateEnumValue(path, node.Enum, b)
}

func validateNullValue(path string, val any) error {
	if val != nil {
		return fmt.Errorf("%s: expected null", path)
	}
	return nil
}

func validateEnumValue(path string, allowed []any, val any) error {
	if len(allowed) == 0 {
		return nil
	}
	for _, candidate := range allowed {
		if valuesEqual(candidate, val) {
			return nil
		}
	}
	return fmt.Errorf("%s: value %v is not in allowed enum", path, val)
}

func valuesEqual(a, b any) bool {
	if ra, ok := parseAnyNumber(a); ok {
		rb, okB := parseAnyNumber(b)
		return okB && ra.Cmp(rb) == 0
	}
	return a == b
}

func parseExactInt(v any) (*big.Rat, bool) {
	r, ok := parseAnyNumber(v)
	if !ok || !r.IsInt() {
		return nil, false
	}
	return r, true
}

func parseAnyNumber(v any) (*big.Rat, bool) {
	num, ok := v.(json.Number)
	if !ok {
		return nil, false
	}
	return parseNumberRat(num)
}

func parseNumberRat(num json.Number) (*big.Rat, bool) {
	r, ok := new(big.Rat).SetString(num.String())
	return r, ok
}
