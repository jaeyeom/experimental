// Package ghcli parses the GitHub GraphQL JSON envelope.
package ghcli

import (
	"encoding/json"
	"fmt"
	"strings"
)

// ParseEnvelope unmarshals a GraphQL JSON envelope of the form
// {data, errors[].message}.
//
// When errors is empty and data is present, data is that JSON value and
// messages is nil. When errors is non-empty, messages are the trimmed
// errors[].message strings. A payload that is not a JSON object of that
// shape returns an error. ParseEnvelope does not classify messages, call
// gh, or retry.
func ParseEnvelope(raw string) (json.RawMessage, []string, error) {
	trimmed := strings.TrimSpace(raw)
	if trimmed == "" || trimmed[0] != '{' {
		return nil, nil, fmt.Errorf("parse graphql envelope: not a JSON object")
	}
	var envelope struct {
		Data   *json.RawMessage `json:"data"`
		Errors []struct {
			Message string `json:"message"`
		} `json:"errors"`
	}
	if err := json.Unmarshal([]byte(raw), &envelope); err != nil {
		return nil, nil, fmt.Errorf("parse graphql envelope: %w", err)
	}
	if len(envelope.Errors) > 0 {
		messages := make([]string, len(envelope.Errors))
		for i, item := range envelope.Errors {
			messages[i] = strings.TrimSpace(item.Message)
		}
		if envelope.Data == nil {
			return nil, messages, nil
		}
		return *envelope.Data, messages, nil
	}
	if envelope.Data == nil {
		return nil, nil, nil
	}
	return *envelope.Data, nil, nil
}
