package ghcli

import (
	"bytes"
	"testing"
)

func TestParseEnvelopeDataPayload(t *testing.T) {
	raw := `{"data":{"repository":{"name":"experimental"}}}`
	data, messages, err := ParseEnvelope(raw)
	if err != nil {
		t.Fatalf("ParseEnvelope() error = %v", err)
	}
	if len(messages) != 0 {
		t.Fatalf("messages = %#v, want none", messages)
	}
	want := []byte(`{"repository":{"name":"experimental"}}`)
	if !bytes.Equal(data, want) {
		t.Fatalf("data = %s, want %s", data, want)
	}
}

func TestParseEnvelopeErrorsPayload(t *testing.T) {
	raw := `{"errors":[{"message":" one "},{"message":"two"}]}`
	data, messages, err := ParseEnvelope(raw)
	if err != nil {
		t.Fatalf("ParseEnvelope() error = %v", err)
	}
	if data != nil {
		t.Fatalf("data = %s, want nil", data)
	}
	if len(messages) != 2 || messages[0] != "one" || messages[1] != "two" {
		t.Fatalf("messages = %#v", messages)
	}
}

func TestParseEnvelopeInvalidJSON(t *testing.T) {
	_, _, err := ParseEnvelope("{")
	if err == nil {
		t.Fatal("ParseEnvelope() error = nil, want parse error")
	}
}
