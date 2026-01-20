package ghauth

import (
	"context"
	"os"
	"testing"
)

func TestEnvTokenSource(t *testing.T) {
	// Set a test environment variable
	testVar := "TEST_GH_TOKEN_12345"
	testToken := "test_token_value" //nolint:gosec // This is a test token, not a real credential
	os.Setenv(testVar, testToken)
	defer os.Unsetenv(testVar)

	source := NewEnvTokenSource(testVar)
	token, err := source.Token(context.Background())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if token != testToken {
		t.Errorf("expected %q, got %q", testToken, token)
	}
}

func TestEnvTokenSourceNotSet(t *testing.T) {
	source := NewEnvTokenSource("NONEXISTENT_VAR_12345")
	_, err := source.Token(context.Background())
	if err == nil {
		t.Fatal("expected error for unset variable")
	}
}

func TestEnvTokenSourceVarName(t *testing.T) {
	source := NewEnvTokenSource("MY_VAR")
	if source.VarName() != "MY_VAR" {
		t.Errorf("expected 'MY_VAR', got %q", source.VarName())
	}
}

func TestMultiEnvTokenSource(t *testing.T) {
	// Set only the second variable
	os.Setenv("TEST_VAR_2", "token_from_var_2")
	defer os.Unsetenv("TEST_VAR_2")

	source := NewMultiEnvTokenSource("TEST_VAR_1", "TEST_VAR_2", "TEST_VAR_3")
	token, err := source.Token(context.Background())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if token != "token_from_var_2" { //nolint:gosec // This is a test token, not a real credential
		t.Errorf("expected 'token_from_var_2', got %q", token)
	}
}

func TestMultiEnvTokenSourceNoneSet(t *testing.T) {
	source := NewMultiEnvTokenSource("NONEXISTENT_1", "NONEXISTENT_2")
	_, err := source.Token(context.Background())
	if err == nil {
		t.Fatal("expected error when no variables are set")
	}
}
