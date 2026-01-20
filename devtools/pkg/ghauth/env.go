package ghauth

import (
	"context"
	"fmt"
	"os"
)

// EnvTokenSource retrieves GitHub tokens from environment variables.
type EnvTokenSource struct {
	varName string
}

// NewEnvTokenSource creates a token source that reads from the specified environment variable.
func NewEnvTokenSource(varName string) *EnvTokenSource {
	return &EnvTokenSource{varName: varName}
}

// Token retrieves the token from the environment variable.
func (e *EnvTokenSource) Token(_ context.Context) (string, error) {
	token := os.Getenv(e.varName)
	if token == "" {
		return "", fmt.Errorf("environment variable %s is not set or empty", e.varName)
	}
	return token, nil
}

// VarName returns the environment variable name this source reads from.
func (e *EnvTokenSource) VarName() string {
	return e.varName
}

// MultiEnvTokenSource tries multiple environment variables in order.
type MultiEnvTokenSource struct {
	varNames []string
}

// NewMultiEnvTokenSource creates a token source that tries multiple environment variables.
func NewMultiEnvTokenSource(varNames ...string) *MultiEnvTokenSource {
	return &MultiEnvTokenSource{varNames: varNames}
}

// Token retrieves the token from the first set environment variable.
func (m *MultiEnvTokenSource) Token(_ context.Context) (string, error) {
	for _, varName := range m.varNames {
		token := os.Getenv(varName)
		if token != "" {
			return token, nil
		}
	}
	return "", fmt.Errorf("none of the environment variables are set: %v", m.varNames)
}
