package ghauth

import (
	"context"
	"errors"
	"testing"
)

// mockTokenSource is a simple mock for testing.
type mockTokenSource struct {
	token string
	err   error
}

func (m *mockTokenSource) Token(_ context.Context) (string, error) {
	return m.token, m.err
}

func TestChainTokenSource(t *testing.T) {
	tests := []struct {
		name      string
		sources   []TokenSource
		wantToken string
		wantErr   bool
	}{
		{
			name: "first source succeeds",
			sources: []TokenSource{
				&mockTokenSource{token: "token1"},
				&mockTokenSource{token: "token2"},
			},
			wantToken: "token1",
			wantErr:   false,
		},
		{
			name: "first fails, second succeeds",
			sources: []TokenSource{
				&mockTokenSource{err: errors.New("failed")},
				&mockTokenSource{token: "token2"},
			},
			wantToken: "token2",
			wantErr:   false,
		},
		{
			name: "all fail",
			sources: []TokenSource{
				&mockTokenSource{err: errors.New("failed1")},
				&mockTokenSource{err: errors.New("failed2")},
			},
			wantToken: "",
			wantErr:   true,
		},
		{
			name: "empty token treated as failure",
			sources: []TokenSource{
				&mockTokenSource{token: ""},
				&mockTokenSource{token: "valid"},
			},
			wantToken: "valid",
			wantErr:   false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			chain := NewChainTokenSource(tt.sources...)
			token, err := chain.Token(context.Background())

			if (err != nil) != tt.wantErr {
				t.Errorf("Token() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if token != tt.wantToken {
				t.Errorf("Token() = %q, want %q", token, tt.wantToken)
			}
		})
	}
}

func TestErrNoToken(t *testing.T) {
	// Test with no sources
	chain := NewChainTokenSource()
	_, err := chain.Token(context.Background())

	if !errors.Is(err, ErrNoToken) {
		t.Errorf("expected ErrNoToken, got %v", err)
	}
}
