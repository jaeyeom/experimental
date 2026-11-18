package authwatch

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestParseCredentialsMetadata(t *testing.T) {
	const secretAccess = "sk-ant-oat01-SUPERSECRET-ACCESS"   //nolint:gosec // G101: test fixture
	const secretRefresh = "sk-ant-ort01-SUPERSECRET-REFRESH" //nolint:gosec // G101: test fixture

	t.Run("valid with extra fields ignored", func(t *testing.T) {
		payload := `{
			"claudeAiOauth": {
				"accessToken": "` + secretAccess + `",
				"refreshToken": "` + secretRefresh + `",
				"expiresAt": 1790000000000,
				"refreshTokenExpiresAt": 1792000000000,
				"scopes": ["user:inference"],
				"subscriptionType": "max"
			},
			"extraTopLevel": "ignored"
		}`
		exp, err := ParseCredentialsMetadata([]byte(payload))
		if err != nil {
			t.Fatal(err)
		}
		if !exp.AccessExpiresAt.Equal(time.UnixMilli(1790000000000).UTC()) {
			t.Errorf("AccessExpiresAt = %v", exp.AccessExpiresAt)
		}
		if !exp.RefreshExpiresAt.Equal(time.UnixMilli(1792000000000).UTC()) {
			t.Errorf("RefreshExpiresAt = %v", exp.RefreshExpiresAt)
		}
	})

	t.Run("missing fields and changed types never leak values", func(t *testing.T) {
		cases := []struct {
			name      string
			payload   string
			wantField string
		}{
			{
				name:      "missing claudeAiOauth",
				payload:   `{"other": "secret-val"}`,
				wantField: "claudeAiOauth",
			},
			{
				name:      "missing expiresAt",
				payload:   `{"claudeAiOauth": {"refreshTokenExpiresAt": 1792000000000}}`,
				wantField: "claudeAiOauth.expiresAt",
			},
			{
				name:      "missing refreshTokenExpiresAt",
				payload:   `{"claudeAiOauth": {"expiresAt": 1790000000000}}`,
				wantField: "claudeAiOauth.refreshTokenExpiresAt",
			},
			{
				name:      "changed type expiresAt string",
				payload:   `{"claudeAiOauth": {"expiresAt": "secret-string-val", "refreshTokenExpiresAt": 1792000000000}}`,
				wantField: "claudeAiOauth.expiresAt",
			},
			{
				name:      "changed type refreshTokenExpiresAt bool",
				payload:   `{"claudeAiOauth": {"expiresAt": 1790000000000, "refreshTokenExpiresAt": true}}`,
				wantField: "claudeAiOauth.refreshTokenExpiresAt",
			},
		}
		for _, tc := range cases {
			t.Run(tc.name, func(t *testing.T) {
				_, err := ParseCredentialsMetadata([]byte(tc.payload))
				if err == nil {
					t.Fatal("expected FormatError")
				}
				var fmtErr *FormatError
				if !errors.As(err, &fmtErr) {
					t.Fatalf("expected *FormatError, got %T: %v", err, err)
				}
				if !strings.Contains(err.Error(), tc.wantField) {
					t.Errorf("error %q does not name field %q", err.Error(), tc.wantField)
				}
				if strings.Contains(err.Error(), "secret") {
					t.Errorf("error %q leaked value", err.Error())
				}
			})
		}
	})
}

func TestDefaultStoreReaderLinuxAndDarwin(t *testing.T) {
	tmp := t.TempDir()
	env := []string{"HOME=" + tmp}

	t.Run("linux missing then present", func(t *testing.T) {
		reader := DefaultStoreReader("linux", nil)
		prof := ProfileConfig{Name: "default"}
		if _, err := reader(context.Background(), prof, env); !errors.Is(err, ErrStoreNotFound) {
			t.Fatalf("expected ErrStoreNotFound, got %v", err)
		}
		cfgDir := filepath.Join(tmp, ".claude")
		if err := os.MkdirAll(cfgDir, 0o700); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(cfgDir, ".credentials.json"), []byte(`{}`), 0o600); err != nil {
			t.Fatal(err)
		}
		got, err := reader(context.Background(), prof, env)
		if err != nil {
			t.Fatal(err)
		}
		if string(got) != `{}` {
			t.Errorf("got %q", got)
		}
	})

	t.Run("darwin fake keychain command", func(t *testing.T) {
		var gotArgs []string
		fakeCmd := func(_ context.Context, name string, args, _ []string) ([]byte, int, error) {
			if name != "security" {
				t.Fatalf("unexpected binary %q", name)
			}
			gotArgs = args
			return []byte(`{"claudeAiOauth":{"expiresAt":1,"refreshTokenExpiresAt":2}}`), 0, nil
		}
		reader := DefaultStoreReader("darwin", fakeCmd)
		got, err := reader(context.Background(), ProfileConfig{Name: "default"}, env)
		if err != nil {
			t.Fatal(err)
		}
		if len(gotArgs) != 4 || gotArgs[0] != "find-generic-password" || gotArgs[1] != "-w" || gotArgs[2] != "-s" || gotArgs[3] != DefaultKeychainService {
			t.Errorf("unexpected security args: %v", gotArgs)
		}
		if !strings.Contains(string(got), "claudeAiOauth") {
			t.Errorf("unexpected output: %q", got)
		}

		missingCmd := func(_ context.Context, _ string, _, _ []string) ([]byte, int, error) {
			return nil, 44, errors.New("exit status 44")
		}
		if _, err := DefaultStoreReader("darwin", missingCmd)(context.Background(), ProfileConfig{Name: "default"}, env); !errors.Is(err, ErrStoreNotFound) {
			t.Fatalf("expected ErrStoreNotFound on exit 44, got %v", err)
		}
	})
}
