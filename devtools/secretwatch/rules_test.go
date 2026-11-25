package secretwatch

import (
	"strings"
	"testing"
)

func TestDefaultRulesPositiveAndNearMiss(t *testing.T) {
	rs, err := CompileRuleSet(Config{})
	if err != nil {
		t.Fatal(err)
	}

	// Build synthetic secrets dynamically so static secret scanners do not flag test files.
	ghpTok := "ghp_" + strings.Repeat("a1B2", 9)
	ghoTok := "gho_" + strings.Repeat("c3D4", 9)
	ghuTok := "ghu_" + strings.Repeat("e5F6", 9)
	ghsTok := "ghs_" + strings.Repeat("g7H8", 9)
	ghrTok := "ghr_" + strings.Repeat("i9J0", 9)
	ghPatTok := "github_pat_" + strings.Repeat("k1L2m3N4", 5)

	bearerTok := strings.Join([]string{
		"eyJhbGciOiJIUzI1NiJ9.",
		"eyJzdWIiOiIxMjM0NTY3ODkwIn0.",
		"SflKxwRJSMeKKF2Q",
		"T4fwpMeJf36POk6yJV",
	}, "")
	basicTok := "dXNlcjpzZWNyZXQtdG9rZW4="
	awsKey := "AKIA" + "IOSFODNN7" + "EXMPL12"
	awsTempKey := "ASIA" + "1234567890" + "ABCDEF"
	gcpAPIKey := "AIza" + "SyDa1b2C3d4E5f6G" + "7h8I9j0K1l2M3n4O5p6"
	gcpPrivKey := "-----BEGIN " + "PRIVATE KEY-----\\n" + strings.Repeat("MIIEvQIBADAN", 4)
	pemRSA := "-----BEGIN " + "RSA PRIVATE KEY-----"
	pemOpenSSH := "-----BEGIN " + "OPENSSH PRIVATE KEY-----"
	pemPGP := "-----BEGIN " + "PGP PRIVATE KEY BLOCK-----"
	slackBot := "xoxb-" + "123456789012-" + "AbCdEfGhIjKlMnOp"
	slackUser := "xoxp-" + "987654321098-" + "QrStUvWxYz123456"
	genericSecret := strings.Join([]string{"Kz9mPq2x", "Lw8nRv5t", "Yz3aBc7d", "Ef4gHj6k"}, "")

	tests := []struct {
		name         string
		message      string
		wantMatch    bool
		wantRule     string
		wantPrefix   string
		secretSample string
	}{
		// GitHub tokens (all 6 prefixes + near misses)
		{
			name:         "github ghp",
			message:      "calling api with " + ghpTok,
			wantMatch:    true,
			wantRule:     "github-token",
			wantPrefix:   "ghp_****",
			secretSample: ghpTok,
		},
		{
			name:         "github gho",
			message:      "token=" + ghoTok,
			wantMatch:    true,
			wantRule:     "github-token",
			wantPrefix:   "gho_****",
			secretSample: ghoTok,
		},
		{
			name:         "github ghu",
			message:      "token=" + ghuTok,
			wantMatch:    true,
			wantRule:     "github-token",
			wantPrefix:   "ghu_****",
			secretSample: ghuTok,
		},
		{
			name:         "github ghs",
			message:      "token=" + ghsTok,
			wantMatch:    true,
			wantRule:     "github-token",
			wantPrefix:   "ghs_****",
			secretSample: ghsTok,
		},
		{
			name:         "github ghr",
			message:      "token=" + ghrTok,
			wantMatch:    true,
			wantRule:     "github-token",
			wantPrefix:   "ghr_****",
			secretSample: ghrTok,
		},
		{
			name:         "github fine-grained pat",
			message:      "GITHUB_TOKEN=" + ghPatTok,
			wantMatch:    true,
			wantRule:     "github-token",
			wantPrefix:   "gith****",
			secretSample: ghPatTok,
		},
		{
			name:      "github near miss short",
			message:   "doc says replace ghp_shortplaceholder",
			wantMatch: false,
		},
		{
			name:      "github near miss wrong prefix",
			message:   "value=ghx_" + strings.Repeat("a1B2", 9),
			wantMatch: false,
		},
		// Authorization / Bearer headers
		{
			name:         "authorization bearer",
			message:      "POST /v1/run Authorization: Bearer " + bearerTok,
			wantMatch:    true,
			wantRule:     "auth-header",
			wantPrefix:   "eyJh****",
			secretSample: bearerTok,
		},
		{
			name:         "authorization basic",
			message:      "Authorization: Basic " + basicTok,
			wantMatch:    true,
			wantRule:     "auth-header",
			wantPrefix:   "dXNl****",
			secretSample: basicTok,
		},
		{
			name:         "standalone bearer",
			message:      `header="Bearer ` + bearerTok + `"`,
			wantMatch:    true,
			wantRule:     "auth-header",
			wantPrefix:   "eyJh****",
			secretSample: bearerTok,
		},
		{
			name:      "auth header near miss redacted",
			message:   "Authorization: Bearer [REDACTED]",
			wantMatch: false,
		},
		{
			name:      "auth header near miss unauthorized",
			message:   "Authorization: unauthorized",
			wantMatch: false,
		},
		{
			name:      "bearer near miss english prose",
			message:   "the bearer of responsibility in this service",
			wantMatch: false,
		},
		// AWS access keys
		{
			name:         "aws access key AKIA",
			message:      "using key " + awsKey,
			wantMatch:    true,
			wantRule:     "aws-access-key",
			wantPrefix:   "AKIA****",
			secretSample: awsKey,
		},
		{
			name:         "aws access key ASIA",
			message:      "session key " + awsTempKey,
			wantMatch:    true,
			wantRule:     "aws-access-key",
			wantPrefix:   "ASIA****",
			secretSample: awsTempKey,
		},
		{
			name:      "aws near miss short",
			message:   "AKIA12345 is not a full key",
			wantMatch: false,
		},
		{
			name:      "aws near miss lowercase",
			message:   strings.ToLower(awsKey),
			wantMatch: false,
		},
		// GCP API key and service-account private key
		{
			name:         "gcp api key",
			message:      "googleKey=" + gcpAPIKey,
			wantMatch:    true,
			wantRule:     "gcp-api-key",
			wantPrefix:   "AIza****",
			secretSample: gcpAPIKey,
		},
		{
			name:      "gcp api key near miss short",
			message:   "AIzaSyShort123",
			wantMatch: false,
		},
		{
			name:         "gcp service account private key",
			message:      `{"type":"service_account","private_key":"` + gcpPrivKey + `"}`,
			wantMatch:    true,
			wantRule:     "gcp-service-account-key",
			wantPrefix:   "----****",
			secretSample: gcpPrivKey,
		},
		{
			name:      "gcp service account near miss public key",
			message:   `{"type":"service_account","private_key":"-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG"}`,
			wantMatch: false,
		},
		// Slack tokens
		{
			name:         "slack bot token",
			message:      "SLACK_TOKEN=" + slackBot,
			wantMatch:    true,
			wantRule:     "slack-token",
			wantPrefix:   "xoxb****",
			secretSample: slackBot,
		},
		{
			name:         "slack user token",
			message:      "auth=" + slackUser,
			wantMatch:    true,
			wantRule:     "slack-token",
			wantPrefix:   "xoxp****",
			secretSample: slackUser,
		},
		{
			name:      "slack near miss placeholder",
			message:   "SLACK_TOKEN=xoxb-placeholder-without-digits",
			wantMatch: false,
		},
		{
			name:      "slack near miss invalid prefix",
			message:   "xox1-123456789012-AbCdEfGhIjKl",
			wantMatch: false,
		},
		// PEM private keys
		{
			name:         "pem rsa private key",
			message:      "loaded " + pemRSA + " from stdin",
			wantMatch:    true,
			wantRule:     "pem-private-key",
			wantPrefix:   "----****",
			secretSample: pemRSA,
		},
		{
			name:         "pem openssh private key",
			message:      pemOpenSSH,
			wantMatch:    true,
			wantRule:     "pem-private-key",
			wantPrefix:   "----****",
			secretSample: pemOpenSSH,
		},
		{
			name:         "pem pgp private key block",
			message:      pemPGP,
			wantMatch:    true,
			wantRule:     "pem-private-key",
			wantPrefix:   "----****",
			secretSample: pemPGP,
		},
		{
			name:      "pem near miss public key",
			message:   "-----BEGIN " + "PUBLIC KEY-----",
			wantMatch: false,
		},
		{
			name:      "pem near miss certificate",
			message:   "-----BEGIN " + "CERTIFICATE-----",
			wantMatch: false,
		},
		// Generic high-entropy assignments
		{
			name:         "generic *_TOKEN high entropy",
			message:      "DEPLOY_TOKEN=" + genericSecret,
			wantMatch:    true,
			wantRule:     "generic-secret-assignment",
			wantPrefix:   "Kz9m****",
			secretSample: genericSecret,
		},
		{
			name:         "generic *_SECRET high entropy",
			message:      `WEBHOOK_SECRET="` + genericSecret + `"`,
			wantMatch:    true,
			wantRule:     "generic-secret-assignment",
			wantPrefix:   "Kz9m****",
			secretSample: genericSecret,
		},
		{
			name:         "generic *_KEY high entropy",
			message:      "SIGNING_KEY=" + genericSecret,
			wantMatch:    true,
			wantRule:     "generic-secret-assignment",
			wantPrefix:   "Kz9m****",
			secretSample: genericSecret,
		},
		{
			name:      "generic near miss low entropy",
			message:   "DEPLOY_TOKEN=" + strings.Repeat("a", 24),
			wantMatch: false,
		},
		{
			name:      "generic near miss repeating pattern",
			message:   "DEPLOY_TOKEN=" + strings.Repeat("ab", 12),
			wantMatch: false,
		},
		{
			name:      "generic near miss path",
			message:   "TLS_KEY=/etc/ssl/private/server-2026-prod.pem",
			wantMatch: false,
		},
		{
			name:      "generic near miss public key",
			message:   "SSH_PUBLIC_KEY=" + genericSecret,
			wantMatch: false,
		},
		{
			name:      "generic near miss placeholder",
			message:   "API_KEY=your_example_api_key_value_1234",
			wantMatch: false,
		},
		{
			name:      "generic near miss short",
			message:   "API_TOKEN=short123",
			wantMatch: false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, ok := rs.ScanMessage("app.service", tc.message)
			if ok != tc.wantMatch {
				t.Fatalf("ScanMessage() matched = %v (got %+v), want %v", ok, got, tc.wantMatch)
			}
			if !tc.wantMatch {
				return
			}
			if got.Rule != tc.wantRule {
				t.Errorf("rule = %q, want %q", got.Rule, tc.wantRule)
			}
			if got.MaskedPrefix != tc.wantPrefix {
				t.Errorf("maskedPrefix = %q, want %q", got.MaskedPrefix, tc.wantPrefix)
			}
			assertMasked(t, got.MaskedPrefix, tc.secretSample)
		})
	}
}

func TestCustomRuleAndAllowlistSuppression(t *testing.T) {
	customTok := "corp_tok_" + strings.Repeat("X9y8", 7)
	ghTok := "ghp_" + strings.Repeat("b2C3", 9)

	cfg := Config{
		Rules: []RuleConfig{
			{
				Name:       "corp-token",
				Pattern:    `\b(corp_tok_[A-Za-z0-9]{20,})\b`,
				MinEntropy: 2.5,
			},
		},
		Allowlist: AllowlistConfig{
			Patterns: []string{`^ghp_0000`, `fixture-allow`},
			Units:    []string{"ignored.service", "dev-*.service"},
		},
	}
	rs, err := CompileRuleSet(cfg)
	if err != nil {
		t.Fatal(err)
	}

	m, ok := rs.ScanMessage("prod.service", "CORP_TOKEN="+customTok)
	if !ok || m.Rule != "corp-token" || m.MaskedPrefix != "corp****" {
		t.Fatalf("custom rule got (%+v, %v), want corp-token", m, ok)
	}
	assertMasked(t, m.MaskedPrefix, customTok)

	// Allowlist by exact unit and glob unit.
	for _, unit := range []string{"ignored.service", "dev-web.service"} {
		if m, ok := rs.ScanMessage(unit, ghTok); ok {
			t.Errorf("unit %q should be allowlisted, got %+v", unit, m)
		}
	}

	// Allowlist by pattern on message or secret prefix.
	allowPrefix := "ghp_0000" + strings.Repeat("a1B2", 8)
	if m, ok := rs.ScanMessage("prod.service", allowPrefix); ok {
		t.Errorf("secret matching allowlist pattern should be suppressed, got %+v", m)
	}
	if m, ok := rs.ScanMessage("prod.service", "fixture-allow "+ghTok); ok {
		t.Errorf("message matching allowlist pattern should be suppressed, got %+v", m)
	}

	// Non-allowlisted secret on same line as allowlisted secret still matches (both orders).
	if m, ok := rs.ScanMessage("prod.service", allowPrefix+" "+ghTok); !ok || m.Rule != "github-token" {
		t.Errorf("expected non-allowlisted secret after allowlisted secret to match, got (%+v, %v)", m, ok)
	}
	if m, ok := rs.ScanMessage("prod.service", ghTok+" "+allowPrefix); !ok || m.Rule != "github-token" {
		t.Errorf("expected non-allowlisted secret before allowlisted secret to match, got (%+v, %v)", m, ok)
	}
}

func TestMaskSecretBounds(t *testing.T) {
	if got := MaskSecret("abc"); got != "****" {
		t.Errorf("MaskSecret(abc) = %q, want ****", got)
	}
	if got := MaskSecret("abcd"); got != "****" {
		t.Errorf("MaskSecret(abcd) = %q, want ****", got)
	}
	if got := MaskSecret("abcdef"); got != "abcd****" {
		t.Errorf("MaskSecret(abcdef) = %q, want abcd****", got)
	}
	if got := MaskSecret("a\nbcd1234"); got != "a?bc****" {
		t.Errorf("MaskSecret(control) = %q, want a?bc****", got)
	}
}

func assertMasked(t *testing.T, haystack, secret string) {
	t.Helper()
	if secret == "" {
		return
	}
	if strings.Contains(haystack, secret) {
		t.Fatalf("full secret leaked in %q", haystack)
	}
	runes := []rune(secret)
	for i := 0; i+5 <= len(runes); i++ {
		sub := string(runes[i : i+5])
		if strings.Contains(haystack, sub) {
			t.Fatalf("more than 4 characters (%q) of secret leaked in %q", sub, haystack)
		}
	}
}
