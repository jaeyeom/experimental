// Package dlp implements two-stage data loss prevention scanning for secrets,
// high-entropy assignments, and host metadata leakage in job deliverables.
package dlp

import (
	"bytes"
	"math"
	"path/filepath"
	"regexp"
	"strings"
)

const (
	worktreePlaceholder = "<repo>"
	minEntropyLen       = 20
	minEntropyBits      = 4.0
	minHostnameLen      = 4
	minUsernameLen      = 3
)

var (
	reGitHubPAT            = regexp.MustCompile(`ghp_[A-Za-z0-9]{36}`)
	reGitHubOAuth          = regexp.MustCompile(`gho_[A-Za-z0-9]{36}`)
	reGitHubAppToken       = regexp.MustCompile(`ghs_[A-Za-z0-9]{36}`)
	reGitHubUserToken      = regexp.MustCompile(`ghu_[A-Za-z0-9]{36}`)
	reGitHubFineGrainedPAT = regexp.MustCompile(`github_pat_[A-Za-z0-9_]{20,}`)
	reAnthropicKey         = regexp.MustCompile(`sk-ant-[A-Za-z0-9_-]{20,}`)
	reOpenAIKey            = regexp.MustCompile(`sk-(?:proj-)?[A-Za-z0-9_-]{20,}`)
	reAWSAccessKeyID       = regexp.MustCompile(`(?:AKIA|ASIA)[0-9A-Z]{16}`)
	reSlackToken           = regexp.MustCompile(`xox[baprs]-[A-Za-z0-9-]{10,}`)
	rePEMPrivateKey        = regexp.MustCompile(`-----BEGIN [A-Z ]*PRIVATE KEY-----`)
	reBearerToken          = regexp.MustCompile(`Bearer\s+[A-Za-z0-9._-]{20,}`)
	reJWTToken             = regexp.MustCompile(`eyJ[A-Za-z0-9_-]{10,}\.eyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}`)

	reSecretAssignment = regexp.MustCompile(
		`(?i)["']?[A-Za-z0-9_.-]*(?:secret|token|api_key|apikey|password|passwd|private_key)[A-Za-z0-9_.-]*["']?` +
			`[ \t]*(?::=|=|:)[ \t]*` +
			`(?:["']([^\s"']{20,})["']|([^\s"',;}\])]{20,}))`,
	)

	reGenericHomeDir = regexp.MustCompile(`(/Users/|/home/)[A-Za-z0-9._-]+/`)
)

// HostContext describes host-specific metadata used to normalize worktree paths
// and detect host leakage during DLP scanning.
type HostContext struct {
	WorktreePath  string   // Ephemeral worktree path to normalize to "<repo>" before scanning.
	Username      string   // Local OS username (blocked if appearing as path/user leak).
	HomeDir       string   // Consumer's real home directory path (e.g. "/Users/alice", "/home/alice").
	Hostname      string   // Consumer's local hostname (ignored if empty or < 4 chars).
	PersonalEmail string   // Consumer's personal git email (blocked case-insensitively).
	ExtraDenied   []string // Optional additional literal sensitive strings to block.
}

// FindingKind categorizes the type of DLP violation detected.
type FindingKind string

const (
	// FindingSecret indicates a high-confidence credential or secret pattern match.
	FindingSecret FindingKind = "secret"
	// FindingHighEntropy indicates a high-entropy value in a secret-like assignment.
	FindingHighEntropy FindingKind = "high_entropy"
	// FindingHostLeak indicates leaked host metadata or local filesystem paths.
	FindingHostLeak FindingKind = "host_leak"
)

// Finding describes a single DLP violation without exposing raw secret values.
type Finding struct {
	Kind   FindingKind `json:"kind"`
	Rule   string      `json:"rule"`
	Detail string      `json:"detail"` // Safe summary; NEVER include the raw secret value!
}

// Result holds the outcome of scanning a payload, including normalized output
// and any detected findings.
type Result struct {
	Blocked    bool      `json:"blocked"`
	Normalized []byte    `json:"-"` // Input with WorktreePath normalized to "<repo>".
	Findings   []Finding `json:"findings,omitempty"`
}

type secretRule struct {
	name   string
	re     *regexp.Regexp
	detail string
}

// Scanner inspects job deliverables for secrets, high-entropy assignments, and
// host metadata leakage.
type Scanner struct {
	secretRules []secretRule
}

// NewScanner constructs a Scanner configured with all built-in DLP rules.
func NewScanner() *Scanner {
	return &Scanner{
		secretRules: []secretRule{
			{name: "github-pat", re: reGitHubPAT, detail: "detected potential GitHub personal access token"},
			{name: "github-oauth", re: reGitHubOAuth, detail: "detected potential GitHub OAuth token"},
			{name: "github-app-token", re: reGitHubAppToken, detail: "detected potential GitHub app token"},
			{name: "github-user-token", re: reGitHubUserToken, detail: "detected potential GitHub user token"},
			{name: "github-fine-grained-pat", re: reGitHubFineGrainedPAT, detail: "detected potential GitHub fine-grained token"},
			{name: "anthropic-api-key", re: reAnthropicKey, detail: "detected potential Anthropic API key"},
			{name: "openai-api-key", re: reOpenAIKey, detail: "detected potential OpenAI API key"},
			{name: "aws-access-key-id", re: reAWSAccessKeyID, detail: "detected potential AWS access key ID"},
			{name: "slack-token", re: reSlackToken, detail: "detected potential Slack token"},
			{name: "pem-private-key", re: rePEMPrivateKey, detail: "detected PEM private key header"},
			{name: "bearer-token", re: reBearerToken, detail: "detected potential Bearer authorization token"},
			{name: "jwt-token", re: reJWTToken, detail: "detected potential JWT token"},
		},
	}
}

// Scan normalizes worktree paths in payload and evaluates all DLP rules against
// the normalized content.
func (s *Scanner) Scan(payload []byte, host HostContext) Result {
	normalized := normalizeWorktree(payload, host.WorktreePath)
	text := string(normalized)

	var findings []Finding
	findings = append(findings, s.scanSecrets(normalized)...)
	findings = append(findings, scanHighEntropyAssignments(text)...)
	findings = append(findings, scanHostLeakage(text, host)...)

	return Result{
		Blocked:    len(findings) > 0,
		Normalized: normalized,
		Findings:   findings,
	}
}

func normalizeWorktree(payload []byte, worktreePath string) []byte {
	normalized := bytes.Clone(payload)
	if worktreePath == "" {
		return normalized
	}
	cleaned := strings.TrimRight(filepath.Clean(worktreePath), "/")
	if cleaned == "" || cleaned == "." {
		return normalized
	}
	return bytes.ReplaceAll(normalized, []byte(cleaned), []byte(worktreePlaceholder))
}

func (s *Scanner) scanSecrets(data []byte) []Finding {
	var findings []Finding
	for _, rule := range s.secretRules {
		if rule.re.Match(data) {
			findings = append(findings, Finding{
				Kind:   FindingSecret,
				Rule:   rule.name,
				Detail: rule.detail,
			})
		}
	}
	return findings
}

func scanHighEntropyAssignments(text string) []Finding {
	matches := reSecretAssignment.FindAllStringSubmatch(text, -1)
	for _, m := range matches {
		val := m[1]
		if val == "" {
			val = m[2]
		}
		if len(val) >= minEntropyLen && shannonEntropy(val) >= minEntropyBits {
			return []Finding{
				{
					Kind:   FindingHighEntropy,
					Rule:   "high-entropy-assignment",
					Detail: "detected high-entropy value in secret-like assignment",
				},
			}
		}
	}
	return nil
}

func scanHostLeakage(text string, host HostContext) []Finding {
	var findings []Finding

	if homeDir := cleanPathPrefix(host.HomeDir); homeDir != "" && strings.Contains(text, homeDir) {
		findings = append(findings, Finding{
			Kind:   FindingHostLeak,
			Rule:   "host-home-dir",
			Detail: "detected host home directory path in deliverable",
		})
	}

	if host.PersonalEmail != "" && strings.Contains(strings.ToLower(text), strings.ToLower(host.PersonalEmail)) {
		findings = append(findings, Finding{
			Kind:   FindingHostLeak,
			Rule:   "host-personal-email",
			Detail: "detected personal email address in deliverable",
		})
	}

	if len(host.Hostname) >= minHostnameLen && containsBounded(text, host.Hostname) {
		findings = append(findings, Finding{
			Kind:   FindingHostLeak,
			Rule:   "host-hostname",
			Detail: "detected host machine name in deliverable",
		})
	}

	if reGenericHomeDir.MatchString(text) {
		findings = append(findings, Finding{
			Kind:   FindingHostLeak,
			Rule:   "generic-home-dir",
			Detail: "detected personal home directory path in deliverable",
		})
	}

	findings = append(findings, scanUsernameAndExtra(text, host)...)
	return findings
}

func scanUsernameAndExtra(text string, host HostContext) []Finding {
	var findings []Finding

	if len(host.Username) >= minUsernameLen {
		if strings.Contains(text, "/Users/"+host.Username) || strings.Contains(text, "/home/"+host.Username) {
			findings = append(findings, Finding{
				Kind:   FindingHostLeak,
				Rule:   "host-username-path",
				Detail: "detected host username path in deliverable",
			})
		}
	}

	for _, denied := range host.ExtraDenied {
		if denied != "" && strings.Contains(text, denied) {
			findings = append(findings, Finding{
				Kind:   FindingHostLeak,
				Rule:   "host-extra-denied",
				Detail: "detected denied host string in deliverable",
			})
			break
		}
	}

	return findings
}

func cleanPathPrefix(p string) string {
	if p == "" {
		return ""
	}
	cleaned := strings.TrimRight(filepath.Clean(p), "/")
	if cleaned == "" || cleaned == "." {
		return ""
	}
	return cleaned
}

func shannonEntropy(s string) float64 {
	if len(s) == 0 {
		return 0
	}
	counts := make(map[byte]int)
	for i := range len(s) {
		counts[s[i]]++
	}
	total := float64(len(s))
	var entropy float64
	for _, count := range counts {
		p := float64(count) / total
		entropy -= p * math.Log2(p)
	}
	return entropy
}

func containsBounded(text, needle string) bool {
	if needle == "" {
		return false
	}
	start := 0
	for start <= len(text)-len(needle) {
		idx := strings.Index(text[start:], needle)
		if idx < 0 {
			return false
		}
		pos := start + idx
		end := pos + len(needle)
		leftOK := pos == 0 || !isASCIIAlphaNum(text[pos-1])
		rightOK := end == len(text) || !isASCIIAlphaNum(text[end])
		if leftOK && rightOK {
			return true
		}
		start = pos + 1
	}
	return false
}

func isASCIIAlphaNum(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9')
}
