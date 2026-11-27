package orgctl

import (
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"
)

func TestOrgctlDocs(t *testing.T) {
	readme := readDoc(t, "README.org")
	reference := readDoc(t, filepath.Join("docs", "reference.org"))
	index := readDoc(t, filepath.Join("..", "README.org"))

	lines := strings.Split(readme, "\n")
	if len(lines) < 10 {
		t.Fatalf("README has %d lines", len(lines))
	}
	if strings.Count(readme, "\n") > 400 {
		t.Fatalf("README has %d lines, want at most 400", strings.Count(readme, "\n"))
	}
	head := strings.Join(lines[:10], "\n")
	for _, want := range []string{"herdr.dev", "agentbus", "file you keep"} {
		if !strings.Contains(head, want) {
			t.Errorf("first 10 lines lack %q", want)
		}
	}
	if !strings.Contains(readme, "go install github.com/jaeyeom/experimental/devtools/orgctl/cmd/orgctl@latest") {
		t.Error("README has no go install line")
	}
	if !strings.Contains(section(readme, "Prerequisites and install", "Concepts"), "pkl") {
		t.Error("install section does not name the pkl CLI")
	}
	concepts := section(readme, "Concepts", "Quick start")
	if concepts == "" {
		t.Fatal("Concepts section is missing or is not before Quick start")
	}
	for _, term := range []string{
		"machine", "role", "pool", "workspace", "pane", "agentbus",
		"handle", "liveness", "brief", "decides", "approval", "needs",
		"durable", "scope", "when", "unmanaged",
	} {
		if !strings.Contains(concepts, term) {
			t.Errorf("Concepts does not define %q", term)
		}
	}
	if !strings.Contains(readme, "https://github.com/jaeyeom/experimental/issues/771") {
		t.Error("README does not link tracking issue 771")
	}
	if !strings.Contains(readme, "file:docs/reference.org") {
		t.Error("README does not link the reference")
	}
	if strings.Contains(readme, "* Plan lines") || strings.Contains(readme, "* Drift lines") {
		t.Error("line catalogs belong in the reference")
	}
	declared := declaredKeys(readme)
	for _, sample := range markedSamples(readme) {
		for _, role := range rolesInSample(sample) {
			if !declared[role] {
				t.Errorf("sample uses undeclared role %s\n%s", role, sample)
			}
		}
	}
	if !strings.Contains(reference, "brief unmanaged") {
		t.Error("reference marker table lacks the unmanaged brief line")
	}
	if !strings.Contains(reference, "does not change the exit code") {
		t.Error("reference does not say whether i changes the exit code")
	}
	pools := section(reference, "Pools and scope", "Import")
	if pools == "" || !strings.Contains(pools, "namePattern") || !strings.Contains(pools, "BEGIN_EXAMPLE") {
		t.Error("pools and scope need a worked example")
	}
	if !strings.Contains(index, "| orgctl") {
		t.Error("devtools README does not list orgctl")
	}
	for _, want := range []string{"render agents-md", "CLAUDE.md", "examples/acme.pkl", "# Role Class: review"} {
		if !strings.Contains(readme, want) {
			t.Errorf("README does not document %q", want)
		}
	}
	if !strings.Contains(reference, "agents-md PATH edited in place") {
		t.Error("reference does not document agents-md drift lines")
	}
	for _, want := range []string{"cron publish", "--only", "--json", "--cron-state-dir"} {
		if !strings.Contains(readme, want) {
			t.Errorf("README does not document %q", want)
		}
		if !strings.Contains(reference, want) {
			t.Errorf("reference does not document %q", want)
		}
	}
	if !strings.Contains(reference, "ROLE cron stale") || !strings.Contains(reference, "unlisted") {
		t.Error("reference does not document published cron markers")
	}
}

func section(doc, start, end string) string {
	i := strings.Index(doc, "* "+start)
	if i < 0 {
		return ""
	}
	rest := doc[i:]
	j := strings.Index(rest[len("* "+start):], "\n* "+end)
	if j < 0 {
		return ""
	}
	return rest[:len("* "+start)+j]
}

var declaredKey = regexp.MustCompile(`\["([^"]+)"\]`)

func declaredKeys(doc string) map[string]bool {
	out := map[string]bool{}
	for _, match := range declaredKey.FindAllStringSubmatch(doc, -1) {
		out[match[1]] = true
	}
	return out
}

var exampleBlock = regexp.MustCompile(`(?s)#\+BEGIN_EXAMPLE\n(.*?)\n#\+END_EXAMPLE`)

func markedSamples(doc string) []string {
	var out []string
	for _, match := range exampleBlock.FindAllStringSubmatch(doc, -1) {
		body := match[1]
		if strings.Contains(body, "\n+") || strings.HasPrefix(body, "+") || strings.Contains(body, "USER STEP:") || strings.Contains(body, "USER:") {
			out = append(out, body)
		}
	}
	return out
}

var sampleRole = regexp.MustCompile(`(?:^[+~!?i] |^USER STEP: |role )(\S+)`)

func rolesInSample(sample string) []string {
	var roles []string
	for _, line := range strings.Split(sample, "\n") {
		match := sampleRole.FindStringSubmatch(line)
		if match == nil || !strings.Contains(match[1], "/") {
			continue
		}
		if strings.HasPrefix(line, "+ tab ") || strings.HasPrefix(line, "USER: close tab ") {
			continue
		}
		roles = append(roles, strings.TrimRight(match[1], ":"))
	}
	return roles
}

func readDoc(t *testing.T, name string) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), name))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools/orgctl", name))
	}
	for _, path := range candidates {
		body, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		return string(body)
	}
	t.Fatalf("%s not found", name)
	return ""
}
