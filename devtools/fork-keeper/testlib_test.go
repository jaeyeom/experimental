package forkkeeper

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

func TestMain(m *testing.M) {
	switch os.Getenv("FORK_KEEPER_TEST_HELPER") {
	case "gh":
		os.Exit(runStubGH(os.Args[1:]))
	case "agent":
		os.Exit(runFakeAgent(os.Args[1:]))
	default:
		os.Exit(m.Run())
	}
}

type testFixture struct {
	t           *testing.T
	root        string
	upstreamGit string
	originGit   string
	useed       string
	fseed       string
}

func ensureTestPATH(t *testing.T, extraDirs ...string) {
	t.Helper()
	cur := os.Getenv("PATH")
	parts := append([]string{}, extraDirs...)
	if cur != "" {
		parts = append(parts, cur)
	}
	parts = append(parts, "/opt/homebrew/bin", "/usr/local/bin", "/home/linuxbrew/.linuxbrew/bin")
	t.Setenv("PATH", strings.Join(parts, string(os.PathListSeparator)))
}

func isolateGitEnv(t *testing.T, baseDir string) {
	t.Helper()
	noHooks := filepath.Join(baseDir, "nohooks")
	if err := os.MkdirAll(noHooks, 0o755); err != nil {
		t.Fatal(err)
	}
	home := filepath.Join(baseDir, "home")
	if err := os.MkdirAll(home, 0o755); err != nil {
		t.Fatal(err)
	}
	gitconfig := filepath.Join(baseDir, "gitconfig")
	content := fmt.Sprintf(`[user]
	name = Fork Keeper Test
	email = fork-keeper@example.invalid
[core]
	hooksPath = %s
[init]
	defaultBranch = main
[advice]
	detachedHead = false
`, noHooks)
	if err := os.WriteFile(gitconfig, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Setenv("GIT_CONFIG_GLOBAL", gitconfig)
	t.Setenv("GIT_CONFIG_NOSYSTEM", "1")
	t.Setenv("HOME", home)
}

func newTestFixture(t *testing.T) *testFixture {
	t.Helper()
	ensureTestPATH(t)
	root := t.TempDir()
	isolateGitEnv(t, root)

	fx := &testFixture{
		t:           t,
		root:        root,
		upstreamGit: filepath.Join(root, "upstream.git"),
		originGit:   filepath.Join(root, "origin.git"),
		useed:       filepath.Join(root, "useed"),
		fseed:       filepath.Join(root, "fseed"),
	}

	fx.mustRun("", "git", "init", "-q", "--bare", "-b", "main", fx.upstreamGit)
	fx.mustRun("", "git", "init", "-q", "--bare", "-b", "main", fx.originGit)
	fx.mustRun("", "git", "init", "-q", "-b", "main", fx.useed)
	fx.mustRun(fx.useed, "git", "remote", "add", "origin", fx.upstreamGit)
	if err := os.WriteFile(filepath.Join(fx.useed, "README"), []byte("base\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	fx.mustRun(fx.useed, "git", "add", "README")
	fx.mustRun(fx.useed, "git", "commit", "-q", "-m", "U0")
	fx.mustRun(fx.useed, "git", "push", "-q", "origin", "main")
	fx.mustRun(fx.useed, "git", "push", "-q", fx.originGit, "main")
	fx.mustRun("", "git", "clone", "-q", fx.originGit, fx.fseed)
	return fx
}

func (fx *testFixture) mustRun(dir, name string, args ...string) string {
	fx.t.Helper()
	cmd := exec.Command(name, args...)
	if dir != "" {
		cmd.Dir = dir
	}
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		fx.t.Fatalf("%s %s (dir=%s): %v\nstdout: %s\nstderr: %s",
			name, strings.Join(args, " "), dir, err, stdout.String(), stderr.String())
	}
	return strings.TrimSpace(stdout.String())
}

func (fx *testFixture) ucommit(relPath, line string) string {
	fx.t.Helper()
	p := filepath.Join(fx.useed, relPath)
	if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
		fx.t.Fatal(err)
	}
	f, err := os.OpenFile(p, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		fx.t.Fatal(err)
	}
	if _, err := fmt.Fprintln(f, line); err != nil {
		_ = f.Close()
		fx.t.Fatal(err)
	}
	if err := f.Close(); err != nil {
		fx.t.Fatal(err)
	}
	fx.mustRun(fx.useed, "git", "add", relPath)
	fx.mustRun(fx.useed, "git", "commit", "-q", "-m", "upstream: "+relPath)
	fx.mustRun(fx.useed, "git", "push", "-q", "-f", "origin", "HEAD:main")
	return fx.mustRun(fx.useed, "git", "rev-parse", "HEAD")
}

func (fx *testFixture) fcommit(relPath, line string) string {
	fx.t.Helper()
	fx.mustRun(fx.fseed, "git", "pull", "-q", "--rebase", "origin", "main")
	p := filepath.Join(fx.fseed, relPath)
	if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
		fx.t.Fatal(err)
	}
	f, err := os.OpenFile(p, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		fx.t.Fatal(err)
	}
	if _, err := fmt.Fprintln(f, line); err != nil {
		_ = f.Close()
		fx.t.Fatal(err)
	}
	if err := f.Close(); err != nil {
		fx.t.Fatal(err)
	}
	fx.mustRun(fx.fseed, "git", "add", relPath)
	fx.mustRun(fx.fseed, "git", "commit", "-q", "-m", "mine: "+relPath)
	fx.mustRun(fx.fseed, "git", "push", "-q", "origin", "HEAD:main")
	return fx.mustRun(fx.fseed, "git", "rev-parse", "HEAD")
}

func (fx *testFixture) recordHead(sha string) {
	fx.t.Helper()
	fx.mustRun(fx.useed, "git", "push", "-q", "-f", fx.originGit, sha+":refs/fork-keeper/upstream-head")
}

func (fx *testFixture) forkAtUpstream() {
	fx.t.Helper()
	fx.mustRun(fx.useed, "git", "push", "-q", "-f", fx.originGit, "HEAD:main")
}

func (fx *testFixture) oref(ref string) string {
	fx.t.Helper()
	//nolint:gosec // Test fixture git command.
	cmd := exec.Command("git", "--git-dir="+fx.originGit, "rev-parse", "-q", "--verify", ref)
	out, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}

func (fx *testFixture) uref(ref string) string {
	fx.t.Helper()
	//nolint:gosec // Test fixture git command.
	cmd := exec.Command("git", "--git-dir="+fx.upstreamGit, "rev-parse", "-q", "--verify", ref)
	out, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}

func (fx *testFixture) fetchOnlyWorkdir() string {
	fx.t.Helper()
	workDir := filepath.Join(fx.root, "w")
	_ = os.RemoveAll(workDir)
	fx.mustRun("", "git", "init", "-q", workDir)
	fx.mustRun(workDir, "git", "fetch", "-q", fx.originGit,
		"+refs/heads/*:refs/remotes/origin/*",
		"+refs/fork-keeper/*:refs/fork-keeper/*")
	fx.mustRun(workDir, "git", "fetch", "-q", "--no-tags", fx.upstreamGit,
		"+refs/heads/*:refs/remotes/upstream/*",
		"+refs/tags/*:refs/upstream-tags/*")
	return workDir
}

func (fx *testFixture) prepTripwireRepo() (workDir string, rangeInfo RangeInfo, candSHA string) {
	fx.t.Helper()
	workDir = fx.fetchOnlyWorkdir()
	var err error
	rangeInfo, err = ComputeRange(context.Background(), filepath.Join(workDir, ".git"), "upstream", "main")
	if err != nil {
		fx.t.Fatalf("ComputeRange: %v", err)
	}
	fx.mustRun(workDir, "git", "checkout", "-q", "-B", "cand", rangeInfo.UpstreamNew)
	for _, c := range rangeInfo.UserCommits {
		fx.mustRun(workDir, "git", "cherry-pick", c)
	}
	candSHA = fx.mustRun(workDir, "git", "rev-parse", "HEAD")
	return workDir, rangeInfo, candSHA
}

type stubPRRecord struct {
	Number       int      `json:"number"`
	HeadRefName  string   `json:"headRefName"`
	Labels       []string `json:"labels"`
	HeadOverride string   `json:"head_override,omitempty"` //nolint:tagliatelle // Matches stub gh test fixture schema.
}

func runStubGH(args []string) int {
	ghState := os.Getenv("GH_STATE")
	fakeOrigin := os.Getenv("FAKE_ORIGIN")
	statePath := filepath.Join(ghState, "prs.json")
	if _, err := os.Stat(statePath); errors.Is(err, fs.ErrNotExist) {
		_ = os.WriteFile(statePath, []byte("[]\n"), 0o600)
	}
	appendLine(filepath.Join(ghState, "calls"), strings.Join(args, " "))
	if marker := os.Getenv("FAKE_GH_MARKER"); marker != "" {
		appendLine(marker, strings.Join(args, " "))
	}

	if len(args) < 2 {
		fmt.Fprintf(os.Stderr, "gh stub: missing command: %v\n", args)
		return 1
	}

	switch {
	case args[0] == "pr" && args[1] == "list":
		return stubGHPRList(statePath, fakeOrigin)
	case args[0] == "pr" && args[1] == "create":
		return stubGHPRCreate(statePath, ghState, args[2:])
	case args[0] == "pr" && args[1] == "edit":
		return stubGHPREdit(statePath, ghState, args[2:])
	case args[0] == "api":
		return stubGHAPI(statePath, ghState, args[1:])
	default:
		fmt.Fprintf(os.Stderr, "gh stub: unsupported: %s\n", strings.Join(args, " "))
		return 1
	}
}

func loadStubPRs(path string) ([]stubPRRecord, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read stub prs %s: %w", path, err)
	}
	var prs []stubPRRecord
	if err := json.Unmarshal(data, &prs); err != nil {
		return nil, fmt.Errorf("decode stub prs %s: %w", path, err)
	}
	return prs, nil
}

func saveStubPRs(path string, prs []stubPRRecord) error {
	data, err := json.Marshal(prs)
	if err != nil {
		return fmt.Errorf("encode stub prs: %w", err)
	}
	if err := os.WriteFile(path, data, 0o600); err != nil {
		return fmt.Errorf("write stub prs %s: %w", path, err)
	}
	return nil
}

func stubGHPRList(statePath, fakeOrigin string) int {
	prs, err := loadStubPRs(statePath)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	out := make([]ghPR, 0)
	for _, r := range prs {
		//nolint:gosec // Test stub helper running git against test fixture repo.
		headCmd := exec.Command("git", "--git-dir="+fakeOrigin, "rev-parse", "-q", "--verify", "refs/heads/"+r.HeadRefName)
		headOut, err := headCmd.Output()
		if err != nil {
			continue
		}
		head := strings.TrimSpace(string(headOut))
		// Skip PRs already merged into main.
		//nolint:gosec // Test stub helper running git against test fixture repo.
		ancCmd := exec.Command("git", "--git-dir="+fakeOrigin, "merge-base", "--is-ancestor", head, "refs/heads/main")
		if ancCmd.Run() == nil {
			continue
		}
		oid := head
		if r.HeadOverride != "" {
			oid = r.HeadOverride
		}
		labels := make([]ghLabel, 0, len(r.Labels))
		for _, l := range r.Labels {
			labels = append(labels, ghLabel{Name: l})
		}
		out = append(out, ghPR{
			Number:      r.Number,
			HeadRefName: r.HeadRefName,
			HeadRefOid:  oid,
			Labels:      labels,
			URL:         fmt.Sprintf("https://github.invalid/pull/%d", r.Number),
		})
	}
	encoded, err := json.Marshal(out)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	fmt.Println(string(encoded))
	return 0
}

func stubGHPRCreate(statePath, ghState string, args []string) int {
	var head, body string
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--head":
			if i+1 < len(args) {
				head = args[i+1]
				i++
			}
		case "--body-file":
			if i+1 < len(args) {
				body = args[i+1]
				i++
			}
		}
	}
	prs, err := loadStubPRs(statePath)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	n := len(prs) + 1
	prs = append(prs, stubPRRecord{
		Number:      n,
		HeadRefName: head,
		Labels:      []string{},
	})
	if err := saveStubPRs(statePath, prs); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	if body != "" {
		if data, err := os.ReadFile(body); err == nil {
			_ = os.WriteFile(filepath.Join(ghState, fmt.Sprintf("body.%d", n)), data, 0o600)
		}
	}
	if hook := os.Getenv("FAKE_GH_ON_CREATE"); hook != "" {
		//nolint:gosec // Test stub helper running test-provided FAKE_GH_ON_CREATE hook.
		cmd := exec.Command("sh", "-c", hook)
		if out, err := cmd.CombinedOutput(); err != nil {
			fmt.Fprintf(os.Stderr, "FAKE_GH_ON_CREATE failed: %v\n%s\n", err, out)
			return 1
		}
	}
	fmt.Printf("https://github.invalid/pull/%d\n", n)
	return 0
}

func stubGHPREdit(statePath, ghState string, args []string) int {
	if len(args) == 0 {
		return 1
	}
	sel := args[0]
	prs, err := loadStubPRs(statePath)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	for i := 1; i < len(args); i++ {
		switch args[i] {
		case "--add-label":
			if i+1 < len(args) {
				label := args[i+1]
				i++
				for idx := range prs {
					if prs[idx].HeadRefName == sel {
						found := false
						for _, existing := range prs[idx].Labels {
							if existing == label {
								found = true
								break
							}
						}
						if !found {
							prs[idx].Labels = append(prs[idx].Labels, label)
						}
					}
				}
			}
		case "--remove-label":
			if i+1 < len(args) {
				label := args[i+1]
				i++
				for idx := range prs {
					if prs[idx].HeadRefName == sel {
						filtered := prs[idx].Labels[:0]
						for _, existing := range prs[idx].Labels {
							if existing != label {
								filtered = append(filtered, existing)
							}
						}
						prs[idx].Labels = filtered
					}
				}
			}
		case "--body-file":
			if i+1 < len(args) {
				body := args[i+1]
				i++
				if data, err := os.ReadFile(body); err == nil {
					target := filepath.Join(ghState, "body."+sel)
					if err := os.WriteFile(target, data, 0o600); err != nil {
						_ = os.WriteFile(filepath.Join(ghState, "body.edit"), data, 0o600)
					}
				}
			}
		default:
			if i+1 < len(args) && strings.HasPrefix(args[i], "-") {
				i++
			}
		}
	}
	if err := saveStubPRs(statePath, prs); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	return 0
}

func stubGHAPI(statePath, ghState string, args []string) int {
	endpoint := ""
	for _, a := range args {
		if a == "--paginate" {
			continue
		}
		endpoint = a
		break
	}
	re := regexp.MustCompile(`.*/issues/([0-9]+)/timeline`)
	m := re.FindStringSubmatch(endpoint)
	if len(m) != 2 {
		fmt.Fprintf(os.Stderr, "gh stub api: unsupported endpoint %q\n", endpoint)
		return 1
	}
	prNum := m[1]
	timelineFile := filepath.Join(ghState, "timeline."+prNum)
	if data, err := os.ReadFile(timelineFile); err == nil {
		fmt.Print(string(data))
		return 0
	}
	prs, err := loadStubPRs(statePath)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	for _, p := range prs {
		if fmt.Sprintf("%d", p.Number) == prNum {
			for _, l := range p.Labels {
				if l == labelApproved {
					fmt.Println(`[{"event":"labeled","label":{"name":"fork-keeper:approved"}}]`)
					return 0
				}
			}
		}
	}
	fmt.Println("[]")
	return 0
}

func runFakeAgent(args []string) int {
	ghState := os.Getenv("GH_STATE")
	outDir := os.Getenv("FORK_KEEPER_OUT")
	for i := 0; i < len(args); i++ {
		if args[i] == "--out" && i+1 < len(args) {
			outDir = args[i+1]
			i++
		}
	}
	appendLine(filepath.Join(ghState, "agent.runs"), "run")
	_ = os.WriteFile(filepath.Join(ghState, "agent.env"), []byte(strings.Join(os.Environ(), "\n")+"\n"), 0o600)
	if remOut, err := exec.Command("git", "remote").Output(); err == nil {
		_ = os.WriteFile(filepath.Join(ghState, "agent.remotes"), remOut, 0o600)
	}

	mode := os.Getenv("FAKE_AGENT_MODE")
	if mode == "no-candidate" {
		return 0
	}
	if mode == "fail-no-candidate" {
		fmt.Fprintln(os.Stderr, "simulated agent failure")
		return 1
	}

	rangeFile := os.Getenv("FORK_KEEPER_RANGE_FILE")
	rangeBytes, err := os.ReadFile(rangeFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "read range file %s: %v\n", rangeFile, err)
		return 1
	}
	var rangeInfo RangeInfo
	if err := json.Unmarshal(rangeBytes, &rangeInfo); err != nil {
		fmt.Fprintf(os.Stderr, "parse range file: %v\n", err)
		return 1
	}

	if mode == "stdout-clean" {
		cand := os.Getenv("FORK_KEEPER_CANDIDATE_HEAD")
		v := Verdict{
			Schema:        1,
			Verdict:       VerdictClean,
			CandidateHead: cand,
			Range:         rangeInfo,
			Conflicts:     []VerdictConflict{},
			Findings:      []VerdictFinding{},
			Summary:       "stdout clean report",
		}
		b, _ := json.Marshal(v)
		fmt.Println(string(b))
		return 0
	}

	if err := runGit(context.Background(), "", "checkout", "-q", "-B", "fork-keeper/candidate", rangeInfo.UpstreamNew); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	for _, c := range rangeInfo.UserCommits {
		if err := runGit(context.Background(), "", "cherry-pick", c); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if mode == "none" {
		return 0
	}

	headOut, err := exec.Command("git", "rev-parse", "HEAD").Output()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	candHead := strings.TrimSpace(string(headOut))

	verdict := VerdictClean
	switch mode {
	case "needs-review":
		verdict = VerdictNeedsReview
	case "lie":
		rangeInfo.FirstRun = false
		rangeInfo.Rewritten = false
		rangeInfo.MovedTags = []MovedTag{}
		rangeInfo.DeletedTags = []string{}
	}

	v := Verdict{
		Schema:        1,
		Verdict:       verdict,
		CandidateHead: candHead,
		Range:         rangeInfo,
		Conflicts:     []VerdictConflict{},
		Findings:      []VerdictFinding{},
		Summary:       "stub",
	}
	vBytes, err := json.Marshal(v)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	if err := os.WriteFile(filepath.Join(outDir, "verdict.json"), vBytes, 0o600); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	if err := os.WriteFile(filepath.Join(outDir, "report.md"), []byte(fmt.Sprintf("stub report (%s)\n", verdict)), 0o600); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	return 0
}

func appendLine(path, line string) {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o600)
	if err != nil {
		return
	}
	_, _ = fmt.Fprintln(f, line)
	_ = f.Close()
}
