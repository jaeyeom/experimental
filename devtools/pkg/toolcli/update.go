package toolcli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// defaultRef is the revision tools are updated to: they are installed from main.
const defaultRef = "main"

// GoInstaller installs Go binaries and queries revisions using the `go` tool.
type GoInstaller struct {
	// Run executes `go <args>` and returns stdout. Defaults to the real go tool.
	Run func(ctx context.Context, args ...string) ([]byte, error)
}

func (g GoInstaller) run(ctx context.Context, env []string, args ...string) ([]byte, error) {
	if g.Run != nil {
		return g.Run(ctx, args...)
	}
	cmd := exec.CommandContext(ctx, "go", args...) //nolint:gosec // G204: args come from tool configuration / CLI ref
	cmd.Env = append(os.Environ(), env...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		if msg := strings.TrimSpace(stderr.String()); msg != "" {
			return nil, errors.New(msg)
		}
		return nil, fmt.Errorf("run go: %w", err)
	}
	return stdout.Bytes(), nil
}

// LatestRevision queries `go list -m -json <module>@<ref>` to resolve the
// revision of ref. The argument must be a module path, not a package path.
func (g GoInstaller) LatestRevision(ctx context.Context, module, ref string) (RevisionInfo, error) {
	if ref == "" {
		ref = defaultRef
	}
	target := module + "@" + ref
	stdout, err := g.run(ctx, nil, "list", "-m", "-json", target)
	if err != nil {
		return RevisionInfo{}, fmt.Errorf("go list %s: %w", target, err)
	}
	var mod struct {
		Version string    `json:"Version"` //nolint:tagliatelle // go list -m -json uses PascalCase
		Time    time.Time `json:"Time"`    //nolint:tagliatelle // go list -m -json uses PascalCase
		Origin  *struct {
			Hash string `json:"Hash"` //nolint:tagliatelle // go list -m -json uses PascalCase
		} `json:"Origin"` //nolint:tagliatelle // go list -m -json uses PascalCase
	}
	if err := json.Unmarshal(stdout, &mod); err != nil {
		return RevisionInfo{}, fmt.Errorf("parse go list output: %w", err)
	}
	rev := mod.Version
	if mod.Origin != nil && mod.Origin.Hash != "" {
		rev = mod.Origin.Hash
	}
	return RevisionInfo{Version: mod.Version, Revision: rev, Time: mod.Time}, nil
}

// Install runs `go install <pkg>@<ref>` with GOBIN set to binDir.
func (g GoInstaller) Install(ctx context.Context, pkg, ref, binDir string) error {
	if ref == "" {
		ref = defaultRef
	}
	target := pkg + "@" + ref
	if _, err := g.run(ctx, []string{"GOBIN=" + binDir}, "install", target); err != nil {
		return fmt.Errorf("go install %s: %w", target, err)
	}
	return nil
}

// BinaryUpdate records the update status of one binary.
type BinaryUpdate struct {
	Name        string `json:"name"`
	Package     string `json:"package"`
	OldRevision string `json:"oldRevision"`
	NewRevision string `json:"newRevision"`
	Updated     bool   `json:"updated"`
}

// StaleSkill records an installed skill that needs refreshing after an update.
type StaleSkill struct {
	Agent         Agent  `json:"agent"`
	Scope         Scope  `json:"scope"`
	Path          string `json:"path"`
	InstalledHash string `json:"installedHash"`
	CurrentHash   string `json:"currentHash"`
}

// UpdateOutput is the JSON payload for `<tool> update --json`.
type UpdateOutput struct {
	Tool            string         `json:"tool"`
	Ref             string         `json:"ref"`
	CheckOnly       bool           `json:"checkOnly"`
	UpdateAvailable bool           `json:"updateAvailable"`
	Updated         bool           `json:"updated"`
	Binaries        []BinaryUpdate `json:"binaries"`
	RestartNeeded   []string       `json:"restartNeeded,omitempty"`
	StaleSkills     []StaleSkill   `json:"staleSkills,omitempty"`
	RefreshCommand  string         `json:"refreshCommand,omitempty"`
}

func (t *Tool) cmdUpdate(ctx context.Context, args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("update", flag.ContinueOnError)
	fs.SetOutput(stderr)
	check := fs.Bool("check", false, "check if a newer revision exists without installing")
	ref := fs.String("ref", defaultRef, "target branch, tag, or version")
	force := fs.Bool("force", false, "update even if the target revision is older than the installed one")
	jsonOut := fs.Bool("json", false, "output JSON")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		return failMsg(stderr, "update does not take positional arguments", 2)
	}
	if t.MainPackage == "" {
		return failMsg(stderr, "tool main package is not configured", 2)
	}

	out, code, err := t.runUpdate(ctx, *check, *force, *ref)
	if err != nil {
		return failErr(stderr, err, 2)
	}

	if *jsonOut {
		if err := writeJSON(stdout, out); err != nil {
			return failErr(stderr, err, 2)
		}
		return code
	}

	printUpdateHuman(stdout, out)
	return code
}

func (t *Tool) runUpdate(ctx context.Context, check, force bool, ref string) (UpdateOutput, int, error) {
	inst := t.installer()
	bi := t.buildInfo()
	oldRev := currentRevLabel(bi)

	latest, err := inst.LatestRevision(ctx, t.modulePath(bi), ref)
	if err != nil {
		return UpdateOutput{}, 2, fmt.Errorf("check latest revision: %w", err)
	}
	if isAhead(bi, latest) && !force {
		return UpdateOutput{}, 2, fmt.Errorf("installed %s is newer than %s (%s); refusing to downgrade, use --force to override", oldRev, ref, latest.Version)
	}
	newRev := latest.Revision
	if newRev == "" {
		newRev = latest.Version
	}

	needsUpdate := oldRev == "" || oldRev == "unknown" || (newRev != "" && !buildMatchesLatest(bi, latest))
	out := UpdateOutput{
		Tool:            t.Name,
		Ref:             ref,
		CheckOnly:       check,
		UpdateAvailable: needsUpdate,
	}

	binDir, err := t.resolveBinDir(!check && needsUpdate)
	if err != nil {
		return UpdateOutput{}, 2, err
	}

	if err := t.applyUpdateTargets(ctx, inst, ref, binDir, oldRev, newRev, !check && needsUpdate, &out); err != nil {
		return UpdateOutput{}, 2, err
	}

	out.StaleSkills = t.findStaleSkills()
	if len(out.StaleSkills) > 0 {
		out.RefreshCommand = t.Name + " skill install --agent all"
	}

	if check && needsUpdate {
		return out, 1, nil
	}
	return out, 0, nil
}

func (t *Tool) resolveBinDir(need bool) (string, error) {
	if !need {
		return "", nil
	}
	exe, err := t.executable()
	if err != nil {
		return "", err
	}
	return filepath.Dir(exe), nil
}

func (t *Tool) applyUpdateTargets(ctx context.Context, inst Installer, ref, binDir, oldRev, newRev string, doInstall bool, out *UpdateOutput) error {
	for _, tgt := range t.updateTargets() {
		bu := BinaryUpdate{
			Name:        tgt.name,
			Package:     tgt.pkg,
			OldRevision: oldRev,
			NewRevision: newRev,
		}
		if doInstall {
			if err := inst.Install(ctx, tgt.pkg, ref, binDir); err != nil {
				return fmt.Errorf("install %s: %w", tgt.name, err)
			}
			bu.Updated = true
			out.Updated = true
			if tgt.daemon != "" {
				out.RestartNeeded = append(out.RestartNeeded, tgt.daemon)
			}
		}
		out.Binaries = append(out.Binaries, bu)
	}
	return nil
}

type updateTarget struct {
	name   string
	pkg    string
	daemon string
}

func (t *Tool) updateTargets() []updateTarget {
	targets := []updateTarget{{
		name:   t.Name,
		pkg:    t.MainPackage,
		daemon: t.DaemonService,
	}}
	for _, c := range t.Companions {
		targets = append(targets, updateTarget{
			name:   c.Name,
			pkg:    c.Package,
			daemon: c.DaemonService,
		})
	}
	return targets
}

func (t *Tool) findStaleSkills() []StaleSkill {
	if t.SkillText == "" && len(t.ExtraSkills) == 0 {
		return nil
	}
	records := t.collectSkillStatusRecords()
	var stale []StaleSkill
	for _, r := range records {
		if r.entry.Status == "stale" {
			stale = append(stale, StaleSkill{
				Agent:         r.entry.Agent,
				Scope:         r.entry.Scope,
				Path:          r.entry.Path,
				InstalledHash: r.entry.InstalledHash,
				CurrentHash:   r.wantHash,
			})
		}
	}
	return stale
}

func printUpdateHuman(w io.Writer, out UpdateOutput) {
	for _, b := range out.Binaries {
		if !out.UpdateAvailable {
			_, _ = fmt.Fprintf(w, "%s: up to date (%s)\n", b.Name, b.OldRevision)
			continue
		}
		state := "current"
		switch {
		case b.Updated:
			state = "updated"
		case out.CheckOnly && out.UpdateAvailable:
			state = "update available"
		}
		_, _ = fmt.Fprintf(w, "%s: %s -> %s (%s)\n", b.Name, b.OldRevision, b.NewRevision, state)
	}
	if len(out.RestartNeeded) > 0 {
		_, _ = fmt.Fprintf(w, "restart needed: %s\n", strings.Join(out.RestartNeeded, ", "))
	}
	if len(out.StaleSkills) > 0 {
		_, _ = fmt.Fprintln(w, "stale skills:")
		for _, s := range out.StaleSkills {
			_, _ = fmt.Fprintf(w, "  %s (%s): %s\n", s.Agent, s.Scope, s.Path)
		}
		_, _ = fmt.Fprintf(w, "refresh with: %s\n", out.RefreshCommand)
	}
}

func currentRevLabel(bi BuildInfo) string {
	if bi.VCSRevision != "" {
		return bi.VCSRevision
	}
	if bi.Version != "" && bi.Version != "(devel)" {
		return bi.Version
	}
	return "unknown"
}

// buildMatchesLatest reports whether the installed build matches the resolved
// revision, comparing commit SHAs (including pseudo-version commit suffixes)
// when available and falling back to tagged versions otherwise.
func buildMatchesLatest(bi BuildInfo, latest RevisionInfo) bool {
	cur := currentRevLabel(bi)
	if cur == "" || cur == "unknown" {
		return false
	}
	rev := latest.Revision
	if rev == "" {
		rev = latest.Version
	}
	if revisionsMatch(cur, rev) {
		return true
	}
	if bi.VCSRevision == "" && bi.Version != "" && bi.Version != "(devel)" && latest.Version != "" {
		if _, ok := pseudoVersionCommit(bi.Version); !ok {
			if _, ok := pseudoVersionCommit(latest.Version); !ok {
				return bi.Version == latest.Version
			}
		}
	}
	return false
}

func revisionsMatch(a, b string) bool {
	if a == "" || b == "" {
		return false
	}
	if a == b {
		return true
	}
	if sha, ok := pseudoVersionCommit(a); ok {
		a = sha
	}
	if sha, ok := pseudoVersionCommit(b); ok {
		b = sha
	}
	if a == b {
		return true
	}
	if len(a) >= 7 && len(b) >= 7 && isHex(a) && isHex(b) {
		return strings.HasPrefix(a, b) || strings.HasPrefix(b, a)
	}
	return false
}

// pseudoVersionCommit extracts the 12-character commit SHA from a Go module
// pseudo-version (e.g. v0.1.1-0.20261123163641-0508ba726c28).
func pseudoVersionCommit(v string) (string, bool) {
	if !strings.HasPrefix(v, "v") {
		return "", false
	}
	v = strings.TrimSuffix(v, "+incompatible")
	lastDash := strings.LastIndexByte(v, '-')
	if lastDash < 0 {
		return "", false
	}
	sha := v[lastDash+1:]
	if len(sha) != 12 || !isHex(sha) {
		return "", false
	}
	prefix := v[:lastDash]
	sep := strings.LastIndexAny(prefix, "-.")
	if sep < 0 {
		return "", false
	}
	ts := prefix[sep+1:]
	if len(ts) != 14 || !isDigits(ts) {
		return "", false
	}
	return sha, true
}

func isHex(s string) bool {
	for i := range len(s) {
		c := s[i]
		isDigit := c >= '0' && c <= '9'
		isLowerHex := c >= 'a' && c <= 'f'
		if !isDigit && !isLowerHex {
			return false
		}
	}
	return len(s) > 0
}

func isDigits(s string) bool {
	for i := range len(s) {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return len(s) > 0
}

// modulePath returns the module that contains the tool's main package.
func (t *Tool) modulePath(bi BuildInfo) string {
	if bi.ModulePath != "" {
		return bi.ModulePath
	}
	return t.MainPackage
}

// isAhead reports whether the installed build is newer than the resolved
// revision, judged by commit times, when the revisions differ.
func isAhead(bi BuildInfo, latest RevisionInfo) bool {
	installed, err := time.Parse(time.RFC3339, bi.BuildTime)
	if err != nil || latest.Time.IsZero() {
		return false
	}
	return !buildMatchesLatest(bi, latest) && installed.After(latest.Time)
}
