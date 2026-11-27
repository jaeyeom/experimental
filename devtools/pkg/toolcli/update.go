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
	"strconv"
	"strings"
	"time"
)

// DefaultRef is the default revision tools are updated to: they are installed from main.
const DefaultRef = "main"

const defaultRef = DefaultRef

// GoInstaller installs Go binaries and queries revisions using the `go` tool.
type GoInstaller struct {
	// Run executes `go <args>` and returns stdout. Defaults to the real go tool.
	Run func(ctx context.Context, args ...string) ([]byte, error)
	// RunEnv, when non-nil, executes `go <args>` with additional environment
	// variables (such as GOBIN) and returns stdout. Takes precedence over Run.
	RunEnv func(ctx context.Context, env map[string]string, args ...string) ([]byte, error)
	// ModulePath, when non-empty, overrides the module path queried by LatestRevision.
	ModulePath string
	// LDFlags, when non-nil, returns -ldflags value passed to `go install`.
	LDFlags func(rev RevisionInfo) string
}

func (g GoInstaller) run(ctx context.Context, env map[string]string, args ...string) ([]byte, error) {
	if g.RunEnv != nil {
		return g.RunEnv(ctx, env, args...)
	}
	if g.Run != nil {
		return g.Run(ctx, args...)
	}
	cmd := exec.CommandContext(ctx, "go", args...) //nolint:gosec // G204: args come from tool configuration / CLI ref
	if len(env) > 0 {
		cmdEnv := os.Environ()
		for k, v := range env {
			cmdEnv = append(cmdEnv, k+"="+v)
		}
		cmd.Env = cmdEnv
	}
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
// Mutable refs (such as branch names like main) try GOPROXY=direct first to
// avoid stale proxy caches, falling back to the default environment on failure.
func (g GoInstaller) LatestRevision(ctx context.Context, module, ref string) (RevisionInfo, error) {
	if ref == "" {
		ref = defaultRef
	}
	if g.ModulePath != "" {
		module = g.ModulePath
	}
	target := module + "@" + ref
	stdout, err := g.listModuleJSON(ctx, ref, target)
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

func (g GoInstaller) listModuleJSON(ctx context.Context, ref, target string) ([]byte, error) {
	args := []string{"list", "-m", "-json", target}
	if isMutableRef(ref) && (g.RunEnv != nil || g.Run == nil) {
		stdout, err := g.run(ctx, map[string]string{"GOPROXY": "direct"}, args...)
		if err == nil || ctx.Err() != nil {
			return stdout, err
		}
	}
	return g.run(ctx, nil, args...)
}

func isMutableRef(ref string) bool {
	if ref == "" {
		return true
	}
	if _, ok := parseSemver(ref); ok {
		return false
	}
	if len(ref) >= 7 && len(ref) <= 40 && isHex(ref) {
		return false
	}
	return true
}

// Install runs `go install <pkg>@<ref>` with GOBIN set to binDir.
func (g GoInstaller) Install(ctx context.Context, pkg, ref, binDir string) error {
	if ref == "" {
		ref = defaultRef
	}
	target := pkg + "@" + ref
	args := []string{"install"}
	if g.LDFlags != nil {
		if latest, err := g.LatestRevision(ctx, pkg, ref); err == nil {
			if flags := g.LDFlags(latest); flags != "" {
				args = append(args, "-ldflags", flags)
			}
		}
	}
	args = append(args, target)
	if _, err := g.run(ctx, map[string]string{"GOBIN": binDir}, args...); err != nil {
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
	Ahead           bool           `json:"ahead,omitempty"`
	Updated         bool           `json:"updated"`
	CurrentTime     string         `json:"currentTime,omitempty"`
	LatestTime      string         `json:"latestTime,omitempty"`
	Binaries        []BinaryUpdate `json:"binaries"`
	RestartNeeded   []string       `json:"restartNeeded,omitempty"`
	StaleSkills     []StaleSkill   `json:"staleSkills,omitempty"`
	RefreshCommand  string         `json:"refreshCommand,omitempty"`
}

// UpdateOptions configures RunUpdate.
type UpdateOptions struct {
	Check bool
	Force bool
	Ref   string
	JSON  bool
}

// RunUpdate executes the update protocol command and returns (exitCode, error).
// When error is nil and exitCode is non-zero (for example --check with an
// available update), output has already been written to stdout.
func (t *Tool) RunUpdate(ctx context.Context, opts UpdateOptions, stdout io.Writer) (int, error) {
	if t.MainPackage == "" {
		return 2, errors.New("tool main package is not configured")
	}
	ref := opts.Ref
	if ref == "" {
		ref = defaultRef
	}
	out, code, err := t.runUpdate(ctx, opts.Check, opts.Force, ref)
	if err != nil {
		return 2, err
	}

	if opts.JSON {
		if err := writeJSON(stdout, out); err != nil {
			return 2, err
		}
		return code, nil
	}

	if err := printUpdateHuman(stdout, out); err != nil {
		return 2, err
	}
	return code, nil
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

	code, err := t.RunUpdate(ctx, UpdateOptions{
		Check: *check,
		Force: *force,
		Ref:   *ref,
		JSON:  *jsonOut,
	}, stdout)
	if err != nil {
		return failErr(stderr, err, code)
	}
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
	ahead := isAhead(bi, latest)
	if ahead && !check && !force {
		return UpdateOutput{}, 2, fmt.Errorf("installed %s is newer than %s (%s); refusing to downgrade, use --force to override", oldRev, ref, latest.Version)
	}
	newRev := resolvedRevisionLabel(latest)
	needsUpdate := shouldUpdate(bi, oldRev, newRev, latest, ahead, force)
	out := newUpdateOutput(t.Name, ref, check, needsUpdate, ahead && !force, bi, latest)

	doInstall := !check && needsUpdate
	binDir, err := t.resolveBinDir(doInstall)
	if err != nil {
		return UpdateOutput{}, 2, err
	}
	if err := t.applyUpdateTargets(ctx, inst, ref, binDir, oldRev, newRev, doInstall, &out); err != nil {
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

func resolvedRevisionLabel(latest RevisionInfo) string {
	if latest.Revision != "" {
		return latest.Revision
	}
	return latest.Version
}

func shouldUpdate(bi BuildInfo, oldRev, newRev string, latest RevisionInfo, ahead, force bool) bool {
	if ahead && !force {
		return false
	}
	if oldRev == "" || oldRev == "unknown" {
		return true
	}
	return newRev != "" && !buildMatchesLatest(bi, latest)
}

func newUpdateOutput(toolName, ref string, check, needsUpdate, ahead bool, bi BuildInfo, latest RevisionInfo) UpdateOutput {
	out := UpdateOutput{
		Tool:            toolName,
		Ref:             ref,
		CheckOnly:       check,
		UpdateAvailable: needsUpdate,
		Ahead:           ahead,
	}
	if ct, ok := installedCommitTime(bi); ok {
		out.CurrentTime = ct.UTC().Format(time.RFC3339)
	}
	if lt, ok := latestCommitTime(latest); ok {
		out.LatestTime = lt.UTC().Format(time.RFC3339)
	}
	return out
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

func printUpdateHuman(w io.Writer, out UpdateOutput) error {
	for _, b := range out.Binaries {
		if err := printBinaryUpdateHuman(w, out, b); err != nil {
			return err
		}
	}
	if len(out.RestartNeeded) > 0 {
		if _, err := fmt.Fprintf(w, "restart needed: %s\n", strings.Join(out.RestartNeeded, ", ")); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	return printStaleSkillsHuman(w, out)
}

func printBinaryUpdateHuman(w io.Writer, out UpdateOutput, b BinaryUpdate) error {
	if !out.UpdateAvailable {
		if out.Ahead {
			if _, err := fmt.Fprintf(w, "%s: ahead of %s (%s)\n", b.Name, out.Ref, b.OldRevision); err != nil {
				return fmt.Errorf("write output: %w", err)
			}
			return nil
		}
		if _, err := fmt.Fprintf(w, "%s: up to date (%s)\n", b.Name, b.OldRevision); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
		return nil
	}
	state := "current"
	switch {
	case b.Updated:
		state = "updated"
	case out.CheckOnly && out.UpdateAvailable:
		state = "update available"
	}
	if _, err := fmt.Fprintf(w, "%s: %s -> %s (%s)\n", b.Name, b.OldRevision, b.NewRevision, state); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	return nil
}

func printStaleSkillsHuman(w io.Writer, out UpdateOutput) error {
	if len(out.StaleSkills) == 0 {
		return nil
	}
	if _, err := fmt.Fprintln(w, "stale skills:"); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	for _, s := range out.StaleSkills {
		if _, err := fmt.Fprintf(w, "  %s (%s): %s\n", s.Agent, s.Scope, s.Path); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	if _, err := fmt.Fprintf(w, "refresh with: %s\n", out.RefreshCommand); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	return nil
}

func currentRevLabel(bi BuildInfo) string {
	if bi.VCSRevision != "" {
		return bi.VCSRevision
	}
	if bi.Commit != "" && bi.Commit != "unknown" {
		return bi.Commit
	}
	if bi.Version != "" && bi.Version != "(devel)" && bi.Version != "dev" {
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
	sha, _, ok := parsePseudoVersion(v)
	return sha, ok
}

// parsePseudoVersion extracts the 12-character commit SHA and UTC commit
// timestamp from any Go pseudo-version form (vX.0.0-YYYYMMDDHHMMSS-hash,
// vX.Y.Z-0.YYYYMMDDHHMMSS-hash, or vX.Y.Z-pre.0.YYYYMMDDHHMMSS-hash),
// ignoring optional build metadata such as +incompatible or +dirty.
func parsePseudoVersion(v string) (string, time.Time, bool) {
	if !strings.HasPrefix(v, "v") {
		return "", time.Time{}, false
	}
	if plus := strings.IndexByte(v, '+'); plus >= 0 {
		v = v[:plus]
	}
	lastDash := strings.LastIndexByte(v, '-')
	if lastDash < 0 {
		return "", time.Time{}, false
	}
	sha := v[lastDash+1:]
	if len(sha) != 12 || !isHex(sha) {
		return "", time.Time{}, false
	}
	prefix := v[:lastDash]
	sep := strings.LastIndexAny(prefix, "-.")
	if sep < 0 {
		return "", time.Time{}, false
	}
	base := prefix[:sep]
	if prefix[sep] == '-' {
		if strings.ContainsRune(base, '-') {
			return "", time.Time{}, false
		}
	} else if !strings.HasSuffix(base, "-0") && !strings.HasSuffix(base, ".0") {
		return "", time.Time{}, false
	}
	tsStr := prefix[sep+1:]
	if len(tsStr) != 14 || !isDigits(tsStr) {
		return "", time.Time{}, false
	}
	ts, err := time.Parse("20060102150405", tsStr)
	if err != nil {
		return "", time.Time{}, false
	}
	return sha, ts.UTC(), true
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
	if t.ModulePath != "" {
		return t.ModulePath
	}
	return t.MainPackage
}

func installedCommitTime(bi BuildInfo) (time.Time, bool) {
	if bi.BuildTime != "" {
		if parsed, err := time.Parse(time.RFC3339, bi.BuildTime); err == nil {
			return parsed.UTC(), true
		}
	}
	if _, ts, ok := parsePseudoVersion(bi.Version); ok {
		return ts, true
	}
	return time.Time{}, false
}

func latestCommitTime(latest RevisionInfo) (time.Time, bool) {
	if !latest.Time.IsZero() {
		return latest.Time.UTC(), true
	}
	if _, ts, ok := parsePseudoVersion(latest.Version); ok {
		return ts, true
	}
	return time.Time{}, false
}

// isAhead reports whether the installed build is newer than the resolved
// revision, judged by commit times (VCS BuildTime or pseudo-version UTC
// timestamp) when available, or by Go semver precedence otherwise.
func isAhead(bi BuildInfo, latest RevisionInfo) bool {
	if buildMatchesLatest(bi, latest) {
		return false
	}
	if instTime, ok := installedCommitTime(bi); ok {
		if latTime, ok := latestCommitTime(latest); ok {
			return instTime.After(latTime)
		}
	}
	if bi.Version != "" && latest.Version != "" {
		if cmp, ok := compareGoSemver(bi.Version, latest.Version); ok {
			return cmp > 0
		}
	}
	return false
}

type semver struct {
	major int
	minor int
	patch int
	pre   []string
}

func parseSemver(raw string) (semver, bool) {
	if !strings.HasPrefix(raw, "v") {
		return semver{}, false
	}
	mainPart, buildMeta, hasPlus := strings.Cut(raw[1:], "+")
	if hasPlus && !validDotIdents(buildMeta, true) {
		return semver{}, false
	}
	coreStr, preStr, hasPre := strings.Cut(mainPart, "-")
	major, minor, patch, ok := parseSemverCore(coreStr)
	if !ok {
		return semver{}, false
	}
	var pre []string
	if hasPre {
		if !validDotIdents(preStr, false) {
			return semver{}, false
		}
		pre = strings.Split(preStr, ".")
	}
	return semver{major: major, minor: minor, patch: patch, pre: pre}, true
}

func parseSemverCore(coreStr string) (int, int, int, bool) {
	parts := strings.Split(coreStr, ".")
	if len(parts) < 1 || len(parts) > 3 {
		return 0, 0, 0, false
	}
	var nums [3]int
	for i, p := range parts {
		if !isDigits(p) || (len(p) > 1 && p[0] == '0') {
			return 0, 0, 0, false
		}
		n, err := strconv.Atoi(p)
		if err != nil {
			return 0, 0, 0, false
		}
		nums[i] = n
	}
	return nums[0], nums[1], nums[2], true
}

func validDotIdents(s string, allowLeadingZeroNum bool) bool {
	if s == "" {
		return false
	}
	for ident := range strings.SplitSeq(s, ".") {
		if !validIdent(ident) {
			return false
		}
		if !allowLeadingZeroNum && isDigits(ident) && len(ident) > 1 && ident[0] == '0' {
			return false
		}
	}
	return true
}

func validIdent(s string) bool {
	if s == "" {
		return false
	}
	for i := range len(s) {
		c := s[i]
		isAlpha := (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
		isDigit := c >= '0' && c <= '9'
		if !isAlpha && !isDigit && c != '-' {
			return false
		}
	}
	return true
}

func compareGoSemver(aStr, bStr string) (int, bool) {
	a, ok := parseSemver(aStr)
	if !ok {
		return 0, false
	}
	b, ok := parseSemver(bStr)
	if !ok {
		return 0, false
	}
	if a.major != b.major {
		return cmpInt(a.major, b.major), true
	}
	if a.minor != b.minor {
		return cmpInt(a.minor, b.minor), true
	}
	if a.patch != b.patch {
		return cmpInt(a.patch, b.patch), true
	}
	return comparePre(a.pre, b.pre), true
}

func comparePre(a, b []string) int {
	if len(a) == 0 && len(b) == 0 {
		return 0
	}
	if len(a) == 0 {
		return 1
	}
	if len(b) == 0 {
		return -1
	}
	for i := 0; i < len(a) && i < len(b); i++ {
		if c := comparePreIdent(a[i], b[i]); c != 0 {
			return c
		}
	}
	return cmpInt(len(a), len(b))
}

func comparePreIdent(a, b string) int {
	if a == b {
		return 0
	}
	aNum, bNum := isDigits(a), isDigits(b)
	if aNum && bNum {
		if len(a) != len(b) {
			return cmpInt(len(a), len(b))
		}
		return strings.Compare(a, b)
	}
	if aNum {
		return -1
	}
	if bNum {
		return 1
	}
	return strings.Compare(a, b)
}

func cmpInt(a, b int) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}
