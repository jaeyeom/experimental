package toolcli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"path/filepath"
	"strings"

	executor "github.com/jaeyeom/go-cmdexec"
)

// GoInstaller installs Go binaries and queries revisions using an injected Executor.
type GoInstaller struct {
	Exec       executor.Executor
	ModulePath string
	LDFlags    func(rev RevisionInfo) string
}

// LatestRevision queries `go list -m -json <module>@<ref>` to resolve the latest revision.
func (g GoInstaller) LatestRevision(ctx context.Context, pkg, ref string) (RevisionInfo, error) {
	if g.Exec == nil {
		return RevisionInfo{}, errors.New("go installer: expected an executor, found nil")
	}
	if ref == "" {
		ref = "latest"
	}
	modPath := g.ModulePath
	if modPath == "" {
		modPath = pkg
	}
	target := modPath + "@" + ref
	res, err := g.Exec.Execute(ctx, executor.ToolConfig{
		Command: "go",
		Args:    []string{"list", "-m", "-json", target},
	})
	if err != nil {
		return RevisionInfo{}, fmt.Errorf("go list %s: %w", target, err)
	}
	if res == nil {
		return RevisionInfo{}, fmt.Errorf("go list %s: expected execution result, found nil", target)
	}
	if res.ExitCode != 0 {
		msg := strings.TrimSpace(res.Stderr)
		if msg == "" {
			return RevisionInfo{}, fmt.Errorf("go list %s: exit %d", target, res.ExitCode)
		}
		return RevisionInfo{}, fmt.Errorf("go list %s: %s", target, msg)
	}
	var mod struct {
		Version string `json:"Version"` //nolint:tagliatelle // go list -m -json uses PascalCase
		Origin  *struct {
			Hash string `json:"Hash"` //nolint:tagliatelle // go list -m -json uses PascalCase
		} `json:"Origin"` //nolint:tagliatelle // go list -m -json uses PascalCase
	}
	if err := json.Unmarshal([]byte(res.Output), &mod); err != nil {
		return RevisionInfo{}, fmt.Errorf("parse go list output: %w", err)
	}
	rev := mod.Version
	if mod.Origin != nil && mod.Origin.Hash != "" {
		rev = mod.Origin.Hash
	}
	return RevisionInfo{Version: mod.Version, Revision: rev}, nil
}

// Install runs `go install <pkg>@<ref>` with GOBIN set to binDir.
func (g GoInstaller) Install(ctx context.Context, pkg, ref, binDir string) error {
	if g.Exec == nil {
		return errors.New("go installer: expected an executor, found nil")
	}
	if ref == "" {
		ref = "latest"
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
	res, err := g.Exec.Execute(ctx, executor.ToolConfig{
		Command: "go",
		Args:    args,
		Env:     map[string]string{"GOBIN": binDir},
	})
	if err != nil {
		return fmt.Errorf("go install %s: %w", target, err)
	}
	if res == nil {
		return fmt.Errorf("go install %s: expected execution result, found nil", target)
	}
	if res.ExitCode != 0 {
		msg := strings.TrimSpace(res.Stderr)
		if msg == "" {
			return fmt.Errorf("go install %s: exit %d", target, res.ExitCode)
		}
		return fmt.Errorf("go install %s: %s", target, msg)
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

// RunUpdate executes the update protocol command and returns (exitCode, error).
// When error is nil and exitCode is non-zero (for example --check with an
// available update), output has already been written to stdout.
func (t *Tool) RunUpdate(ctx context.Context, check bool, ref string, asJSON bool, stdout io.Writer) (int, error) {
	if t.MainPackage == "" {
		return 2, errors.New("tool main package is not configured")
	}
	if ref == "" {
		ref = "latest"
	}
	out, code, err := t.runUpdate(ctx, check, ref)
	if err != nil {
		return 2, err
	}
	if asJSON {
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

func (t *Tool) runUpdate(ctx context.Context, check bool, ref string) (UpdateOutput, int, error) {
	inst := t.installer()
	bi := t.buildInfo()
	oldRev := currentRevLabel(bi)

	latest, err := inst.LatestRevision(ctx, t.MainPackage, ref)
	if err != nil {
		return UpdateOutput{}, 2, fmt.Errorf("check latest revision: %w", err)
	}
	newRev := latest.Revision
	if newRev == "" {
		newRev = latest.Version
	}

	needsUpdate := oldRev == "" || oldRev == "unknown" || (newRev != "" && !revisionsMatch(oldRev, newRev))
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
	if t.SkillText == "" {
		return nil
	}
	entries := t.collectSkillStatus()
	var stale []StaleSkill
	for _, e := range entries {
		if e.Status == "stale" {
			stale = append(stale, StaleSkill{
				Agent:         e.Agent,
				Scope:         e.Scope,
				Path:          e.Path,
				InstalledHash: e.InstalledHash,
				CurrentHash:   SkillHash(t.SkillText),
			})
		}
	}
	return stale
}

func printUpdateHuman(w io.Writer, out UpdateOutput) error {
	for _, b := range out.Binaries {
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
	}
	if len(out.RestartNeeded) > 0 {
		if _, err := fmt.Fprintf(w, "restart needed: %s\n", strings.Join(out.RestartNeeded, ", ")); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	if len(out.StaleSkills) > 0 {
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

func revisionsMatch(a, b string) bool {
	if a == b {
		return true
	}
	if len(a) >= 7 && len(b) >= 7 {
		return strings.HasPrefix(a, b) || strings.HasPrefix(b, a)
	}
	return false
}
