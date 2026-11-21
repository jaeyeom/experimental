// Package toolcli implements the standard CLI protocol for devtools binaries:
// version, update, doctor, and skill install/uninstall/status.
package toolcli

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime/debug"
	"strings"

	"github.com/jaeyeom/experimental/devtools/pkg/version"
)

// Companion describes a companion binary installed alongside the main tool.
type Companion struct {
	Name    string
	Package string
	// DaemonService, when non-empty, names the daemon or service that must be
	// restarted after updating this binary.
	DaemonService string
}

// BuildInfo describes the compiled binary's version metadata.
type BuildInfo struct {
	ModulePath  string `json:"modulePath"`
	Version     string `json:"version"`
	VCSRevision string `json:"vcsRevision"`
	VCSModified bool   `json:"vcsModified"`
	BuildTime   string `json:"buildTime"`
}

// RevisionInfo describes the latest available revision for a Go package.
type RevisionInfo struct {
	Version  string
	Revision string
}

// Installer abstracts remote revision lookup and binary installation.
type Installer interface {
	LatestRevision(ctx context.Context, pkg, ref string) (RevisionInfo, error)
	Install(ctx context.Context, pkg, ref, binDir string) error
}

// CheckStatus is the outcome of a single doctor check.
type CheckStatus string

const (
	// StatusOK indicates the check passed.
	StatusOK CheckStatus = "ok"
	// StatusWarn indicates a non-fatal warning.
	StatusWarn CheckStatus = "warn"
	// StatusFail indicates a failure.
	StatusFail CheckStatus = "fail"
)

// CheckResult is the outcome of a single doctor check.
type CheckResult struct {
	Name   string      `json:"name"`
	Status CheckStatus `json:"status"`
	Detail string      `json:"detail"`
	Hint   string      `json:"hint,omitempty"`
}

// DoctorCheck is a tool-specific diagnostic function.
type DoctorCheck func(ctx context.Context, offline bool) CheckResult

// Tool configures the standard CLI protocol for a single tool binary.
type Tool struct {
	Name              string
	MainPackage       string
	DaemonService     string
	Companions        []Companion
	SkillText         string
	ConfigPath        func() (string, bool)
	CheckConfig       func() error
	RequiredCommands  []string
	ExtraDoctorChecks []DoctorCheck

	// Optional dependency-injection hooks (default to OS/runtime implementations).
	ReadBuildInfo func() BuildInfo
	Installer     Installer
	HomeDir       func() (string, error)
	WorkDir       func() (string, error)
	Executable    func() (string, error)
	LookPath      func(file string) (string, error)
}

// Handle dispatches args if args[0] is one of the standard protocol commands
// (version, update, doctor, skill). It returns (exitCode, true) when handled,
// or (0, false) otherwise.
func (t Tool) Handle(ctx context.Context, args []string, stdout, stderr io.Writer) (int, bool) {
	if len(args) == 0 {
		return 0, false
	}
	switch args[0] {
	case "version":
		return t.cmdVersion(args[1:], stdout, stderr), true
	case "update":
		return t.cmdUpdate(ctx, args[1:], stdout, stderr), true
	case "doctor":
		return t.cmdDoctor(ctx, args[1:], stdout, stderr), true
	case "skill":
		return t.cmdSkill(args[1:], stdout, stderr), true
	default:
		return 0, false
	}
}

func (t *Tool) buildInfo() BuildInfo {
	if t.ReadBuildInfo != nil {
		return t.ReadBuildInfo()
	}
	return readRuntimeBuildInfo()
}

func readRuntimeBuildInfo() BuildInfo {
	bi, ok := debug.ReadBuildInfo()
	if !ok {
		return BuildInfo{Version: version.Resolve()}
	}
	info := BuildInfo{
		ModulePath: bi.Main.Path,
		Version:    version.Resolve(),
	}
	for _, s := range bi.Settings {
		switch s.Key {
		case "vcs.revision":
			info.VCSRevision = s.Value
		case "vcs.time":
			info.BuildTime = s.Value
		case "vcs.modified":
			info.VCSModified = s.Value == "true"
		}
	}
	return info
}

func (t *Tool) installer() Installer {
	if t.Installer != nil {
		return t.Installer
	}
	return GoInstaller{}
}

func (t *Tool) homeDir() (string, error) {
	if t.HomeDir != nil {
		return t.HomeDir()
	}
	dir, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("resolve home directory: %w", err)
	}
	return dir, nil
}

func (t *Tool) workDir() (string, error) {
	if t.WorkDir != nil {
		return t.WorkDir()
	}
	dir, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("resolve working directory: %w", err)
	}
	return dir, nil
}

func (t *Tool) executable() (string, error) {
	if t.Executable != nil {
		return t.Executable()
	}
	exe, err := os.Executable()
	if err != nil {
		return "", fmt.Errorf("resolve executable path: %w", err)
	}
	if resolved, err := filepath.EvalSymlinks(exe); err == nil {
		return resolved, nil
	}
	return exe, nil
}

func (t *Tool) lookPath(file string) (string, error) {
	if t.LookPath != nil {
		return t.LookPath(file)
	}
	p, err := exec.LookPath(file)
	if err != nil {
		return "", fmt.Errorf("look up %s on PATH: %w", file, err)
	}
	return p, nil
}

func writeJSON(w io.Writer, v any) error {
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	if err := enc.Encode(v); err != nil {
		return fmt.Errorf("encode json: %w", err)
	}
	return nil
}

func failMsg(stderr io.Writer, msg string, code int) int {
	_, _ = fmt.Fprintln(stderr, strings.TrimRight(msg, "\n"))
	return code
}

func failErr(stderr io.Writer, err error, code int) int {
	_, _ = fmt.Fprintf(stderr, "error: %v\n", err)
	return code
}
