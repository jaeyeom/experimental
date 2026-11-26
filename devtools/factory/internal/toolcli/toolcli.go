// Package toolcli implements the shared CLI protocol for agent tools:
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

	executor "github.com/jaeyeom/go-cmdexec"
)

// Companion describes a companion binary installed alongside the main tool.
type Companion struct {
	Name          string
	Package       string
	DaemonService string
}

// BuildInfo describes the compiled binary's version metadata.
type BuildInfo struct {
	ModulePath  string `json:"modulePath"`
	Version     string `json:"version"`
	Commit      string `json:"commit,omitempty"`
	VCSRevision string `json:"vcsRevision"`
	VCSModified bool   `json:"vcsModified"`
	BuildTime   string `json:"buildTime"`
}

// RevisionInfo describes the latest available revision for a Go package or module.
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
	ModulePath        string
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
	Exec          executor.Executor
	HomeDir       func() (string, error)
	WorkDir       func() (string, error)
	Executable    func() (string, error)
	LookPath      func(file string) (string, error)
	NoColor       bool
}

func (t *Tool) buildInfo() BuildInfo {
	if t.ReadBuildInfo != nil {
		return t.ReadBuildInfo()
	}
	return ReadRuntimeBuildInfo("", "")
}

// ReadRuntimeBuildInfo reads version metadata from debug.ReadBuildInfo,
// preferring non-empty ldVersion and ldCommit when supplied.
func ReadRuntimeBuildInfo(ldVersion, ldCommit string) BuildInfo {
	bi, ok := debug.ReadBuildInfo()
	if !ok || bi == nil {
		return BuildInfo{
			Version: ldVersion,
			Commit:  ldCommit,
		}
	}
	info := BuildInfo{
		ModulePath: bi.Main.Path,
		Version:    ldVersion,
		Commit:     ldCommit,
	}
	if info.Version == "" && bi.Main.Version != "" && bi.Main.Version != "(devel)" {
		info.Version = bi.Main.Version
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
	if info.Commit == "" {
		info.Commit = info.VCSRevision
	}
	return info
}

func (t *Tool) installer() Installer {
	if t.Installer != nil {
		return t.Installer
	}
	exec := t.Exec
	if exec == nil {
		exec = executor.NewBasicExecutor()
	}
	return GoInstaller{Exec: exec, ModulePath: t.ModulePath}
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
