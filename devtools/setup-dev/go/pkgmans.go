package main

import (
	"errors"
	"log"
	"os"
	"os/exec"
)

// Termux is a struct that represents the Termux environment.
type Termux struct{}

// Check TERMUX_VERSION environment variable. If it exists, return true.
func (t *Termux) Check() bool {
	return os.Getenv("TERMUX_VERSION") != ""
}

// TermuxPkg is a struct that represents the Termux package manager.
type TermuxPkg struct {
	PkgMap map[string]string
}

// NewTermuxPkg returns a new TermuxPkg with the proper package mapping.
func NewTermuxPkg() *TermuxPkg {
	return &TermuxPkg{
		PkgMap: map[string]string{
			"ag":      "silversearcher-ag",
			"rg":      "ripgrep",
			"go":      "golang",
			"gpg":     "gnupg",
			"locate":  "mlocate",
			"ssh":     "openssh",
			"python3": "python",
			"pip":     "python-pip",
			"pip3":    "python-pip",
			"node":    "nodejs",
			"protoc":  "protobuf",
		},
	}
}

// Check checks if the Termux environment is available.
func (p *TermuxPkg) Check() bool {
	t := Termux{}
	return t.Check()
}

// UpdateAllCmd returns the command to update the Termux environment.
func (p *TermuxPkg) UpdateAllCmd() *exec.Cmd {
	return exec.Command("pkg", "upgrade", "-y")
}

// InstallCmd returns the command to install a package in the Termux environment.
func (p *TermuxPkg) InstallCmd(pkg string) *exec.Cmd {
	if _, ok := p.PkgMap[pkg]; ok {
		pkg = p.PkgMap[pkg]
	}
	return exec.Command("pkg", "install", "-y", pkg)
}

// PackageManager is an interface that defines the methods for a package manager.
type PackageManager interface {
	InstallCmd(pkg string) *exec.Cmd
	UpdateAllCmd() *exec.Cmd
}

// FindPkgManagers returns a list of package managers available in the
// environment.
func FindPkgManagers() []PackageManager {
	var pkgMans []PackageManager
	tp := NewTermuxPkg()
	if tp.Check() {
		pkgMans = append(pkgMans, tp)
	}
	return pkgMans
}

// PackageManagers is a struct that represents a list of package managers.
type PackageManagers struct {
	PkgManagers []PackageManager
	Runner      func(cmd *exec.Cmd) error
}

// NewPackageManagers returns a new PackageManagers with the available package
// managers.
func NewPackageManagers() *PackageManagers {
	return &PackageManagers{
		PkgManagers: FindPkgManagers(),
		Runner:      Run,
	}
}

var ErrNoPackageManager = errors.New("no package manager available")

func (pms PackageManagers) Install(pkg string) func() error {
	return func() error {
		log.Println("Install", pkg)
		for _, pm := range pms.PkgManagers {
			cmd := pm.InstallCmd(pkg)
			err := Run(cmd)
			if err == nil {
				return nil
			}
		}
		return ErrNoPackageManager
	}
}

func (pms PackageManagers) UpdateAll() error {
	for _, pm := range pms.PkgManagers {
		cmd := pm.UpdateAllCmd()
		err := Run(cmd)
		if err == nil {
			return nil
		}
	}
	return ErrNoPackageManager
}
