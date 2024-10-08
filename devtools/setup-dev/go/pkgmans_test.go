package main

import (
	"strings"
	"testing"
)

func TestTermux_Check(t *testing.T) {
	t.Setenv("TERMUX_VERSION", "0.0.0")
	e := Termux{}
	if !e.Check() {
		t.Error("Expected true, got false")
	}
}

func TestTermux_Check_False(t *testing.T) {
	t.Setenv("TERMUX_VERSION", "")
	e := Termux{}
	if e.Check() {
		t.Error("Expected false, got true")
	}
}

func TestTermuxPkg_UpdateAllCmd(t *testing.T) {
	p := NewTermuxPkg()
	if !p.Check() {
		t.Skip("Skipping test since Termux is not available")
	}
	cmd := p.UpdateAllCmd()
	if cmd.Err != nil {
		t.Error(cmd.Err)
	}
	if !strings.HasSuffix(cmd.String(), "pkg upgrade -y") {
		t.Errorf("Expected pkg upgrade -y, got %s", cmd.String())
	}
}

func TestTermuxPkg_InstallCmd_emacs(t *testing.T) {
	p := NewTermuxPkg()
	if !p.Check() {
		t.Skip("Skipping test since Termux is not available")
	}
	cmd := p.InstallCmd("emacs")
	if cmd.Err != nil {
		t.Error(cmd.Err)
	}
	if !strings.HasSuffix(cmd.String(), "pkg install -y emacs") {
		t.Errorf("Expected pkg install -y emacs, got %s", cmd.String())
	}
}
