package main

import (
	"bytes"
	"strings"
	"testing"
)

func runCLI(args ...string) (code int, stdout, stderr string) {
	var out, errOut bytes.Buffer
	code = run(args, &out, &errOut, []string{"HOME=/nonexistent"}, appRuntime{})
	return code, out.String(), errOut.String()
}

func TestHelpNamesEveryCommandOnStdout(t *testing.T) {
	for _, arg := range []string{"help", "--help", "-h"} {
		code, out, errOut := runCLI(arg)
		if code != 0 || errOut != "" {
			t.Fatalf("%s: exit %d, stderr %q", arg, code, errOut)
		}
		for _, c := range commands() {
			if !strings.Contains(out, "  "+c.name+" ") || !strings.Contains(out, c.summary) {
				t.Errorf("%s: usage lacks %s:\n%s", arg, c.name, out)
			}
		}
		for _, want := range []string{"--config", "--since", "--follow", "config.pkl", "README"} {
			if !strings.Contains(out, want) {
				t.Errorf("%s: usage lacks %q", arg, want)
			}
		}
	}
}

func TestCommandHelpPrintsItsFlags(t *testing.T) {
	flags := map[string][]string{
		"check": {"-since", "-follow", "-json", "-config"},
		"scan":  {"-since", "-until", "-unit", "-json", "-config"},
	}
	for name, want := range flags {
		for _, args := range [][]string{{"help", name}, {name, "--help"}, {name, "-h"}} {
			code, out, errOut := runCLI(args...)
			if code != 0 || errOut != "" {
				t.Fatalf("%v: exit %d, stderr %q", args, code, errOut)
			}
			if !strings.HasPrefix(out, "usage: secretwatch "+name+" ") {
				t.Errorf("%v: %q", args, out)
			}
			for _, f := range want {
				if !strings.Contains(out, f) {
					t.Errorf("%v: lacks %s:\n%s", args, f, out)
				}
			}
		}
	}
}

func TestNoArgsAndUnknownCommandUseStderr(t *testing.T) {
	code, out, errOut := runCLI()
	if code == 0 || out != "" || !strings.Contains(errOut, "usage: secretwatch") {
		t.Fatalf("no args: exit %d, stdout %q, stderr %q", code, out, errOut)
	}
	for _, args := range [][]string{{"bogus"}, {"help", "bogus"}} {
		code, out, errOut = runCLI(args...)
		if code == 0 || out != "" || !strings.Contains(errOut, `unknown command "bogus"`) || !strings.Contains(errOut, "usage: secretwatch") {
			t.Fatalf("%v: exit %d, stdout %q, stderr %q", args, code, out, errOut)
		}
	}
	code, _, errOut = runCLI("scan", "--bogus")
	if code != 2 || !strings.Contains(errOut, "usage: secretwatch scan") {
		t.Fatalf("bad flag: exit %d, stderr %q", code, errOut)
	}
}

func TestDispatchMatchesUsage(t *testing.T) {
	_, usage, _ := runCLI("help")
	for _, c := range commands() {
		if c.summary == "" || c.args == "" || !strings.Contains(usage, "secretwatch "+c.name+" ") {
			t.Errorf("command %s is dispatched but not described in the usage", c.name)
		}
		if code, _, errOut := runCLI(c.name, "--help"); code != 0 || strings.Contains(errOut, "unknown command") {
			t.Errorf("command %s in the usage is not dispatched", c.name)
		}
	}
}
