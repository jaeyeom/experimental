package genfiles

import (
	"strings"
	"testing"
)

const exampleList = `# pnpm lock files, one per package.
- pattern: "**/pnpm-lock.yaml"
  comment: package.json next to it.
  dir: {var: dir}
  run:
    - [pnpm, install, --lockfile-only]

- pattern: Cargo.lock
  env: {CARGO_BAZEL_REPIN: "1"}
  run:
    - [bazel, mod, deps]
    - [bazel, mod, deps]

- pattern: "tools/sim/urdf/*.urdf"
  run:
    - [bazel, run, "//tools/sim:gen", --, {var: path},
       {join: [tools/sim/xacro/, {var: stem}, .xacro]}]

- pattern: vendor/proto/*_pb2.py
  comment: Hand-run protoc 3.20.

- pattern: x.lock
  run:
    - [sh, -c, 'gen "$1"', sh, {var: path}]
`

func TestParseExample(t *testing.T) {
	entries, err := Parse([]byte(exampleList))
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if len(entries) != 5 {
		t.Fatalf("len = %d, want 5", len(entries))
	}
	pnpm := entries[0]
	if pnpm.Pattern != "**/pnpm-lock.yaml" || pnpm.Comment != "package.json next to it." || pnpm.Line != 2 {
		t.Errorf("pnpm entry = %+v", pnpm)
	}
	if pnpm.Dir == nil || pnpm.Dir.Var != "dir" {
		t.Errorf("pnpm dir = %+v", pnpm.Dir)
	}
	if len(pnpm.Run) != 1 || len(pnpm.Run[0]) != 3 || pnpm.Run[0][2].Lit != "--lockfile-only" {
		t.Errorf("pnpm run = %+v", pnpm.Run)
	}
	cargo := entries[1]
	if len(cargo.Env) != 1 || cargo.Env[0].Name != "CARGO_BAZEL_REPIN" || cargo.Env[0].Value.Lit != "1" || len(cargo.Run) != 2 {
		t.Errorf("cargo entry = %+v", cargo)
	}
	urdf := entries[2].Run[0]
	var args []string
	for _, v := range urdf {
		args = append(args, v.Expand("tools/sim/urdf/body.urdf"))
	}
	want := "bazel run //tools/sim:gen -- tools/sim/urdf/body.urdf tools/sim/xacro/body.xacro"
	if strings.Join(args, " ") != want {
		t.Errorf("urdf argv = %q, want %q", strings.Join(args, " "), want)
	}
	if entries[3].Run != nil {
		t.Errorf("entry without run has Run = %+v", entries[3].Run)
	}
}

func TestParseErrors(t *testing.T) {
	tests := []struct {
		name, yaml, want string
	}{
		{"not a list", "pattern: x\n", "line 1: expected a list of entries"},
		{"empty file", "", "expected a list of entries"},
		{"entry not mapping", "- x\n", "line 1: entry 1: expected a mapping"},
		{"unknown key", "- pattern: x\n  regenerate: make\n", `line 2: entry 1: unknown key "regenerate"`},
		{"missing pattern", "- comment: x\n", `line 1: entry 1: missing required key "pattern"`},
		{"bad pattern", "- pattern: \"a/[x\"\n", "line 1: entry 1: pattern"},
		{"duplicate pattern", "- pattern: x\n- pattern: x\n", `line 2: entry 2: duplicate pattern "x"`},
		{"int in argv", "- pattern: x\n  run: [[gen, --count, 10]]\n", "line 2: entry 1: run[0][2]: expected a string or a {var}/{join} mapping, found int"},
		{"bool coerced", "- pattern: x\n  run: [[gen, true]]\n", "run[0][1]: expected a string or a {var}/{join} mapping, found bool"},
		{"null", "- pattern: x\n  run: [[gen, ~]]\n", "found null"},
		{"float", "- pattern: x\n  run: [[gen, 1.0]]\n", "found float"},
		{"alias", "- pattern: &a x\n- pattern: *a\n", "line 2: entry 2: pattern: aliases are not supported"},
		{"custom tag", "- pattern: x\n  run: [[gen, !tpl \"{path}\"]]\n", `custom tag "!tpl" is not supported`},
		{"two keys", "- pattern: x\n  run: [[gen, {var: path, join: []}]]\n", "run[0][1]: a value mapping has exactly one key, var or join"},
		{"other key", "- pattern: x\n  run: [[gen, {tpl: path}]]\n", `run[0][1]: unknown value key "tpl"; expected var or join`},
		{"unknown var", "- pattern: x\n  run: [[gen, {var: pth}]]\n", `run[0][1]: unknown var "pth"; expected one of path, dir, name, stem`},
		{"comment not string", "- pattern: x\n  comment: 3\n", "comment: expected a string, found int"},
		{"run not list", "- pattern: x\n  run: gen\n", "run: expected a list of commands"},
		{"empty run", "- pattern: x\n  run: []\n", "run: must not be empty; omit run for a file with no command"},
		{"command not list", "- pattern: x\n  run: [gen]\n", "run[0]: expected a list of arguments"},
		{"empty command", "- pattern: x\n  run: [[]]\n", "run[0]: a command must not be empty"},
		{"program from file", "- pattern: x\n  run: [[{var: path}]]\n", "run[0][0]: the program must be a literal string"},
		{"sh -c var", "- pattern: x\n  run: [[sh, -c, {var: path}]]\n", "run[0][2]: the script after -c must be a literal string; pass file values as positional parameters"},
		{"bash -ec join", "- pattern: x\n  run: [[/bin/bash, -ec, {join: [a, {var: path}]}]]\n", "run[0][2]: the script after -ec must be a literal string"},
		{"dir without run", "- pattern: x\n  dir: a\n", "entry 1: dir and env need run"},
		{"env without run", "- pattern: x\n  env: {A: b}\n", "entry 1: dir and env need run"},
		{"bad env name", "- pattern: x\n  env: {1A: b}\n  run: [[gen]]\n", `env: invalid variable name "1A"`},
		{"env not mapping", "- pattern: x\n  env: [a]\n  run: [[gen]]\n", "env: expected a mapping"},
		{"absolute dir", "- pattern: x\n  dir: /tmp\n  run: [[gen]]\n", `dir: "/tmp" must be relative to the repository root`},
		{"escaping dir", "- pattern: x\n  dir: a/../..\n  run: [[gen]]\n", `dir: "a/../.." must stay inside the repository`},
		{"tagged run", "- pattern: x\n  run: !foo [[gen]]\n", `run: custom tag "!foo" is not supported`},
		{"tagged command", "- pattern: x\n  run: [!foo [gen]]\n", `run[0]: custom tag "!foo" is not supported`},
		{"tagged mapping value", "- pattern: x\n  run: [[gen, !foo {var: path}]]\n", `run[0][1]: custom tag "!foo" is not supported`},
		{"tagged env", "- pattern: x\n  env: !foo {A: b}\n  run: [[gen]]\n", `env: custom tag "!foo" is not supported`},
		{"tagged entry", "- !foo {pattern: x}\n", `entry 1: custom tag "!foo" is not supported`},
		{"join not list", "- pattern: x\n  run: [[gen, {join: a}]]\n", "run[0][1]: join: expected a list of values"},
		{"sh -c -- var", "- pattern: x\n  run: [[bash, -c, --, {var: path}]]\n", "run[0][3]: the script after -c must be a literal string"},
		{"sh -c -e var", "- pattern: x\n  run: [[bash, -c, -e, {var: path}]]\n", "run[0][3]: the script after -c must be a literal string"},
		{"sh -c -o opt var", "- pattern: x\n  run: [[bash, -c, -o, errexit, {var: path}]]\n", "run[0][4]: the script after -c must be a literal string"},
		{"sh -c var option", "- pattern: x\n  run: [[sh, -c, -o, {var: path}, 'echo hi']]\n", "run[0][3]: the script after -c must be a literal string"},
		{"duplicate key", "- pattern: a\n  pattern: b\n", `line 2: entry 1: duplicate key "pattern"`},
		{"duplicate run", "- pattern: a\n  run: [[x]]\n  run: [[y]]\n", `duplicate key "run"`},
		{"duplicate env", "- pattern: a\n  env: {A: b, A: c}\n  run: [[x]]\n", `env: duplicate variable "A"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := Parse([]byte(tt.yaml))
			if err == nil {
				t.Fatalf("Parse succeeded, want error containing %q", tt.want)
			}
			if !strings.Contains(err.Error(), tt.want) {
				t.Errorf("error = %q, want it to contain %q", err, tt.want)
			}
		})
	}
}

func TestParseReportsAllProblems(t *testing.T) {
	_, err := Parse([]byte("- pattern: x\n  foo: 1\n- pattern: y\n  bar: 2\n"))
	if err == nil || !strings.Contains(err.Error(), `"foo"`) || !strings.Contains(err.Error(), `"bar"`) {
		t.Errorf("error = %v, want both unknown keys", err)
	}
}

func TestParsePatterns(t *testing.T) {
	// The older shape (a regenerate string and extra keys) still reads.
	old := `
- pattern: "**/*.pb.go"
  regenerate: "make proto"
  bazel_generated: true
  source: "proto/"
- pattern: "**/zz_generated.*.go"
  owner: "codegen"
`
	got, err := ParsePatterns([]byte(old))
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(got, ",") != "**/*.pb.go,**/zz_generated.*.go" {
		t.Errorf("patterns = %v", got)
	}
	if _, err := ParsePatterns([]byte(exampleList)); err != nil {
		t.Errorf("ParsePatterns(example) = %v", err)
	}
	for _, bad := range []string{
		"- regenerate: make proto\n",
		"- pattern: \"a/[\"\n",
		"- pattern: 3\n",
		"not: [valid: yaml",
		"generated_files:\n  - pattern: x\n",
	} {
		if _, err := ParsePatterns([]byte(bad)); err == nil {
			t.Errorf("ParsePatterns(%q) = nil error", bad)
		}
	}
}

func TestParseShellScriptPositionalAllowed(t *testing.T) {
	for _, y := range []string{
		"- pattern: x\n  run: [[bash, -c, --, 'gen \"$1\"', sh, {var: path}]]\n",
		"- pattern: x\n  run: [[bash, -e, -c, 'gen \"$1\"', sh, {var: path}]]\n",
		"- pattern: x\n  run: [[bash, -c, -o, errexit, 'gen \"$1\"', sh, {var: path}]]\n",
	} {
		if _, err := Parse([]byte(y)); err != nil {
			t.Errorf("Parse(%q) = %v", y, err)
		}
	}
}

func TestParsePatternsDuplicatePattern(t *testing.T) {
	if _, err := ParsePatterns([]byte("- pattern: a\n  pattern: b\n")); err == nil || !strings.Contains(err.Error(), `duplicate key "pattern"`) {
		t.Errorf("err = %v", err)
	}
}

func TestParseRunEmptyWithDirReportsOnce(t *testing.T) {
	_, err := Parse([]byte("- pattern: x\n  run: []\n  dir: a\n"))
	if err == nil || strings.Contains(err.Error(), "dir and env need run") {
		t.Errorf("err = %v, want only the empty-run error", err)
	}
}

func TestParsePatternsEmptyList(t *testing.T) {
	for _, y := range []string{"", "# only comments\n", "[]\n"} {
		got, err := ParsePatterns([]byte(y))
		if err != nil || len(got) != 0 {
			t.Errorf("ParsePatterns(%q) = %v, %v; want no patterns, no error", y, got, err)
		}
	}
}
