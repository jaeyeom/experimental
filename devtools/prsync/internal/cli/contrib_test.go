package cli

import (
	"bytes"
	"encoding/xml"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func contribDir(t *testing.T) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "..", "..", "contrib"))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools/prsync/contrib"))
	}
	for _, dir := range candidates {
		info, err := os.Stat(dir)
		if err == nil && info.IsDir() {
			return dir
		}
	}
	t.Fatal("contrib directory not found")
	return ""
}

func TestContribSystemdUnitsValid(t *testing.T) {
	dir := contribDir(t)

	loopUnit := parseSystemdUnit(t, filepath.Join(dir, "prsync-loop.service"))
	if loopUnit["Unit"]["Description"] == "" {
		t.Fatal("prsync-loop.service missing Unit.Description")
	}
	svc := loopUnit["Service"]
	if !strings.Contains(svc["ExecStart"], "prsync loop --config") {
		t.Fatalf("prsync-loop.service ExecStart = %q, want prsync loop --config ...", svc["ExecStart"])
	}
	if svc["Restart"] != "on-failure" {
		t.Fatalf("prsync-loop.service Restart = %q, want on-failure", svc["Restart"])
	}
	if svc["TimeoutStopSec"] == "" || !strings.Contains(svc["Environment"], "PATH=") {
		t.Fatalf("prsync-loop.service Service = %+v", svc)
	}
	if loopUnit["Install"]["WantedBy"] != "default.target" {
		t.Fatalf("prsync-loop.service Install = %+v", loopUnit["Install"])
	}

	ensureUnit := parseSystemdUnit(t, filepath.Join(dir, "prsync-loop-ensure.service"))
	if ensureUnit["Unit"]["Description"] == "" {
		t.Fatal("prsync-loop-ensure.service missing Unit.Description")
	}
	ensureSvc := ensureUnit["Service"]
	if ensureSvc["Type"] != "oneshot" {
		t.Fatalf("prsync-loop-ensure.service Type = %q, want oneshot", ensureSvc["Type"])
	}
	if !strings.Contains(ensureSvc["ExecStart"], "prsync loop --ensure --config") {
		t.Fatalf("prsync-loop-ensure.service ExecStart = %q", ensureSvc["ExecStart"])
	}
	if !strings.Contains(ensureSvc["Environment"], "PATH=") {
		t.Fatalf("prsync-loop-ensure.service Environment = %q", ensureSvc["Environment"])
	}

	timerUnit := parseSystemdUnit(t, filepath.Join(dir, "prsync-loop-ensure.timer"))
	if timerUnit["Unit"]["Description"] == "" {
		t.Fatal("prsync-loop-ensure.timer missing Unit.Description")
	}
	timer := timerUnit["Timer"]
	if timer["OnCalendar"] != "hourly" || timer["Persistent"] != "true" {
		t.Fatalf("prsync-loop-ensure.timer Timer = %+v", timer)
	}
	if timerUnit["Install"]["WantedBy"] != "timers.target" {
		t.Fatalf("prsync-loop-ensure.timer Install = %+v", timerUnit["Install"])
	}
}

func TestContribLaunchdPlistsValid(t *testing.T) {
	dir := contribDir(t)

	loopPlist := parsePlistDict(t, filepath.Join(dir, "com.github.jaeyeom.prsync.loop.plist"))
	if loopPlist["Label"] != "com.github.jaeyeom.prsync.loop" {
		t.Fatalf("loop plist Label = %v", loopPlist["Label"])
	}
	args, ok := loopPlist["ProgramArguments"].([]string)
	if !ok || !containsAll(args, "loop", "--config") || containsAll(args, "--ensure") {
		t.Fatalf("loop plist ProgramArguments = %#v", loopPlist["ProgramArguments"])
	}
	if loopPlist["KeepAlive"] != true || loopPlist["RunAtLoad"] != true {
		t.Fatalf("loop plist KeepAlive/RunAtLoad = %v / %v", loopPlist["KeepAlive"], loopPlist["RunAtLoad"])
	}
	env, ok := loopPlist["EnvironmentVariables"].(map[string]any)
	if !ok || env["PATH"] == "" {
		t.Fatalf("loop plist EnvironmentVariables = %#v", loopPlist["EnvironmentVariables"])
	}

	ensurePlist := parsePlistDict(t, filepath.Join(dir, "com.github.jaeyeom.prsync.loop-ensure.plist"))
	if ensurePlist["Label"] != "com.github.jaeyeom.prsync.loop-ensure" {
		t.Fatalf("ensure plist Label = %v", ensurePlist["Label"])
	}
	ensureArgs, ok := ensurePlist["ProgramArguments"].([]string)
	if !ok || !containsAll(ensureArgs, "loop", "--ensure", "--config") {
		t.Fatalf("ensure plist ProgramArguments = %#v", ensurePlist["ProgramArguments"])
	}
	if ensurePlist["StartInterval"] != "3600" || ensurePlist["RunAtLoad"] != true {
		t.Fatalf("ensure plist StartInterval/RunAtLoad = %v / %v", ensurePlist["StartInterval"], ensurePlist["RunAtLoad"])
	}
	ensureEnv, ok := ensurePlist["EnvironmentVariables"].(map[string]any)
	if !ok || ensureEnv["PATH"] == "" {
		t.Fatalf("ensure plist EnvironmentVariables = %#v", ensurePlist["EnvironmentVariables"])
	}
}

func TestContribReadmeDocumentsCronSystemdLaunchd(t *testing.T) {
	data, err := os.ReadFile(filepath.Join(contribDir(t), "README.org")) //nolint:gosec // test fixture in repo contrib dir
	if err != nil {
		t.Fatal(err)
	}
	text := string(data)
	for _, want := range []string{
		"* systemd",
		"* launchd",
		"* crontab",
		"0 * * * *",
		"prsync loop --ensure",
		"prsync doctor",
	} {
		if !strings.Contains(text, want) {
			t.Errorf("contrib/README.org missing %q", want)
		}
	}
}

func parseSystemdUnit(t *testing.T, path string) map[string]map[string]string {
	t.Helper()
	data, err := os.ReadFile(path) //nolint:gosec // test fixture in repo contrib dir
	if err != nil {
		t.Fatal(err)
	}
	sections := map[string]map[string]string{}
	curSection := ""
	for lineNo, raw := range strings.Split(string(data), "\n") {
		line := strings.TrimSpace(raw)
		if line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, ";") {
			continue
		}
		if strings.HasPrefix(line, "[") && strings.HasSuffix(line, "]") {
			curSection = strings.TrimSpace(line[1 : len(line)-1])
			if sections[curSection] == nil {
				sections[curSection] = map[string]string{}
			}
			continue
		}
		if curSection == "" {
			t.Fatalf("%s:%d: setting outside section: %q", path, lineNo+1, line)
		}
		k, v, ok := strings.Cut(line, "=")
		if !ok {
			t.Fatalf("%s:%d: missing '=' in %q", path, lineNo+1, line)
		}
		sections[curSection][strings.TrimSpace(k)] = strings.TrimSpace(v)
	}
	return sections
}

func parsePlistDict(t *testing.T, path string) map[string]any {
	t.Helper()
	data, err := os.ReadFile(path) //nolint:gosec // test fixture in repo contrib dir
	if err != nil {
		t.Fatal(err)
	}
	dec := xml.NewDecoder(bytes.NewReader(data))
	for {
		tok, err := dec.Token()
		if err != nil {
			t.Fatalf("%s: parse plist root: %v", path, err)
		}
		se, ok := tok.(xml.StartElement)
		if !ok || se.Name.Local != "plist" {
			continue
		}
		val, err := decodePlistValue(dec)
		if err != nil {
			t.Fatalf("%s: decode plist body: %v", path, err)
		}
		dict, ok := val.(map[string]any)
		if !ok {
			t.Fatalf("%s: top-level plist value = %T, want dict", path, val)
		}
		return dict
	}
}

func decodePlistValue(dec *xml.Decoder) (any, error) {
	for {
		tok, err := dec.Token()
		if err != nil {
			return nil, fmt.Errorf("read plist token: %w", err)
		}
		se, ok := tok.(xml.StartElement)
		if !ok {
			continue
		}
		return decodePlistElement(dec, se.Name.Local)
	}
}

func decodePlistElement(dec *xml.Decoder, tag string) (any, error) {
	switch tag {
	case "string", "integer":
		var s string
		if err := dec.DecodeElement(&s, &xml.StartElement{Name: xml.Name{Local: tag}}); err != nil {
			return nil, fmt.Errorf("decode plist %s: %w", tag, err)
		}
		return s, nil
	case "true", "false":
		if err := dec.Skip(); err != nil {
			return nil, fmt.Errorf("skip plist %s: %w", tag, err)
		}
		return tag == "true", nil
	case "array":
		var out []string
		for {
			tok, err := dec.Token()
			if err != nil {
				return nil, fmt.Errorf("read plist array token: %w", err)
			}
			switch v := tok.(type) {
			case xml.EndElement:
				if v.Name.Local == "array" {
					return out, nil
				}
			case xml.StartElement:
				elem, err := decodePlistElement(dec, v.Name.Local)
				if err != nil {
					return nil, err
				}
				s, _ := elem.(string)
				out = append(out, s)
			}
		}
	case "dict":
		out := map[string]any{}
		curKey := ""
		haveKey := false
		for {
			tok, err := dec.Token()
			if err != nil {
				return nil, fmt.Errorf("read plist dict token: %w", err)
			}
			switch v := tok.(type) {
			case xml.EndElement:
				if v.Name.Local == "dict" {
					if haveKey {
						return nil, errors.New("dangling plist dict key")
					}
					return out, nil
				}
			case xml.StartElement:
				if !haveKey {
					if v.Name.Local != "key" {
						return nil, errors.New("expected <key> in plist <dict>")
					}
					if err := dec.DecodeElement(&curKey, &v); err != nil {
						return nil, fmt.Errorf("decode plist dict key: %w", err)
					}
					haveKey = true
					continue
				}
				val, err := decodePlistElement(dec, v.Name.Local)
				if err != nil {
					return nil, err
				}
				out[curKey] = val
				haveKey = false
			}
		}
	default:
		if err := dec.Skip(); err != nil && !errors.Is(err, io.EOF) {
			return nil, fmt.Errorf("skip plist %s: %w", tag, err)
		}
		return nil, nil
	}
}

func containsAll(haystack []string, needles ...string) bool {
	set := make(map[string]struct{}, len(haystack))
	for _, s := range haystack {
		set[s] = struct{}{}
	}
	for _, n := range needles {
		if _, ok := set[n]; !ok {
			return false
		}
	}
	return true
}
