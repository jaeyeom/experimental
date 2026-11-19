package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

// fakeAgentbus installs an agentbus that logs its arguments and answers
// jobs show with first, then with later on every show after the first.
func fakeAgentbus(t *testing.T, first, later string) string {
	t.Helper()
	bin := t.TempDir()
	dir := t.TempDir()
	log := filepath.Join(dir, "agentbus.log")
	seen := filepath.Join(dir, "shown")
	script := "#!/bin/sh\nprintf '%s\\n' \"$*\" >> " + log + "\n" +
		"case \"$2\" in show)\n" +
		"  if [ -f " + seen + " ]; then printf '%s\\n' '" + later + "'; else : > " + seen + "; printf '%s\\n' '" + first + "'; fi;;\n" +
		"esac\n"
	if err := os.WriteFile(filepath.Join(bin, "agentbus"), []byte(script), 0o700); err != nil { //nolint:gosec // test fake must be executable
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
	t.Setenv("JOB_RUNNER", "r9")
	return log
}

func runParkJob(t *testing.T, extra ...string) (jobResult, string, string) {
	t.Helper()
	prompts := filepath.Join(t.TempDir(), "prompts.log")
	t.Setenv("HERDR_FAKE_PROMPT_LOG", prompts)
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state.json"),
		append([]string{"wake_before_prompt=off", "dispatch_ask_to=review-manager"}, extra...)...)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--job", "j-1"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d stderr = %s stdout = %s", code, stderr.String(), stdout.String())
	}
	var res jobResult
	if err := json.Unmarshal(stdout.Bytes(), &res); err != nil {
		t.Fatal(err)
	}
	raw, err := os.ReadFile(prompts)
	if err != nil {
		t.Fatal(err)
	}
	return res, string(raw), stderr.String()
}

const commentJobRunning = `{"id":"j-1","kind":"comment","subject":"acme/widgets#123","head":"abc123def456","state":"running","needs":{"machine":"local","affinity":"tab","executor":"tab"}}`

func TestDispatchJobInParkModeAsksAndLeavesAParkedJobUnreported(t *testing.T) {
	parked := strings.Replace(commentJobRunning, `"running"`, `"done"`, 1)
	log := fakeAgentbus(t, commentJobRunning, parked)
	res, prompt, _ := runParkJob(t, "dispatch_park_mode=on")
	for _, want := range []string{"agentbus send review-manager", "agentbus jobs park j-1 --runner r9 --ask-msg"} {
		if !strings.Contains(prompt, want) {
			t.Errorf("prompt lacks %q:\n%s", want, prompt)
		}
	}
	if res.Result != jobParked {
		t.Fatalf("res = %+v, want parked", res)
	}
	raw, err := os.ReadFile(log)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(raw), "jobs done") || strings.Contains(string(raw), "jobs fail") {
		t.Fatalf("reported a job the agent parked:\n%s", raw)
	}
}

func TestDispatchJobResumePromptCarriesTheAnswer(t *testing.T) {
	resumed := `{"id":"j-1","kind":"comment","subject":"acme/widgets#123","head":"abc123def456","state":"running","saved":"park/j-1",` +
		`"answer":"Keep the old API.","items":["PRRC_x"],"needs":{"machine":"local","affinity":"tab","executor":"tab"}}`
	log := fakeAgentbus(t, resumed, resumed)
	res, prompt, _ := runParkJob(t, "dispatch_park_mode=on")
	if !strings.Contains(prompt, "Keep the old API.") || !strings.Contains(prompt, "park/j-1") || strings.Contains(prompt, "agentbus jobs park") {
		t.Fatalf("resume prompt:\n%s", prompt)
	}
	raw, err := os.ReadFile(log)
	if err != nil {
		t.Fatal(err)
	}
	if res.Result != jobDone || !strings.Contains(string(raw), "jobs done j-1 --runner r9") {
		t.Fatalf("res = %+v, log:\n%s", res, raw)
	}
}
