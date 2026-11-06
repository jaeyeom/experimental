package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

const circleTokenEnv = "CIRCLE_TOKEN"

func printFailureEvidence(cfg *config.Config, evidence github.FailureEvidence) (string, error) {
	printEvidenceIdentity(evidence)
	if isCircleURL(evidence.TargetURL) && evidence.Log == "" {
		return printCircleCI(cfg, evidence)
	}
	if evidence.Log == "" && evidence.JobURL == "" && evidence.TargetURL != "" {
		fmt.Println("log unavailable: unsupported target")
		return "", nil
	}
	fmt.Printf("Failing step log tail:\n%s\n", tailText(evidence.Log, logTailLines))
	return evidence.Log, nil
}

func printEvidenceIdentity(evidence github.FailureEvidence) {
	switch {
	case evidence.Name != "" && evidence.TargetURL != "":
		fmt.Printf("%s %s\n", evidence.Name, evidence.TargetURL)
	case evidence.Name != "":
		fmt.Println(evidence.Name)
	case evidence.TargetURL != "":
		fmt.Println(evidence.TargetURL)
	}
	if evidence.CheckRunURL != "" {
		fmt.Printf("Check run: %s\n", evidence.CheckRunURL)
	}
	if evidence.JobURL != "" {
		fmt.Printf("Job: %s\n", evidence.JobURL)
	}
}

var circleCLILookPath = exec.LookPath

func printCircleCI(cfg *config.Config, evidence github.FailureEvidence) (string, error) {
	mode := cfg.Settings.CircleCILog
	if mode == "" {
		mode = config.CircleCILogAuto
	}
	token := os.Getenv(circleTokenEnv)
	switch mode {
	case config.CircleCILogAPI:
		if token == "" {
			fmt.Println("log unavailable: no CircleCI token")
			return "", nil
		}
		return printCircleAPILog(cfg, evidence, token)
	case config.CircleCILogCLI:
		if !circleCLIOnPath() {
			fmt.Println("log unavailable: circleci CLI is not on PATH")
			return "", nil
		}
		return printCircleCLILog(cfg, evidence)
	default:
		if token != "" {
			return printCircleAPILog(cfg, evidence, token)
		}
		if circleCLIOnPath() {
			return printCircleCLILog(cfg, evidence)
		}
		fmt.Printf("log unavailable: no CircleCI token and circleci CLI is not on PATH: %s\n", evidence.TargetURL)
		return "", nil
	}
}

func circleCLIOnPath() bool {
	_, err := circleCLILookPath("circleci")
	return err == nil
}

func printCircleAPILog(cfg *config.Config, evidence github.FailureEvidence, token string) (string, error) {
	org, repo, number, ok := circlePipeline(evidence.TargetURL)
	if !ok {
		return printCircleWorkflowByUUID(cfg, evidence)
	}
	body, err := circleStepLog(org, repo, number, token)
	if err != nil {
		return "", fmt.Errorf("circleci log: %w", err)
	}
	return printCircleText(cfg, body)
}

func printCircleWorkflowByUUID(cfg *config.Config, evidence github.FailureEvidence) (string, error) {
	if !circleCLIOnPath() {
		fmt.Printf("%s\nworkflow not found\n", evidence.TargetURL)
		return "", nil
	}
	return printCircleCLILog(cfg, evidence)
}

func printCircleCLILog(cfg *config.Config, evidence github.FailureEvidence) (string, error) {
	workflowID, ok := circleWorkflowID(evidence)
	if !ok {
		fmt.Printf("%s\nworkflow not found\n", evidence.TargetURL)
		return "", nil
	}
	workflowJSON, err := runCommand("circleci", "workflow", "get", workflowID, "--json")
	if err != nil {
		return "", fmt.Errorf("circleci workflow get: %w", err)
	}
	jobID, err := circleFailedJobID(workflowJSON, circleContextJob(evidence.Name))
	if err != nil {
		return "", fmt.Errorf("circleci workflow get: %w", err)
	}
	outputJSON, err := runCommand("circleci", "job", "output", "list", jobID, "--json")
	if err != nil {
		return "", fmt.Errorf("circleci job output list: %w", err)
	}
	text, err := circleFailedStepText(outputJSON)
	if err != nil {
		return "", fmt.Errorf("circleci job output list: %w", err)
	}
	return printCircleText(cfg, text)
}

func printCircleText(cfg *config.Config, body string) (string, error) {
	text := strings.TrimRight(body, "\n")
	fmt.Printf("%s\n", tailText(text, logTailLines))
	pattern := cfg.Settings.FailureLogPattern
	if pattern == "" {
		pattern = config.DefaultFailureLogPattern
	}
	lines, err := linesMatching(text, pattern)
	if err != nil {
		return "", fmt.Errorf("failure_log_pattern %q: %w", pattern, err)
	}
	for _, line := range lines {
		fmt.Println(line)
	}
	return text, nil
}

func circleWorkflowID(evidence github.FailureEvidence) (string, bool) {
	for _, raw := range []string{evidence.TargetURL, evidence.CheckRunURL, evidence.JobURL} {
		if id, ok := workflowUUID(raw); ok {
			return id, true
		}
	}
	return "", false
}

var workflowUUIDPattern = regexp.MustCompile(`/workflows?/([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})`)

func workflowUUID(raw string) (string, bool) {
	if !isCircleURL(raw) {
		return "", false
	}
	match := workflowUUIDPattern.FindStringSubmatch(raw)
	if match == nil {
		return "", false
	}
	return match[1], true
}

func circleContextJob(name string) string {
	const prefix = "ci/circleci: "
	if !strings.HasPrefix(name, prefix) {
		return ""
	}
	return strings.TrimPrefix(name, prefix)
}

func circleFailedJobID(body, jobName string) (string, error) {
	var parsed struct {
		Jobs []struct {
			ID      string `json:"id"`
			Name    string `json:"name"`
			Outcome string `json:"outcome"`
		} `json:"jobs"`
	}
	if err := json.Unmarshal([]byte(body), &parsed); err != nil {
		return "", fmt.Errorf("parse circleci workflow: %w", err)
	}
	for _, job := range parsed.Jobs {
		if !strings.EqualFold(job.Outcome, "failed") || job.ID == "" {
			continue
		}
		if jobName != "" && job.Name != jobName {
			continue
		}
		return job.ID, nil
	}
	return "", errors.New("circleci workflow has no failed job")
}

type circleOutputStep struct {
	Outcome  string `json:"outcome"`
	Output   string `json:"output"`
	ExitCode *int   `json:"exit_code"` //nolint:tagliatelle // CircleCI job output uses snake_case
}

func circleFailedStepText(body string) (string, error) {
	var parsed struct {
		Steps []circleOutputStep `json:"steps"`
	}
	if err := json.Unmarshal([]byte(body), &parsed); err != nil {
		return "", fmt.Errorf("parse circleci job output: %w", err)
	}
	if text := circleStepOutput(parsed.Steps, true); text != "" {
		return text, nil
	}
	return circleStepOutput(parsed.Steps, false), nil
}

func circleStepOutput(steps []circleOutputStep, onlyFailed bool) string {
	var text strings.Builder
	for _, step := range steps {
		if onlyFailed {
			if !circleStepFailed(step) {
				continue
			}
		} else if circleStepSucceeded(step.Outcome) || step.Output == "" {
			continue
		}
		text.WriteString(step.Output)
		if step.Output != "" && !strings.HasSuffix(step.Output, "\n") {
			text.WriteByte('\n')
		}
	}
	return text.String()
}

func circleStepFailed(step circleOutputStep) bool {
	return circleActionFailed(step.Outcome, step.ExitCode)
}

func circleActionFailed(status string, exitCode *int) bool {
	if exitCode != nil && *exitCode != 0 {
		return true
	}
	switch strings.ToLower(status) {
	case "failed", "failure", "error", "errored", "timed_out", "timedout", "infrastructure_fail":
		return true
	default:
		return false
	}
}

func circleStepSucceeded(outcome string) bool {
	switch strings.ToLower(outcome) {
	case "succeeded", "success":
		return true
	default:
		return false
	}
}

func linesMatching(text, pattern string) ([]string, error) {
	re, err := regexp.Compile(pattern)
	if err != nil {
		return nil, fmt.Errorf("compile failure log pattern: %w", err)
	}
	var matched []string
	for _, line := range strings.Split(text, "\n") {
		if line != "" && re.MatchString(line) {
			matched = append(matched, line)
		}
	}
	return matched, nil
}

func isCircleURL(raw string) bool {
	parsed, err := url.Parse(raw)
	if err != nil {
		return false
	}
	host := strings.ToLower(parsed.Hostname())
	return host == "circleci.com" || host == "app.circleci.com" || strings.HasSuffix(host, ".circleci.com")
}

func circlePipeline(raw string) (string, string, int, bool) {
	if !isCircleURL(raw) {
		return "", "", 0, false
	}
	parsed, err := url.Parse(raw)
	if err != nil {
		return "", "", 0, false
	}
	parts := strings.Split(strings.Trim(parsed.Path, "/"), "/")
	if len(parts) >= 5 && parts[0] == "pipelines" && (parts[1] == "github" || parts[1] == "gh") {
		number, err := strconv.Atoi(parts[4])
		if err != nil {
			return "", "", 0, false
		}
		return parts[2], parts[3], number, true
	}
	if len(parts) >= 4 && parts[0] == "gh" {
		number, err := strconv.Atoi(parts[3])
		if err != nil {
			return "", "", 0, false
		}
		return parts[1], parts[2], number, true
	}
	return "", "", 0, false
}

func circleStepLog(org, repo string, number int, token string) (string, error) {
	pipelineID, err := circlePipelineID(org, repo, number, token)
	if err != nil {
		return "", err
	}
	workflowID, err := circleFailedWorkflow(pipelineID, token)
	if err != nil {
		return "", err
	}
	jobNumber, err := circleFailedJob(workflowID, token)
	if err != nil {
		return "", err
	}
	outputURL, err := circleFailedOutput(org, repo, jobNumber, token)
	if err != nil {
		return "", err
	}
	return circleOutputText(outputURL, token)
}

func circlePipelineID(org, repo string, number int, token string) (string, error) {
	raw := fmt.Sprintf(
		"https://circleci.com/api/v2/project/gh/%s/%s/pipeline/%d",
		url.PathEscape(org), url.PathEscape(repo), number,
	)
	var resp struct {
		ID string `json:"id"`
	}
	if err := circleJSON(raw, token, &resp); err != nil {
		return "", err
	}
	if resp.ID == "" {
		return "", errors.New("circleci pipeline response has no id")
	}
	return resp.ID, nil
}

func circleFailedWorkflow(pipelineID, token string) (string, error) {
	raw := "https://circleci.com/api/v2/pipeline/" + url.PathEscape(pipelineID) + "/workflow"
	var resp struct {
		Items []struct {
			ID     string `json:"id"`
			Status string `json:"status"`
		} `json:"items"`
	}
	if err := circleJSON(raw, token, &resp); err != nil {
		return "", err
	}
	for _, item := range resp.Items {
		if strings.EqualFold(item.Status, "failed") && item.ID != "" {
			return item.ID, nil
		}
	}
	return "", errors.New("circleci pipeline has no failed workflow")
}

func circleFailedJob(workflowID, token string) (int, error) {
	raw := "https://circleci.com/api/v2/workflow/" + url.PathEscape(workflowID) + "/job"
	var resp struct {
		Items []struct {
			JobNumber int    `json:"job_number"` //nolint:tagliatelle // CircleCI API uses snake_case
			Status    string `json:"status"`
		} `json:"items"`
	}
	if err := circleJSON(raw, token, &resp); err != nil {
		return 0, err
	}
	for _, item := range resp.Items {
		if strings.EqualFold(item.Status, "failed") && item.JobNumber > 0 {
			return item.JobNumber, nil
		}
	}
	return 0, errors.New("circleci workflow has no failed job")
}

func circleFailedOutput(org, repo string, jobNumber int, token string) (string, error) {
	raw := fmt.Sprintf(
		"https://circleci.com/api/v1.1/project/gh/%s/%s/%d",
		url.PathEscape(org), url.PathEscape(repo), jobNumber,
	)
	var resp struct {
		Steps []struct {
			Actions []struct {
				Status    string `json:"status"`
				ExitCode  *int   `json:"exit_code"`  //nolint:tagliatelle // CircleCI API uses snake_case
				OutputURL string `json:"output_url"` //nolint:tagliatelle // CircleCI API uses snake_case
			} `json:"actions"`
		} `json:"steps"`
	}
	if err := circleJSON(raw, token, &resp); err != nil {
		return "", err
	}
	for _, step := range resp.Steps {
		for _, action := range step.Actions {
			if circleActionFailed(action.Status, action.ExitCode) && action.OutputURL != "" {
				return action.OutputURL, nil
			}
		}
	}
	return "", errors.New("circleci failed job has no step output")
}

func circleOutputText(outputURL, token string) (string, error) {
	var messages []struct {
		Message string `json:"message"`
	}
	if err := circleJSON(outputURL, token, &messages); err != nil {
		return "", err
	}
	var text strings.Builder
	for _, message := range messages {
		text.WriteString(message.Message)
	}
	return text.String(), nil
}

func circleJSON(rawURL, token string, dest any) error {
	body, err := circleGet(context.Background(), rawURL, token)
	if err != nil {
		return err
	}
	if err := json.Unmarshal(body, dest); err != nil {
		return fmt.Errorf("parse circleci response: %w", err)
	}
	return nil
}

func defaultCircleGet(ctx context.Context, rawURL, token string) ([]byte, error) {
	parsed, err := url.Parse(rawURL)
	if err != nil || parsed.Scheme != "https" || parsed.Host == "" {
		return nil, errors.New("circleci url must be https")
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, parsed.String(), nil) //nolint:gosec // G704: URL is the CircleCI API or its step output
	if err != nil {
		return nil, fmt.Errorf("build circleci request: %w", err)
	}
	req.Header.Set("Circle-Token", token)
	client := &http.Client{Timeout: 30 * time.Second}
	//nolint:gosec // G704: URL is the CircleCI API or its step output
	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("circleci request: %w", err)
	}
	defer resp.Body.Close() //nolint:errcheck
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("read circleci response: %w", err)
	}
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return nil, fmt.Errorf("circleci status %d", resp.StatusCode)
	}
	return body, nil
}

var circleGet = defaultCircleGet
