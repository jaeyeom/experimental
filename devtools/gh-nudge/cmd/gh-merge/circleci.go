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

// failureReport is the evidence text printed for a failure. Suggested is an
// RCA class the evidence points to, such as "infra", or empty.
type failureReport struct {
	Text      string
	Suggested string
}

func printFailureEvidence(cfg *config.Config, evidence github.FailureEvidence) (failureReport, error) {
	printEvidenceIdentity(evidence)
	if isCircleURL(evidence.TargetURL) && evidence.Log == "" {
		return printCircleCI(cfg, evidence)
	}
	if evidence.Log == "" && evidence.JobURL == "" && evidence.TargetURL != "" {
		fmt.Println("log unavailable: unsupported target")
		return failureReport{}, nil
	}
	fmt.Printf("Failing step log tail:\n%s\n", tailText(evidence.Log, logTailLines))
	return failureReport{Text: evidence.Log}, nil
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

func printCircleCI(cfg *config.Config, evidence github.FailureEvidence) (failureReport, error) {
	mode := cfg.Settings.CircleCILog
	if mode == "" {
		mode = config.CircleCILogAuto
	}
	token := os.Getenv(circleTokenEnv)
	switch mode {
	case config.CircleCILogAPI:
		if token == "" {
			fmt.Println("log unavailable: no CircleCI token")
			return failureReport{}, nil
		}
		return printCircleAPILog(cfg, evidence, token)
	case config.CircleCILogCLI:
		if !circleCLIOnPath() {
			fmt.Println("log unavailable: circleci CLI is not on PATH")
			return failureReport{}, nil
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
		return failureReport{}, nil
	}
}

func circleCLIOnPath() bool {
	_, err := circleCLILookPath("circleci")
	return err == nil
}

func printCircleAPILog(cfg *config.Config, evidence github.FailureEvidence, token string) (failureReport, error) {
	if org, repo, jobNumber, ok := circleJobNumber(evidence.TargetURL); ok {
		log, err := circleJobLog(org, repo, jobNumber, token)
		return finishCircleAPILog(cfg, evidence, log, err)
	}
	org, repo, number, ok := circlePipeline(evidence.TargetURL)
	if !ok {
		return printCircleWorkflowByUUID(cfg, evidence)
	}
	log, err := circleStepLog(org, repo, number, token, evidence.Name)
	return finishCircleAPILog(cfg, evidence, log, err)
}

func finishCircleAPILog(cfg *config.Config, evidence github.FailureEvidence, log circleLog, err error) (failureReport, error) {
	if err != nil {
		var missing *noFailedJobError
		if errors.As(err, &missing) {
			printNoFailedJob(missing.workflow, evidence.Statuses)
			return failureReport{}, nil
		}
		return failureReport{}, fmt.Errorf("circleci log: %w", err)
	}
	return printCircleLog(cfg, log)
}

func printCircleWorkflowByUUID(cfg *config.Config, evidence github.FailureEvidence) (failureReport, error) {
	if !circleCLIOnPath() {
		fmt.Printf("%s\nworkflow not found\n", evidence.TargetURL)
		return failureReport{}, nil
	}
	return printCircleCLILog(cfg, evidence)
}

func printCircleCLILog(cfg *config.Config, evidence github.FailureEvidence) (failureReport, error) {
	workflowID, ok := circleWorkflowID(evidence)
	if !ok {
		fmt.Printf("%s\nworkflow not found\n", evidence.TargetURL)
		return failureReport{}, nil
	}
	workflowJSON, err := runCommand("circleci", "workflow", "get", workflowID, "--json")
	if err != nil {
		return failureReport{}, fmt.Errorf("circleci workflow get: %w", err)
	}
	jobID, err := circleFailedJobID(workflowJSON, circleContextJob(evidence.Name))
	if errors.Is(err, errNoFailedJob) {
		printNoFailedJob(workflowID, evidence.Statuses)
		return failureReport{}, nil
	}
	if err != nil {
		return failureReport{}, fmt.Errorf("circleci workflow get: %w", err)
	}
	jobJSON, err := runCommand("circleci", "job", "get", jobID, "--json")
	if err != nil {
		return failureReport{}, fmt.Errorf("circleci job get: %w", err)
	}
	log, err := circleCLIJob(jobJSON)
	if err != nil {
		return failureReport{}, fmt.Errorf("circleci job get: %w", err)
	}
	log.jobURL = evidence.TargetURL
	outputJSON, err := runCommand("circleci", "job", "output", "list", jobID, "--json")
	if err != nil {
		return failureReport{}, fmt.Errorf("circleci job output list: %w", err)
	}
	log.text, err = circleFailedStepText(outputJSON, !log.infra)
	if err != nil {
		return failureReport{}, fmt.Errorf("circleci job output list: %w", err)
	}
	return printCircleLog(cfg, log)
}

// circleLog is a failed CircleCI job's step log. For an infrastructure
// failure, infra is set, reason is CircleCI's message, and text is only the
// log of a step that actually failed.
type circleLog struct {
	text   string
	infra  bool
	reason string
	jobURL string
}

const rcaClassInfra = "infra"

func printCircleLog(cfg *config.Config, log circleLog) (failureReport, error) {
	if !log.infra {
		return printCircleText(cfg, log.text)
	}
	reason := log.reason
	if reason == "" {
		reason = "no reason given"
	}
	header := fmt.Sprintf("infrastructure failure: %s\nJob: %s\nSuggested RCA class: %s", reason, log.jobURL, rcaClassInfra)
	fmt.Println(header)
	report := failureReport{Text: header, Suggested: rcaClassInfra}
	if log.text == "" {
		return report, nil
	}
	tail, err := printCircleText(cfg, log.text)
	if err != nil {
		return failureReport{}, err
	}
	report.Text += "\n" + tail.Text
	return report, nil
}

// circleInfraStatus reports whether a CircleCI job status is an
// infrastructure failure, in any case and with "_" or "-".
func circleInfraStatus(status string) bool {
	return strings.ReplaceAll(strings.ToLower(status), "-", "_") == "infrastructure_fail"
}

// circleJobFailed reports whether a CircleCI job status or outcome is one
// whose evidence gh-merge prints.
func circleJobFailed(status string) bool {
	return strings.EqualFold(status, "failed") || circleInfraStatus(status)
}

type circleMessage struct {
	Message string `json:"message"`
}

func circleReason(messages []circleMessage) string {
	var parts []string
	for _, message := range messages {
		if text := strings.TrimSpace(message.Message); text != "" {
			parts = append(parts, text)
		}
	}
	return strings.Join(parts, "; ")
}

func circleCLIJob(body string) (circleLog, error) {
	var parsed struct {
		Status   string          `json:"status"`
		Warnings []circleMessage `json:"warnings"`
	}
	if err := json.Unmarshal([]byte(body), &parsed); err != nil {
		return circleLog{}, fmt.Errorf("parse circleci job: %w", err)
	}
	if !circleInfraStatus(parsed.Status) {
		return circleLog{}, nil
	}
	return circleLog{infra: true, reason: circleReason(parsed.Warnings)}, nil
}

func printCircleText(cfg *config.Config, body string) (failureReport, error) {
	text := strings.TrimRight(body, "\n")
	fmt.Printf("%s\n", tailText(text, logTailLines))
	pattern := cfg.Settings.FailureLogPattern
	if pattern == "" {
		pattern = config.DefaultFailureLogPattern
	}
	lines, err := linesMatching(text, pattern)
	if err != nil {
		return failureReport{}, fmt.Errorf("failure_log_pattern %q: %w", pattern, err)
	}
	for _, line := range lines {
		fmt.Println(line)
	}
	return failureReport{Text: text}, nil
}

func printNoFailedJob(workflow string, statuses []github.CommitStatus) {
	fmt.Printf("no failed job in %s\n", workflow)
	for _, status := range statuses {
		switch strings.ToLower(status.State) {
		case "failure", "error":
		default:
			continue
		}
		if status.Description == "" {
			fmt.Printf("%s %s\n", status.Context, status.State)
			continue
		}
		fmt.Printf("%s %s %s\n", status.Context, status.State, status.Description)
	}
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

// errNoFailedJob means the fetched workflow has no failed job.
// Callers print the commit statuses and continue. Other CircleCI errors stay fatal.
var errNoFailedJob = errors.New("circleci workflow has no failed job")

// noFailedJobError is errNoFailedJob for the workflow id that was queried.
type noFailedJobError struct {
	workflow string
}

func (e *noFailedJobError) Error() string {
	return errNoFailedJob.Error()
}

func (e *noFailedJobError) Unwrap() error {
	return errNoFailedJob
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
		if !circleJobFailed(job.Outcome) || job.ID == "" {
			continue
		}
		if jobName != "" && job.Name != jobName {
			continue
		}
		return job.ID, nil
	}
	return "", errNoFailedJob
}

type circleOutputStep struct {
	Outcome  string `json:"outcome"`
	Output   string `json:"output"`
	ExitCode *int   `json:"exit_code"` //nolint:tagliatelle // CircleCI job output uses snake_case
}

// circleFailedStepText returns the output of the failed steps. With
// fallback, it returns every unsuccessful step's output when none failed.
func circleFailedStepText(body string, fallback bool) (string, error) {
	var parsed struct {
		Steps []circleOutputStep `json:"steps"`
	}
	if err := json.Unmarshal([]byte(body), &parsed); err != nil {
		return "", fmt.Errorf("parse circleci job output: %w", err)
	}
	if text := circleStepOutput(parsed.Steps, true); text != "" || !fallback {
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
	return "", "", 0, false
}

// circleJobNumber parses a CircleCI v1.1 build link, /gh/<org>/<repo>/<job_number>.
// The last segment is a job number, not a pipeline number.
func circleJobNumber(raw string) (string, string, int, bool) {
	if !isCircleURL(raw) {
		return "", "", 0, false
	}
	parsed, err := url.Parse(raw)
	if err != nil {
		return "", "", 0, false
	}
	parts := strings.Split(strings.Trim(parsed.Path, "/"), "/")
	if len(parts) != 4 || parts[0] != "gh" {
		return "", "", 0, false
	}
	number, err := strconv.Atoi(parts[3])
	if err != nil || number <= 0 {
		return "", "", 0, false
	}
	return parts[1], parts[2], number, true
}

func circleStepLog(org, repo string, number int, token, checkName string) (circleLog, error) {
	pipelineID, err := circlePipelineID(org, repo, number, token)
	if err != nil {
		return circleLog{}, err
	}
	jobs, emptyWorkflow, err := circlePipelineFailedJobs(pipelineID, token)
	if err != nil {
		return circleLog{}, err
	}
	if len(jobs) == 0 {
		if emptyWorkflow != "" {
			return circleLog{}, &noFailedJobError{workflow: emptyWorkflow}
		}
		return circleLog{}, errors.New("circleci pipeline has no failed workflow")
	}
	chosen := selectCircleFailedJob(jobs, circleContextJob(checkName))
	return circleFailedJobLog(org, repo, chosen.jobNumber, token)
}

func circleJobLog(org, repo string, jobNumber int, token string) (circleLog, error) {
	workflowID, err := circleLatestWorkflow(org, repo, jobNumber, token)
	if err != nil {
		return circleLog{}, err
	}
	return circleWorkflowLog(org, repo, workflowID, token)
}

func circleLatestWorkflow(org, repo string, jobNumber int, token string) (string, error) {
	raw := fmt.Sprintf(
		"https://circleci.com/api/v2/project/gh/%s/%s/job/%d",
		url.PathEscape(org), url.PathEscape(repo), jobNumber,
	)
	var resp struct {
		LatestWorkflow struct {
			ID string `json:"id"`
		} `json:"latest_workflow"` //nolint:tagliatelle // CircleCI API uses snake_case
	}
	if err := circleJSON(raw, token, &resp); err != nil {
		return "", err
	}
	if resp.LatestWorkflow.ID == "" {
		return "", errors.New("circleci job response has no latest_workflow id")
	}
	return resp.LatestWorkflow.ID, nil
}

func circleWorkflowLog(org, repo, workflowID, token string) (circleLog, error) {
	jobNumber, err := circleFailedJob(workflowID, token)
	if err != nil {
		return circleLog{}, err
	}
	return circleFailedJobLog(org, repo, jobNumber, token)
}

// circleFailedJobLog reads the job's status before its step log. An
// infrastructure failure has a log only when one of its steps failed.
func circleFailedJobLog(org, repo string, jobNumber int, token string) (circleLog, error) {
	job, err := fetchCircleJob(org, repo, jobNumber, token)
	if err != nil {
		return circleLog{}, err
	}
	log := circleLog{}
	if circleInfraStatus(job.status) {
		log = circleLog{infra: true, reason: job.reason, jobURL: job.url}
		if log.jobURL == "" {
			log.jobURL = fmt.Sprintf("https://circleci.com/gh/%s/%s/%d", org, repo, jobNumber)
		}
	} else if job.outputURL == "" {
		return circleLog{}, errors.New("circleci failed job has no step output")
	}
	if job.outputURL == "" {
		return log, nil
	}
	log.text, err = circleOutputText(job.outputURL, token)
	if err != nil {
		return circleLog{}, err
	}
	return log, nil
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

type circleFailedJobRef struct {
	workflowName string
	jobNumber    int
	jobName      string
}

func circlePipelineFailedJobs(pipelineID, token string) ([]circleFailedJobRef, string, error) {
	raw := "https://circleci.com/api/v2/pipeline/" + url.PathEscape(pipelineID) + "/workflow"
	var resp struct {
		Items []struct {
			ID     string `json:"id"`
			Name   string `json:"name"`
			Status string `json:"status"`
		} `json:"items"`
	}
	if err := circleJSON(raw, token, &resp); err != nil {
		return nil, "", err
	}
	var jobs []circleFailedJobRef
	firstFailed := ""
	for _, item := range resp.Items {
		if item.ID == "" {
			continue
		}
		if strings.EqualFold(item.Status, "failed") && firstFailed == "" {
			firstFailed = item.ID
		}
		found, err := circleWorkflowFailedJobs(item.ID, item.Name, token)
		if err != nil {
			return nil, "", err
		}
		jobs = append(jobs, found...)
	}
	if len(jobs) > 0 {
		return jobs, "", nil
	}
	return nil, firstFailed, nil
}

func circleWorkflowFailedJobs(workflowID, workflowName, token string) ([]circleFailedJobRef, error) {
	raw := "https://circleci.com/api/v2/workflow/" + url.PathEscape(workflowID) + "/job"
	var resp struct {
		Items []struct {
			JobNumber int    `json:"job_number"` //nolint:tagliatelle // CircleCI API uses snake_case
			Name      string `json:"name"`
			Status    string `json:"status"`
		} `json:"items"`
	}
	if err := circleJSON(raw, token, &resp); err != nil {
		return nil, err
	}
	var jobs []circleFailedJobRef
	for _, item := range resp.Items {
		if !circleJobFailed(item.Status) || item.JobNumber <= 0 {
			continue
		}
		jobs = append(jobs, circleFailedJobRef{
			workflowName: workflowName,
			jobNumber:    item.JobNumber,
			jobName:      item.Name,
		})
	}
	return jobs, nil
}

func selectCircleFailedJob(jobs []circleFailedJobRef, name string) circleFailedJobRef {
	if name != "" {
		for _, job := range jobs {
			if job.jobName == name {
				return job
			}
		}
		for _, job := range jobs {
			if job.workflowName == name {
				return job
			}
		}
	}
	return jobs[0]
}

func circleFailedJob(workflowID, token string) (int, error) {
	jobs, err := circleWorkflowFailedJobs(workflowID, "", token)
	if err != nil {
		return 0, err
	}
	if len(jobs) == 0 {
		return 0, &noFailedJobError{workflow: workflowID}
	}
	return jobs[0].jobNumber, nil
}

// circleJobDetail is a CircleCI job's status, its messages, its link, and
// the output of its first failed step, or "" when no step failed.
type circleJobDetail struct {
	status    string
	reason    string
	url       string
	outputURL string
}

func fetchCircleJob(org, repo string, jobNumber int, token string) (circleJobDetail, error) {
	raw := fmt.Sprintf(
		"https://circleci.com/api/v1.1/project/gh/%s/%s/%d",
		url.PathEscape(org), url.PathEscape(repo), jobNumber,
	)
	var resp struct {
		Status   string          `json:"status"`
		BuildURL string          `json:"build_url"` //nolint:tagliatelle // CircleCI API uses snake_case
		Messages []circleMessage `json:"messages"`
		Steps    []struct {
			Actions []struct {
				Status    string `json:"status"`
				ExitCode  *int   `json:"exit_code"`  //nolint:tagliatelle // CircleCI API uses snake_case
				OutputURL string `json:"output_url"` //nolint:tagliatelle // CircleCI API uses snake_case
			} `json:"actions"`
		} `json:"steps"`
	}
	if err := circleJSON(raw, token, &resp); err != nil {
		return circleJobDetail{}, err
	}
	job := circleJobDetail{status: resp.Status, reason: circleReason(resp.Messages), url: resp.BuildURL}
	for _, step := range resp.Steps {
		for _, action := range step.Actions {
			if circleActionFailed(action.Status, action.ExitCode) && action.OutputURL != "" {
				job.outputURL = action.OutputURL
				return job, nil
			}
		}
	}
	return job, nil
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
