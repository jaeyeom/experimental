package doctor

import (
	"cmp"
	"fmt"
	"path"
	"slices"
	"strings"

	"gopkg.in/yaml.v3"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// TrustAudit holds optional GitHub repository visibility, collaborator
// permissions, and local .github automation scan inputs for doctor.
type TrustAudit struct {
	VisibilityChecked bool
	IsPrivate         bool
	Visibility        string
	VisibilityErr     string
	OwnerLogin        string
	OwnerType         string

	CollaboratorsChecked    bool
	Collaborators           []CollaboratorPermission
	CollaboratorsSkipReason string

	TeamsChecked    bool
	Teams           []TeamPermission
	TeamsSkipReason string

	RepoFiles []RepoFile
}

// CollaboratorPermission describes one GitHub collaborator's repository access.
type CollaboratorPermission struct {
	Login    string
	RoleName string
	Admin    bool
	Maintain bool
	Push     bool
	Triage   bool
}

// TeamPermission describes one GitHub team's repository access.
type TeamPermission struct {
	Name       string
	Slug       string
	Permission string
	Admin      bool
	Maintain   bool
	Push       bool
	Triage     bool
}

// RepoFile is one local repository template or workflow file scanned by doctor.
type RepoFile struct {
	Path    string
	Content string
}

func trustFindings(in Input) []Finding {
	var out []Finding
	if f, ok := trustEnvironmentFinding(in); ok {
		out = append(out, f)
	}
	out = append(out, trustConfigFindings(in)...)
	out = append(out, githubPermissionFindings(in)...)
	out = append(out, githubAutomationFindings(in)...)
	return out
}

func trustEnvironmentFinding(in Input) (Finding, bool) {
	if in.Config.Trust.Environment != "" {
		return Finding{}, false
	}
	if in.Kind == "github" && in.TrustAudit != nil && in.TrustAudit.VisibilityChecked {
		if !in.TrustAudit.IsPrivate {
			return finding(
				CheckTrustEnvironment,
				SeverityWarn,
				fmt.Sprintf("%s (%s) is a public repository and trust.environment is unset", in.SourceKey, in.Config.Repo),
				`set [source.trust] environment = "untrusted" in factory.toml (or "trusted" only if every issue author is trusted)`,
				in.SourceKey,
			), true
		}
		return finding(
			CheckTrustEnvironment,
			SeverityWarn,
			fmt.Sprintf("%s (%s) is a private repository and trust.environment is unset", in.SourceKey, in.Config.Repo),
			`set [source.trust] environment = "trusted" (team-only) or "untrusted" in factory.toml`,
			in.SourceKey,
		), true
	}
	return finding(
		CheckTrustEnvironment,
		SeverityWarn,
		fmt.Sprintf(`%s trust.environment is unset; classify as "trusted" or "untrusted"`, in.SourceKey),
		`set [source.trust] environment = "trusted" (corporate Jira / private team repo) or "untrusted" (public GitHub repo) in factory.toml`,
		in.SourceKey,
	), true
}

func trustConfigFindings(in Input) []Finding {
	var out []Finding
	isPublicGitHub := in.Kind == "github" && in.TrustAudit != nil && in.TrustAudit.VisibilityChecked && !in.TrustAudit.IsPrivate
	if isPublicGitHub && in.Config.Trust.Environment == config.TrustEnvTrusted {
		out = append(out, finding(
			CheckTrustConfig,
			SeverityWarn,
			fmt.Sprintf(`%s (%s) is a public GitHub repository marked trust.environment = "trusted"`, in.SourceKey, in.Config.Repo),
			`set [source.trust] environment = "untrusted" in factory.toml to gate untrusted external issues`,
			in.SourceKey,
		))
	}

	isUntrusted := in.Config.Trust.Environment == config.TrustEnvUntrusted
	if !isUntrusted && !isPublicGitHub {
		return out
	}

	readyStatuses := in.Config.Trust.UntrustedReadyStatuses
	if isUntrusted {
		readyStatuses = in.Config.Trust.EffectiveUntrustedReadyStatuses()
	}
	if risky := riskyUntrustedReadyStatuses(readyStatuses); len(risky) > 0 {
		out = append(out, finding(
			CheckTrustConfig,
			SeverityWarn,
			fmt.Sprintf("%s untrusted_ready_statuses includes %s; unreviewed external issues could start automatically", in.SourceKey, strings.Join(risky, ", ")),
			`restrict untrusted_ready_statuses to post-triage statuses such as ["Todo"] or []`,
			in.SourceKey,
		))
	}

	if isUntrusted && len(in.Config.Trust.EffectiveGuards()) == 0 {
		out = append(out, finding(
			CheckTrustConfig,
			SeverityWarn,
			fmt.Sprintf("%s trust.guards is empty; untrusted issues have no tamper guard against post-triage edits", in.SourceKey),
			`set trust.guards = ["approved_hash"] or ["locked"] in factory.toml`,
			in.SourceKey,
		))
	}

	assocs := in.Config.Trust.TrustedAuthorAssociations
	if isUntrusted {
		assocs = in.Config.Trust.EffectiveTrustedAuthorAssociations()
	}
	if broad := broadAuthorAssociations(assocs); len(broad) > 0 {
		out = append(out, finding(
			CheckTrustConfig,
			SeverityWarn,
			fmt.Sprintf("%s trusted_author_associations includes broad non-maintainer associations (%s)", in.SourceKey, strings.Join(broad, ", ")),
			`restrict trusted_author_associations to ["OWNER"] (or maintainer associations only)`,
			in.SourceKey,
		))
	}
	return out
}

func riskyUntrustedReadyStatuses(statuses []string) []string {
	var risky []string
	seen := make(map[string]struct{})
	for _, s := range statuses {
		trimmed := strings.TrimSpace(s)
		if !strings.EqualFold(trimmed, "OPEN") && !strings.EqualFold(trimmed, "Backlog") {
			continue
		}
		key := strings.ToUpper(trimmed)
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		risky = append(risky, trimmed)
	}
	return risky
}

func broadAuthorAssociations(assocs []string) []string {
	var broad []string
	seen := make(map[string]struct{})
	for _, assoc := range assocs {
		upper := strings.ToUpper(strings.TrimSpace(assoc))
		if upper == "" {
			continue
		}
		switch upper {
		case "OWNER", "MEMBER", "COLLABORATOR":
			continue
		}
		if _, ok := seen[upper]; ok {
			continue
		}
		seen[upper] = struct{}{}
		broad = append(broad, strings.TrimSpace(assoc))
	}
	return broad
}

func githubPermissionFindings(in Input) []Finding {
	if in.Kind != "github" || in.TrustAudit == nil {
		return nil
	}
	audit := in.TrustAudit
	var out []Finding
	if audit.VisibilityErr != "" {
		out = append(out, finding(
			CheckGitHubPermissions,
			SeverityInfo,
			fmt.Sprintf("%s (%s) repository visibility check skipped: %s", in.SourceKey, in.Config.Repo, audit.VisibilityErr),
			"run doctor without --offline with gh repo access",
			in.SourceKey,
		))
	}

	isPublic := audit.VisibilityChecked && !audit.IsPrivate
	shouldAuditActors := in.Config.Trust.Environment != config.TrustEnvTrusted || isPublic
	if shouldAuditActors {
		if actors := riskyRepoActors(in, audit); len(actors) > 0 {
			out = append(out, finding(
				CheckGitHubPermissions,
				SeverityWarn,
				fmt.Sprintf("%s (%s) non-owner collaborators or teams have triage/write access that allows status transitions (%s)", in.SourceKey, in.Config.Repo, strings.Join(actors, ", ")),
				`review repository collaborator/team permissions or enable trust.guards = ["approved_hash"]`,
				in.SourceKey,
			))
		}
	}

	if audit.CollaboratorsSkipReason != "" {
		out = append(out, finding(
			CheckGitHubPermissions,
			SeverityInfo,
			fmt.Sprintf("%s (%s) collaborator permissions not checked: %s", in.SourceKey, in.Config.Repo, audit.CollaboratorsSkipReason),
			"grant push/admin read access or inspect collaborator settings in GitHub",
			in.SourceKey,
		))
	} else if audit.TeamsSkipReason != "" {
		out = append(out, finding(
			CheckGitHubPermissions,
			SeverityInfo,
			fmt.Sprintf("%s (%s) team permissions not checked: %s", in.SourceKey, in.Config.Repo, audit.TeamsSkipReason),
			"inspect team permissions in GitHub repository settings",
			in.SourceKey,
		))
	}
	return out
}

func riskyRepoActors(in Input, audit *TrustAudit) []string {
	var actors []string
	if audit.CollaboratorsChecked {
		collabs := slices.Clone(audit.Collaborators)
		slices.SortFunc(collabs, func(a, b CollaboratorPermission) int { return cmp.Compare(a.Login, b.Login) })
		for _, c := range collabs {
			if c.Login == "" || isRepoOwnerOrMe(c.Login, in, audit) {
				continue
			}
			if role, ok := collaboratorRole(c); ok {
				actors = append(actors, fmt.Sprintf("%s (%s)", c.Login, role))
			}
		}
	}
	if audit.TeamsChecked {
		teams := slices.Clone(audit.Teams)
		slices.SortFunc(teams, func(a, b TeamPermission) int { return cmp.Compare(a.Slug, b.Slug) })
		for _, tm := range teams {
			name := cmp.Or(tm.Slug, tm.Name)
			if name == "" {
				continue
			}
			if role, ok := teamRole(tm); ok {
				actors = append(actors, fmt.Sprintf("team:%s (%s)", name, role))
			}
		}
	}
	return actors
}

func isRepoOwnerOrMe(login string, in Input, audit *TrustAudit) bool {
	if in.Snapshot.Me != "" && strings.EqualFold(login, in.Snapshot.Me) {
		return true
	}
	if audit.OwnerType != "Organization" && audit.OwnerLogin != "" && strings.EqualFold(login, audit.OwnerLogin) {
		return true
	}
	return false
}

func collaboratorRole(c CollaboratorPermission) (string, bool) {
	return permissionRole(c.RoleName, c.Admin, c.Maintain, c.Push, c.Triage)
}

func teamRole(t TeamPermission) (string, bool) {
	return permissionRole(t.Permission, t.Admin, t.Maintain, t.Push, t.Triage)
}

func permissionRole(roleName string, admin, maintain, push, triage bool) (string, bool) {
	switch strings.ToLower(strings.TrimSpace(roleName)) {
	case "admin", "maintain", "write", "push", "triage":
		return strings.ToLower(strings.TrimSpace(roleName)), true
	}
	switch {
	case admin:
		return "admin", true
	case maintain:
		return "maintain", true
	case push:
		return "write", true
	case triage:
		return "triage", true
	default:
		return "", false
	}
}

func githubAutomationFindings(in Input) []Finding {
	if in.Kind != "github" || in.TrustAudit == nil || len(in.TrustAudit.RepoFiles) == 0 {
		return nil
	}
	files := slices.Clone(in.TrustAudit.RepoFiles)
	slices.SortFunc(files, func(a, b RepoFile) int { return cmp.Compare(a.Path, b.Path) })
	readyLabels := in.Config.RequireLabels
	readyStatuses := readyStatusTargets(in.Config)

	var out []Finding
	for _, f := range files {
		reasons := scanRepoFile(f, readyLabels, readyStatuses)
		if len(reasons) == 0 {
			continue
		}
		out = append(out, finding(
			CheckGitHubAutomation,
			SeverityWarn,
			fmt.Sprintf("%s: %s %s", in.SourceKey, f.Path, strings.Join(reasons, "; ")),
			`remove automatic ready-label or status/Todo assignment on new issues, or enforce trust.guards = ["approved_hash"]`,
			f.Path,
		))
	}
	return out
}

func readyStatusTargets(cfg config.Source) []string {
	seen := map[string]struct{}{}
	var out []string
	add := func(status string) {
		trimmed := strings.TrimSpace(status)
		if trimmed == "" || strings.EqualFold(trimmed, "OPEN") || strings.EqualFold(trimmed, "Backlog") {
			return
		}
		key := strings.ToLower(trimmed)
		if _, ok := seen[key]; ok {
			return
		}
		seen[key] = struct{}{}
		out = append(out, trimmed)
	}
	add(config.DefaultGitHubReleaseStatus)
	for _, s := range cfg.Trust.EffectiveUntrustedReadyStatuses() {
		add(s)
	}
	return out
}

func scanRepoFile(f RepoFile, readyLabels, readyStatuses []string) []string {
	clean := path.Clean(strings.ReplaceAll(f.Path, "\\", "/"))
	switch {
	case strings.HasPrefix(clean, ".github/ISSUE_TEMPLATE/"):
		return scanIssueTemplate(path.Base(clean), f.Content, readyLabels, readyStatuses)
	case strings.HasPrefix(clean, ".github/workflows/"):
		return scanWorkflow(f.Content, readyLabels, readyStatuses)
	default:
		return nil
	}
}

func scanIssueTemplate(baseName, content string, readyLabels, readyStatuses []string) []string {
	lowerBase := strings.ToLower(baseName)
	if lowerBase == "config.yml" || lowerBase == "config.yaml" {
		return nil
	}
	var yamlText string
	switch {
	case strings.HasSuffix(lowerBase, ".md") || strings.HasSuffix(lowerBase, ".markdown"):
		fm, ok := extractFrontmatter(content)
		if !ok {
			return nil
		}
		yamlText = fm
	case strings.HasSuffix(lowerBase, ".yml") || strings.HasSuffix(lowerBase, ".yaml"):
		yamlText = content
	default:
		return nil
	}

	var doc map[string]any
	if err := yaml.Unmarshal([]byte(yamlText), &doc); err != nil || doc == nil {
		return nil
	}
	var reasons []string
	if matched := matchedLabels(doc["labels"], readyLabels); len(matched) > 0 {
		reasons = append(reasons, fmt.Sprintf("auto-adds ready label (%s) on new issues", strings.Join(matched, ", ")))
	}
	if statusVal, ok := doc["status"].(string); ok && matchesAnyFold(statusVal, readyStatuses) {
		reasons = append(reasons, fmt.Sprintf("auto-sets issue status %q on new issues", statusVal))
	}
	return reasons
}

func extractFrontmatter(content string) (string, bool) {
	norm := strings.ReplaceAll(content, "\r\n", "\n")
	lines := strings.Split(norm, "\n")
	if len(lines) < 3 || strings.TrimSpace(lines[0]) != "---" {
		return "", false
	}
	for i := 1; i < len(lines); i++ {
		if strings.TrimSpace(lines[i]) == "---" {
			return strings.Join(lines[1:i], "\n"), true
		}
	}
	return "", false
}

func scanWorkflow(content string, readyLabels, readyStatuses []string) []string {
	var wf map[string]any
	if err := yaml.Unmarshal([]byte(content), &wf); err != nil || wf == nil {
		return nil
	}
	if !triggersOnNewIssue(wf["on"]) {
		return nil
	}
	jobs, ok := wf["jobs"].(map[string]any)
	if !ok {
		return nil
	}
	jobNames := make([]string, 0, len(jobs))
	for name := range jobs {
		jobNames = append(jobNames, name)
	}
	slices.Sort(jobNames)

	var reasons []string
	seen := make(map[string]struct{})
	addReason := func(reason string) {
		if _, exists := seen[reason]; exists {
			return
		}
		seen[reason] = struct{}{}
		reasons = append(reasons, reason)
	}

	for _, jobName := range jobNames {
		jobMap, ok := jobs[jobName].(map[string]any)
		if !ok {
			continue
		}
		steps, ok := jobMap["steps"].([]any)
		if !ok {
			continue
		}
		for _, rawStep := range steps {
			step, ok := rawStep.(map[string]any)
			if !ok {
				continue
			}
			for _, r := range scanWorkflowStep(step, readyLabels, readyStatuses) {
				addReason(r)
			}
		}
	}
	return reasons
}

func triggersOnNewIssue(onVal any) bool {
	switch v := onVal.(type) {
	case string:
		return strings.EqualFold(strings.TrimSpace(v), "issues")
	case []any:
		for _, item := range v {
			if s, ok := item.(string); ok && strings.EqualFold(strings.TrimSpace(s), "issues") {
				return true
			}
		}
		return false
	case map[string]any:
		issuesVal, ok := v["issues"]
		if !ok {
			return false
		}
		issuesMap, ok := issuesVal.(map[string]any)
		if !ok || issuesMap == nil {
			return true
		}
		typesVal, hasTypes := issuesMap["types"]
		if !hasTypes || typesVal == nil {
			return true
		}
		return eventTypesIncludeNewIssue(typesVal)
	default:
		return false
	}
}

func eventTypesIncludeNewIssue(typesVal any) bool {
	switch tv := typesVal.(type) {
	case string:
		return isNewIssueType(tv)
	case []any:
		for _, item := range tv {
			if s, ok := item.(string); ok && isNewIssueType(s) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

func isNewIssueType(typ string) bool {
	switch strings.ToLower(strings.TrimSpace(typ)) {
	case "opened", "reopened":
		return true
	default:
		return false
	}
}

func scanWorkflowStep(step map[string]any, readyLabels, readyStatuses []string) []string {
	var reasons []string
	if withMap, ok := step["with"].(map[string]any); ok {
		for _, key := range []string{"labels", "add-labels", "issue-labels"} {
			if matched := matchedLabels(withMap[key], readyLabels); len(matched) > 0 {
				reasons = append(reasons, fmt.Sprintf("auto-adds ready label (%s) on newly opened issues", strings.Join(matched, ", ")))
			}
		}
		for _, key := range []string{"status", "planning-status", "issue-status"} {
			if s, ok := withMap[key].(string); ok && matchesAnyFold(s, readyStatuses) {
				reasons = append(reasons, fmt.Sprintf("auto-sets issue status %q on newly opened issues", s))
			}
		}
		if script, ok := withMap["script"].(string); ok {
			reasons = append(reasons, scanScriptText(script, readyLabels, readyStatuses)...)
		}
	}
	if runText, ok := step["run"].(string); ok {
		reasons = append(reasons, scanRunText(runText, readyLabels, readyStatuses)...)
	}
	return reasons
}

func scanScriptText(script string, readyLabels, readyStatuses []string) []string {
	var reasons []string
	lower := strings.ToLower(script)
	if strings.Contains(lower, "addlabels") || strings.Contains(lower, "/labels") {
		if matched := labelsMentionedInText(script, readyLabels); len(matched) > 0 {
			reasons = append(reasons, fmt.Sprintf("auto-adds ready label (%s) on newly opened issues", strings.Join(matched, ", ")))
		}
	}
	if status, ok := statusSetInText(script, readyStatuses); ok {
		reasons = append(reasons, fmt.Sprintf("auto-sets native issue status (%s) on newly opened issues", status))
	}
	return reasons
}

func scanRunText(runText string, readyLabels, readyStatuses []string) []string {
	var reasons []string
	lower := strings.ToLower(runText)
	if strings.Contains(lower, "--add-label") || strings.Contains(lower, "/labels") {
		if matched := labelsMentionedInText(runText, readyLabels); len(matched) > 0 {
			reasons = append(reasons, fmt.Sprintf("auto-adds ready label (%s) on newly opened issues", strings.Join(matched, ", ")))
		}
	}
	if status, ok := statusSetInText(runText, readyStatuses); ok {
		reasons = append(reasons, fmt.Sprintf("auto-sets native issue status (%s) on newly opened issues", status))
	}
	return reasons
}

func statusSetInText(text string, readyStatuses []string) (string, bool) {
	lower := strings.ToLower(text)
	hasStatusMutator := strings.Contains(lower, "planningstatus") ||
		strings.Contains(lower, "updateissueplanningstatus") ||
		strings.Contains(lower, "--status") ||
		strings.Contains(lower, "updateprojectv2itemfieldvalue")
	if !hasStatusMutator {
		return "", false
	}
	for _, status := range readyStatuses {
		if containsWordOrQuoted(text, status) {
			return status, true
		}
	}
	if strings.Contains(lower, "planningstatus") || strings.Contains(lower, "updateissueplanningstatus") {
		return "planningStatus", true
	}
	return "", false
}

func labelsMentionedInText(text string, readyLabels []string) []string {
	var matched []string
	for _, label := range readyLabels {
		if containsWordOrQuoted(text, label) {
			matched = append(matched, label)
		}
	}
	return matched
}

func containsWordOrQuoted(text, needle string) bool {
	if needle == "" {
		return false
	}
	lowerText := strings.ToLower(text)
	lowerNeedle := strings.ToLower(needle)
	start := 0
	for {
		idx := strings.Index(lowerText[start:], lowerNeedle)
		if idx < 0 {
			return false
		}
		abs := start + idx
		end := abs + len(lowerNeedle)
		prevOK := abs == 0 || !isIdentChar(lowerText[abs-1])
		nextOK := end == len(lowerText) || !isIdentChar(lowerText[end])
		if prevOK && nextOK {
			return true
		}
		start = abs + len(lowerNeedle)
	}
}

func isIdentChar(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9') || b == '_' || b == '-'
}

func matchedLabels(raw any, readyLabels []string) []string {
	if len(readyLabels) == 0 || raw == nil {
		return nil
	}
	var candidates []string
	switch v := raw.(type) {
	case string:
		for _, part := range strings.FieldsFunc(v, func(r rune) bool { return r == ',' || r == '\n' }) {
			if s := strings.TrimSpace(part); s != "" {
				candidates = append(candidates, s)
			}
		}
	case []any:
		for _, item := range v {
			if s, ok := item.(string); ok {
				if trimmed := strings.TrimSpace(s); trimmed != "" {
					candidates = append(candidates, trimmed)
				}
			}
		}
	}
	var matched []string
	for _, c := range candidates {
		if matchesAnyFold(c, readyLabels) {
			matched = append(matched, c)
		}
	}
	return matched
}

func matchesAnyFold(val string, targets []string) bool {
	trimmed := strings.TrimSpace(val)
	for _, target := range targets {
		if strings.EqualFold(trimmed, target) {
			return true
		}
	}
	return false
}
