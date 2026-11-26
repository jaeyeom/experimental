package cli

import (
	"cmp"
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/doctor"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
)

const maxDoctorRepoFileBytes = 512 * 1024

type onlineTrustProbe struct {
	info            gh.RepoTrustInfo
	visibilityErr   string
	collaborators   []doctor.CollaboratorPermission
	collabChecked   bool
	collabSkip      string
	teams           []doctor.TeamPermission
	teamsChecked    bool
	teamsSkipReason string
}

func buildSourceTrustAudit(ctx context.Context, cfg *config.Config, src config.Source, ghOnline bool, cache map[string]onlineTrustProbe) *doctor.TrustAudit {
	if src.Kind != "github" {
		return nil
	}
	audit := &doctor.TrustAudit{
		RepoFiles: scanLocalGitHubFiles(src.Cwd),
	}
	if !ghOnline {
		return audit
	}
	probe, ok := cache[src.Repo]
	if !ok {
		probe = fetchOnlineTrustProbe(ctx, cfg.GHBin, src.Repo)
		cache[src.Repo] = probe
	}
	if probe.visibilityErr != "" {
		audit.VisibilityErr = probe.visibilityErr
		return audit
	}
	audit.VisibilityChecked = true
	audit.IsPrivate = probe.info.IsPrivate
	audit.Visibility = probe.info.Visibility
	audit.OwnerLogin = probe.info.OwnerLogin
	audit.OwnerType = probe.info.OwnerType
	audit.CollaboratorsChecked = probe.collabChecked
	audit.Collaborators = probe.collaborators
	audit.CollaboratorsSkipReason = probe.collabSkip
	audit.TeamsChecked = probe.teamsChecked
	audit.Teams = probe.teams
	audit.TeamsSkipReason = probe.teamsSkipReason
	return audit
}

func fetchOnlineTrustProbe(ctx context.Context, ghBin, repo string) onlineTrustProbe {
	if ghBin == "" {
		ghBin = config.DefaultGHBin
	}
	exec := newCommandExec()
	if exec == nil {
		return onlineTrustProbe{visibilityErr: "expected an executor, found nil"}
	}
	client, err := ghClient(exec, ghBin)
	if err != nil {
		return onlineTrustProbe{visibilityErr: err.Error()}
	}
	info, err := client.RepoTrustInfo(ctx, repo)
	if err != nil {
		return onlineTrustProbe{visibilityErr: err.Error()}
	}
	probe := onlineTrustProbe{info: info}

	collabs, err := client.Collaborators(ctx, repo)
	if err != nil {
		probe.collabSkip = trustSkipReason(err)
	} else {
		probe.collabChecked = true
		probe.collaborators = make([]doctor.CollaboratorPermission, 0, len(collabs))
		for _, c := range collabs {
			probe.collaborators = append(probe.collaborators, doctor.CollaboratorPermission{
				Login:    c.Login,
				RoleName: c.RoleName,
				Admin:    c.Admin,
				Maintain: c.Maintain,
				Push:     c.Push,
				Triage:   c.Triage,
			})
		}
	}

	if info.OwnerType == "Organization" {
		teams, err := client.RepoTeams(ctx, repo)
		if err != nil {
			probe.teamsSkipReason = trustSkipReason(err)
		} else {
			probe.teamsChecked = true
			probe.teams = make([]doctor.TeamPermission, 0, len(teams))
			for _, tm := range teams {
				probe.teams = append(probe.teams, doctor.TeamPermission{
					Name:       tm.Name,
					Slug:       tm.Slug,
					Permission: tm.Permission,
					Admin:      tm.Admin,
					Maintain:   tm.Maintain,
					Push:       tm.Push,
					Triage:     tm.Triage,
				})
			}
		}
	}
	return probe
}

func trustSkipReason(err error) string {
	if errors.Is(err, gh.ErrPermissionDenied) {
		return "insufficient token scope or permission (403)"
	}
	return err.Error()
}

func scanLocalGitHubFiles(cwd string) []doctor.RepoFile {
	if cwd == "" {
		return nil
	}
	var out []doctor.RepoFile
	specs := []struct {
		sub  string
		exts []string
	}{
		{sub: "ISSUE_TEMPLATE", exts: []string{".md", ".markdown", ".yml", ".yaml"}},
		{sub: "workflows", exts: []string{".yml", ".yaml"}},
	}
	for _, spec := range specs {
		dir := filepath.Join(cwd, ".github", spec.sub)
		entries, err := os.ReadDir(dir)
		if err != nil {
			continue
		}
		for _, entry := range entries {
			if entry.IsDir() {
				continue
			}
			ext := strings.ToLower(filepath.Ext(entry.Name()))
			if !slices.Contains(spec.exts, ext) {
				continue
			}
			fullPath := filepath.Join(dir, entry.Name())
			info, err := entry.Info()
			if err != nil || info.Size() > maxDoctorRepoFileBytes {
				continue
			}
			raw, err := os.ReadFile(fullPath) //nolint:gosec // G304: bounded read of .github template/workflow in configured source cwd
			if err != nil {
				continue
			}
			relPath := filepath.ToSlash(filepath.Join(".github", spec.sub, entry.Name()))
			out = append(out, doctor.RepoFile{Path: relPath, Content: string(raw)})
		}
	}
	slices.SortFunc(out, func(a, b doctor.RepoFile) int { return cmp.Compare(a.Path, b.Path) })
	return out
}
