package gh

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
)

// ErrPermissionDenied indicates that gh lacked token scope or repository
// permission to read collaborator or team metadata.
var ErrPermissionDenied = errors.New("gh: permission denied or insufficient token scope")

// RepoTrustInfo describes repository visibility and ownership metadata from GraphQL.
type RepoTrustInfo struct {
	IsPrivate  bool
	Visibility string
	OwnerLogin string
	OwnerType  string
}

// Collaborator describes one repository collaborator returned by the GitHub REST API.
type Collaborator struct {
	Login    string
	RoleName string
	Admin    bool
	Maintain bool
	Push     bool
	Triage   bool
	Pull     bool
}

// RepoTeam describes one repository team returned by the GitHub REST API.
type RepoTeam struct {
	Name       string
	Slug       string
	Permission string
	Admin      bool
	Maintain   bool
	Push       bool
	Triage     bool
	Pull       bool
}

const repoTrustQuery = "query($owner:String!,$name:String!){" +
	"repository(owner:$owner,name:$name){" +
	"isPrivate visibility owner{login __typename}}}"

// RepoTrustInfo fetches repository.isPrivate, visibility, and owner metadata.
func (c *Client) RepoTrustInfo(ctx context.Context, repo string) (RepoTrustInfo, error) {
	if ctx == nil {
		return RepoTrustInfo{}, errors.New("gh repo trust info: expected a context, found nil")
	}
	if !validRepo(repo) {
		return RepoTrustInfo{}, fmt.Errorf("gh repo trust info: expected repository owner/repo, found %q", repo)
	}
	owner, name, _ := strings.Cut(repo, "/")
	result, err := c.execute(ctx,
		"api", "graphql",
		"-f", "query="+repoTrustQuery,
		"-f", "owner="+owner,
		"-f", "name="+name,
	)
	if err != nil {
		return RepoTrustInfo{}, err
	}
	if result.ExitCode != 0 {
		return RepoTrustInfo{}, ghFailure(result)
	}
	return decodeRepoTrustInfo([]byte(result.Output))
}

func decodeRepoTrustInfo(raw []byte) (RepoTrustInfo, error) {
	var top map[string]json.RawMessage
	if err := json.Unmarshal(raw, &top); err != nil {
		return RepoTrustInfo{}, fmt.Errorf("gh repo trust info: expected JSON object, found %s: %w", quoteSnippet(raw), err)
	}
	if errs, ok := top["errors"]; ok {
		if err := graphqlFailure(errs); err != nil {
			return RepoTrustInfo{}, err
		}
	}
	dataObj, err := rawObject(top["data"], "data")
	if err != nil {
		return RepoTrustInfo{}, fmt.Errorf("gh repo trust info: %w", err)
	}
	repoObj, err := rawObject(dataObj["repository"], "repository")
	if err != nil {
		return RepoTrustInfo{}, fmt.Errorf("gh repo trust info: %w", err)
	}
	isPrivate, err := requiredBool(repoObj, "isPrivate")
	if err != nil {
		return RepoTrustInfo{}, fmt.Errorf("gh repo trust info: %w", err)
	}
	visibility, err := decodeRepoVisibility(repoObj, isPrivate)
	if err != nil {
		return RepoTrustInfo{}, fmt.Errorf("gh repo trust info: %w", err)
	}
	ownerLogin, ownerType, err := decodeRepoOwner(repoObj["owner"])
	if err != nil {
		return RepoTrustInfo{}, fmt.Errorf("gh repo trust info: %w", err)
	}
	return RepoTrustInfo{
		IsPrivate:  isPrivate,
		Visibility: visibility,
		OwnerLogin: ownerLogin,
		OwnerType:  ownerType,
	}, nil
}

func decodeRepoVisibility(repoObj map[string]json.RawMessage, isPrivate bool) (string, error) {
	visibility := VisibilityPublic
	if isPrivate {
		visibility = VisibilityPrivate
	}
	rawVis, ok := repoObj["visibility"]
	if !ok || isNull(rawVis) {
		return visibility, nil
	}
	vis, err := jsonString(rawVis, "visibility")
	if err != nil {
		return "", err
	}
	if vis != "" {
		return vis, nil
	}
	return visibility, nil
}

func decodeRepoOwner(rawOwner json.RawMessage) (string, string, error) {
	if isNull(rawOwner) {
		return "", "", nil
	}
	ownerObj, err := rawObject(rawOwner, "owner")
	if err != nil {
		return "", "", err
	}
	var login, typ string
	if rawLogin, ok := ownerObj["login"]; ok && !isNull(rawLogin) {
		login, err = jsonString(rawLogin, "owner.login")
		if err != nil {
			return "", "", err
		}
	}
	if rawType, ok := ownerObj["__typename"]; ok && !isNull(rawType) {
		typ, err = jsonString(rawType, "owner.__typename")
		if err != nil {
			return "", "", err
		}
	}
	return login, typ, nil
}

// Collaborators lists repository collaborators and their effective permissions.
// Returns ErrPermissionDenied (wrapped) when gh lacks push/admin scope.
func (c *Client) Collaborators(ctx context.Context, repo string) ([]Collaborator, error) {
	raw, err := c.restRepoList(ctx, repo, "collaborators?per_page=100", "collaborators")
	if err != nil {
		return nil, err
	}
	var rows []struct {
		Login       string `json:"login"`
		RoleName    string `json:"role_name"` //nolint:tagliatelle // GitHub REST API uses snake_case role_name
		Permissions struct {
			Admin    bool `json:"admin"`
			Maintain bool `json:"maintain"`
			Push     bool `json:"push"`
			Triage   bool `json:"triage"`
			Pull     bool `json:"pull"`
		} `json:"permissions"`
	}
	if err := json.Unmarshal(raw, &rows); err != nil {
		return nil, fmt.Errorf("gh collaborators: expected JSON array, found %s: %w", quoteSnippet(raw), err)
	}
	out := make([]Collaborator, 0, len(rows))
	for _, row := range rows {
		out = append(out, Collaborator{
			Login:    row.Login,
			RoleName: row.RoleName,
			Admin:    row.Permissions.Admin,
			Maintain: row.Permissions.Maintain,
			Push:     row.Permissions.Push,
			Triage:   row.Permissions.Triage,
			Pull:     row.Permissions.Pull,
		})
	}
	return out, nil
}

// RepoTeams lists repository teams and their effective permissions.
// Returns ErrPermissionDenied (wrapped) when gh lacks permission or scope.
func (c *Client) RepoTeams(ctx context.Context, repo string) ([]RepoTeam, error) {
	raw, err := c.restRepoList(ctx, repo, "teams?per_page=100", "teams")
	if err != nil {
		return nil, err
	}
	var rows []struct {
		Name        string `json:"name"`
		Slug        string `json:"slug"`
		Permission  string `json:"permission"`
		Permissions struct {
			Admin    bool `json:"admin"`
			Maintain bool `json:"maintain"`
			Push     bool `json:"push"`
			Triage   bool `json:"triage"`
			Pull     bool `json:"pull"`
		} `json:"permissions"`
	}
	if err := json.Unmarshal(raw, &rows); err != nil {
		return nil, fmt.Errorf("gh teams: expected JSON array, found %s: %w", quoteSnippet(raw), err)
	}
	out := make([]RepoTeam, 0, len(rows))
	for _, row := range rows {
		out = append(out, RepoTeam{
			Name:       row.Name,
			Slug:       row.Slug,
			Permission: row.Permission,
			Admin:      row.Permissions.Admin,
			Maintain:   row.Permissions.Maintain,
			Push:       row.Permissions.Push,
			Triage:     row.Permissions.Triage,
			Pull:       row.Permissions.Pull,
		})
	}
	return out, nil
}

func (c *Client) restRepoList(ctx context.Context, repo, endpoint, label string) ([]byte, error) {
	if ctx == nil {
		return nil, fmt.Errorf("gh %s: expected a context, found nil", label)
	}
	if !validRepo(repo) {
		return nil, fmt.Errorf("gh %s: expected repository owner/repo, found %q", label, repo)
	}
	result, err := c.execute(ctx, "api", "repos/"+repo+"/"+endpoint)
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		fail := ghFailure(result)
		if isPermissionError(fail.Error()) {
			return nil, fmt.Errorf("%w: %w", ErrPermissionDenied, fail)
		}
		return nil, fail
	}
	return []byte(result.Output), nil
}

func isPermissionError(msg string) bool {
	lower := strings.ToLower(msg)
	for _, needle := range []string{
		"403",
		"forbidden",
		"must have push access",
		"must have admin",
		"insufficient",
		"resource not accessible",
		"not accessible by",
	} {
		if strings.Contains(lower, needle) {
			return true
		}
	}
	return false
}
