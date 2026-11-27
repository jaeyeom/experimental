package github

import (
	"encoding/json"
	"fmt"
	"strconv"
)

// Ownership is what gh-approve's CODEOWNERS conditions read. It is
// computed for the viewer and filled only when a rule needs it.
type Ownership struct {
	// Err is set when CODEOWNERS could not be read. The condition then fails.
	Err string
	// Files are the changed files in the order of ApprovalPullRequest.Files.
	Files []FileOwnership
}

// FileOwnership is who owns one changed file under CODEOWNERS.
type FileOwnership struct {
	Path string
	// Owners are CODEOWNERS owners such as "@alice" or "@acme/core".
	Owners []string
	// Mine is true when an owner is the viewer or a team that includes the viewer.
	Mine bool
	// Blocker says which owner keeps the file with the viewer, or "" when
	// the file may be handed off without matching paths_any_match.
	Blocker string
	// MineOwners are the owners that are the viewer or include the viewer.
	MineOwners []string
	// Unknown are owners whose membership could not be read, with the reason.
	Unknown []string
	// Entry is the CODEOWNERS line that owns the file, one per section, such
	// as "/docs/** @acme/docs". Empty when no line matches.
	Entry string
}

// Values of ReviewRequestEvent.Event.
const (
	ReviewRequested      = "review_requested"
	ReviewRequestRemoved = "review_request_removed"
)

// ReviewRequestEvent is one review request timeline event for a user,
// oldest first.
type ReviewRequestEvent struct {
	Event    string `json:"event"`
	Reviewer string `json:"reviewer"`
	// Actor is who requested or removed the reviewer.
	Actor string `json:"actor"`
	Time  string `json:"time"`
}

// codeownersLocations are where GitHub looks for CODEOWNERS, in order.
var codeownersLocations = []struct{ alias, path string }{
	{"github", ".github/CODEOWNERS"},
	{"root", "CODEOWNERS"},
	{"docs", "docs/CODEOWNERS"},
}

const codeownersQuery = `
query($owner: String!, $name: String!, $github: String!, $root: String!, $docs: String!) {
  repository(owner: $owner, name: $name) {
    github: object(expression: $github) { ... on Blob { oid text isTruncated } }
    root: object(expression: $root) { ... on Blob { oid text isTruncated } }
    docs: object(expression: $docs) { ... on Blob { oid text isTruncated } }
  }
}`

type blobJSON struct {
	OID         string  `json:"oid"`
	Text        *string `json:"text"`
	IsTruncated bool    `json:"isTruncated"`
}

// CodeownersBlob is a CODEOWNERS file's path, content, and Git blob OID.
type CodeownersBlob struct {
	Path string
	Text string
	OID  string
}

// CodeownersFile reads the CODEOWNERS file GitHub uses on baseRef and
// returns its path and text. A missing or unreadable file is an error.
func (c *Client) CodeownersFile(ref PullRef, baseRef string) (string, string, error) {
	blob, err := c.CodeownersBlob(ref, baseRef)
	if err != nil {
		return "", "", err
	}
	return blob.Path, blob.Text, nil
}

// CodeownersBlob reads the CODEOWNERS file GitHub uses on baseRef and
// returns its path, text, and blob OID. A missing or unreadable file is an error.
func (c *Client) CodeownersBlob(ref PullRef, baseRef string) (CodeownersBlob, error) {
	name := fmt.Sprintf("%s/%s", ref.Owner, ref.Repo)
	if baseRef == "" {
		return CodeownersBlob{}, fmt.Errorf("read CODEOWNERS of %s: base branch unknown", name)
	}
	fields := []string{"-f", "owner=" + ref.Owner, "-f", "name=" + ref.Repo}
	for _, loc := range codeownersLocations {
		fields = append(fields, "-f", loc.alias+"="+baseRef+":"+loc.path)
	}
	body, callErr := c.graphqlQuery(codeownersQuery, fields)
	if err := graphqlCallError("read CODEOWNERS of "+name, body, callErr); err != nil {
		return CodeownersBlob{}, err
	}
	var resp struct {
		Data struct {
			Repository map[string]*blobJSON `json:"repository"`
		} `json:"data"`
	}
	if err := body.decode(&resp); err != nil {
		return CodeownersBlob{}, fmt.Errorf("read CODEOWNERS of %s: %w", name, err)
	}
	for _, loc := range codeownersLocations {
		blob := resp.Data.Repository[loc.alias]
		switch {
		case blob == nil:
			continue
		case blob.Text == nil:
			return CodeownersBlob{}, fmt.Errorf("read %s of %s: not a text file", loc.path, name)
		case blob.IsTruncated:
			return CodeownersBlob{}, fmt.Errorf("read %s of %s: file truncated", loc.path, name)
		}
		return CodeownersBlob{Path: loc.path, Text: *blob.Text, OID: blob.OID}, nil
	}
	return CodeownersBlob{}, fmt.Errorf("read CODEOWNERS of %s: no CODEOWNERS file on %s", name, baseRef)
}

const repoFileQuery = `
query($owner: String!, $name: String!, $expr: String!) {
  repository(owner: $owner, name: $name) {
    object(expression: $expr) { ... on Blob { text isTruncated } }
  }
}`

// RepoFile reads path at rev (commit SHA or branch ref) from ref's repository.
func (c *Client) RepoFile(ref PullRef, rev, path string) (string, error) {
	name := fmt.Sprintf("%s/%s", ref.Owner, ref.Repo)
	if rev == "" {
		return "", fmt.Errorf("base SHA unknown")
	}
	body, callErr := c.graphqlQuery(repoFileQuery, []string{
		"-f", "owner=" + ref.Owner,
		"-f", "name=" + ref.Repo,
		"-f", "expr=" + rev + ":" + path,
	})
	if err := graphqlCallError("read "+path+" of "+name, body, callErr); err != nil {
		return "", err
	}
	var resp struct {
		Data struct {
			Repository *struct {
				Object *blobJSON `json:"object"`
			} `json:"repository"`
		} `json:"data"`
	}
	if err := body.decode(&resp); err != nil {
		return "", fmt.Errorf("read %s of %s: %w", path, name, err)
	}
	if resp.Data.Repository == nil || resp.Data.Repository.Object == nil {
		return "", fmt.Errorf("not found at %s", rev)
	}
	blob := resp.Data.Repository.Object
	switch {
	case blob.Text == nil:
		return "", fmt.Errorf("not a text file at %s", rev)
	case blob.IsTruncated:
		return "", fmt.Errorf("file truncated at %s", rev)
	}
	return *blob.Text, nil
}

const teamMembersQuery = `
query($org: String!, $slug: String!, $after: String) {
  organization(login: $org) {
    team(slug: $slug) {
      members(first: 100, after: $after, membership: ALL) {
        pageInfo { hasNextPage endCursor }
        nodes { login }
      }
    }
  }
}`

// TeamMembers lists the logins of a team, including child team members.
// It needs the read:org scope.
func (c *Client) TeamMembers(org, slug string) ([]string, error) {
	action := fmt.Sprintf("read members of @%s/%s (needs the read:org scope)", org, slug)
	var members []string
	after := ""
	for {
		fields := []string{"-f", "org=" + org, "-f", "slug=" + slug}
		if after != "" {
			fields = append(fields, "-f", "after="+after)
		}
		body, callErr := c.graphqlQuery(teamMembersQuery, fields)
		if err := graphqlCallError(action, body, callErr); err != nil {
			return nil, err
		}
		var resp struct {
			Data struct {
				Organization *struct {
					Team *struct {
						Members struct {
							PageInfo struct {
								HasNextPage bool   `json:"hasNextPage"`
								EndCursor   string `json:"endCursor"`
							} `json:"pageInfo"`
							Nodes []struct {
								Login string `json:"login"`
							} `json:"nodes"`
						} `json:"members"`
					} `json:"team"`
				} `json:"organization"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return nil, fmt.Errorf("%s: %w", action, err)
		}
		if resp.Data.Organization == nil || resp.Data.Organization.Team == nil {
			return nil, fmt.Errorf("%s: team not visible", action)
		}
		page := resp.Data.Organization.Team.Members
		for _, node := range page.Nodes {
			members = append(members, node.Login)
		}
		if !page.PageInfo.HasNextPage || page.PageInfo.EndCursor == "" {
			return members, nil
		}
		after = page.PageInfo.EndCursor
	}
}

const viewerTeamsQuery = `
query($org: String!, $viewer: String!, $after: String) {
  organization(login: $org) {
    teams(first: 100, after: $after, userLogins: [$viewer]) {
      pageInfo { hasNextPage endCursor }
      nodes {
        slug
        ancestors(first: 100) {
          pageInfo { hasNextPage }
          nodes { slug }
        }
      }
    }
  }
}`

// ViewerTeams lists the slugs of the organization's teams that include
// viewer, in one paged query. The viewer's direct teams come back with
// their ancestors, because membership of a child team is membership of its
// parents. A truncated ancestor list is an error. It needs the read:org scope.
func (c *Client) ViewerTeams(org, viewer string) ([]string, error) {
	action := fmt.Sprintf("read teams of %s in %s (needs the read:org scope)", viewer, org)
	var slugs []string
	after := ""
	for {
		fields := []string{"-f", "org=" + org, "-f", "viewer=" + viewer}
		if after != "" {
			fields = append(fields, "-f", "after="+after)
		}
		body, callErr := c.graphqlQuery(viewerTeamsQuery, fields)
		if err := graphqlCallError(action, body, callErr); err != nil {
			return nil, err
		}
		var resp struct {
			Data struct {
				Organization *struct {
					Teams struct {
						PageInfo struct {
							HasNextPage bool   `json:"hasNextPage"`
							EndCursor   string `json:"endCursor"`
						} `json:"pageInfo"`
						Nodes []struct {
							Slug      string `json:"slug"`
							Ancestors struct {
								PageInfo struct {
									HasNextPage bool `json:"hasNextPage"`
								} `json:"pageInfo"`
								Nodes []struct {
									Slug string `json:"slug"`
								} `json:"nodes"`
							} `json:"ancestors"`
						} `json:"nodes"`
					} `json:"teams"`
				} `json:"organization"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return nil, fmt.Errorf("%s: %w", action, err)
		}
		if resp.Data.Organization == nil {
			return nil, fmt.Errorf("%s: organization not visible", action)
		}
		page := resp.Data.Organization.Teams
		for _, node := range page.Nodes {
			if node.Ancestors.PageInfo.HasNextPage {
				return nil, fmt.Errorf("%s: too many parent teams of %s", action, node.Slug)
			}
			slugs = append(slugs, node.Slug)
			for _, ancestor := range node.Ancestors.Nodes {
				slugs = append(slugs, ancestor.Slug)
			}
		}
		if !page.PageInfo.HasNextPage || page.PageInfo.EndCursor == "" {
			return slugs, nil
		}
		after = page.PageInfo.EndCursor
	}
}

const teamMemberCountQuery = `
query($org: String!, $slug: String!) {
  organization(login: $org) {
    team(slug: $slug) {
      members(first: 1, membership: ALL) { totalCount }
    }
  }
}`

// TeamMemberCount returns how many members a team has, including child team
// members, without listing them. It needs the read:org scope.
func (c *Client) TeamMemberCount(org, slug string) (int, error) {
	action := fmt.Sprintf("count members of @%s/%s (needs the read:org scope)", org, slug)
	body, callErr := c.graphqlQuery(teamMemberCountQuery, []string{"-f", "org=" + org, "-f", "slug=" + slug})
	if err := graphqlCallError(action, body, callErr); err != nil {
		return 0, err
	}
	var resp struct {
		Data struct {
			Organization *struct {
				Team *struct {
					Members struct {
						TotalCount int `json:"totalCount"`
					} `json:"members"`
				} `json:"team"`
			} `json:"organization"`
		} `json:"data"`
	}
	if err := body.decode(&resp); err != nil {
		return 0, fmt.Errorf("%s: %w", action, err)
	}
	if resp.Data.Organization == nil || resp.Data.Organization.Team == nil {
		return 0, fmt.Errorf("%s: team not visible", action)
	}
	return resp.Data.Organization.Team.Members.TotalCount, nil
}

const reviewRequestTimelineQuery = `
query($owner: String!, $name: String!, $number: Int!, $after: String) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      timelineItems(itemTypes: [REVIEW_REQUESTED_EVENT, REVIEW_REQUEST_REMOVED_EVENT], first: 100, after: $after) {
        pageInfo { hasNextPage endCursor }
        nodes {
          __typename
          ... on ReviewRequestedEvent {
            createdAt actor { login } requestedReviewer { __typename ... on User { login } }
          }
          ... on ReviewRequestRemovedEvent {
            createdAt actor { login } requestedReviewer { __typename ... on User { login } }
          }
        }
      }
    }
  }
}`

type timelineNodeJSON struct {
	TypeName  string `json:"__typename"` //nolint:tagliatelle // GraphQL field name
	CreatedAt string `json:"createdAt"`
	Actor     *struct {
		Login string `json:"login"`
	} `json:"actor"`
	RequestedReviewer *struct {
		TypeName string `json:"__typename"` //nolint:tagliatelle // GraphQL field name
		Login    string `json:"login"`
	} `json:"requestedReviewer"`
}

// ReviewRequestTimeline lists every review request and removal of a user
// on the pull request, oldest first, across all pages.
func (c *Client) ReviewRequestTimeline(ref PullRef) ([]ReviewRequestEvent, error) {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	var events []ReviewRequestEvent
	after := ""
	for {
		fields := []string{
			"-f", "owner=" + ref.Owner,
			"-f", "name=" + ref.Repo,
			"-F", "number=" + strconv.Itoa(ref.Number),
		}
		if after != "" {
			fields = append(fields, "-f", "after="+after)
		}
		body, callErr := c.graphqlQuery(reviewRequestTimelineQuery, fields)
		if err := graphqlCallError("read review request timeline of "+name, body, callErr); err != nil {
			return nil, err
		}
		var resp struct {
			Data struct {
				Repository *struct {
					PullRequest *struct {
						TimelineItems struct {
							PageInfo struct {
								HasNextPage bool   `json:"hasNextPage"`
								EndCursor   string `json:"endCursor"`
							} `json:"pageInfo"`
							Nodes []timelineNodeJSON `json:"nodes"`
						} `json:"timelineItems"`
					} `json:"pullRequest"`
				} `json:"repository"`
			} `json:"data"`
		}
		if err := body.decode(&resp); err != nil {
			return nil, fmt.Errorf("read review request timeline of %s: %w", name, err)
		}
		if resp.Data.Repository == nil || resp.Data.Repository.PullRequest == nil {
			return nil, fmt.Errorf("read review request timeline of %s: not found", name)
		}
		items := resp.Data.Repository.PullRequest.TimelineItems
		for _, node := range items.Nodes {
			if event, ok := convertTimelineNode(node); ok {
				events = append(events, event)
			}
		}
		if !items.PageInfo.HasNextPage || items.PageInfo.EndCursor == "" {
			return events, nil
		}
		after = items.PageInfo.EndCursor
	}
}

func convertTimelineNode(node timelineNodeJSON) (ReviewRequestEvent, bool) {
	event := ReviewRequestEvent{Time: node.CreatedAt}
	switch node.TypeName {
	case "ReviewRequestedEvent":
		event.Event = ReviewRequested
	case "ReviewRequestRemovedEvent":
		event.Event = ReviewRequestRemoved
	default:
		return event, false
	}
	reviewer := node.RequestedReviewer
	if reviewer == nil || reviewer.TypeName != "User" || reviewer.Login == "" {
		return event, false
	}
	event.Reviewer = reviewer.Login
	if node.Actor != nil {
		event.Actor = node.Actor.Login
	}
	return event, true
}

// RequestReviewers requests logins as individual reviewers. It is a write
// and is never retried.
func (c *Client) RequestReviewers(ref PullRef, logins []string) error {
	return c.editRequestedReviewers(ref, "POST", "request reviewers on", logins)
}

// RemoveRequestedReviewer removes login as an individual requested
// reviewer. Team requests and reviews are never touched. It is a write and
// is never retried.
func (c *Client) RemoveRequestedReviewer(ref PullRef, login string) error {
	return c.editRequestedReviewers(ref, "DELETE", "remove requested reviewer from", []string{login})
}

func (c *Client) editRequestedReviewers(ref PullRef, method, action string, logins []string) error {
	name := fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
	payload, err := json.Marshal(map[string][]string{"reviewers": logins})
	if err != nil {
		return fmt.Errorf("%s %s: %w", action, name, err)
	}
	path := fmt.Sprintf("repos/%s/%s/pulls/%d/requested_reviewers", ref.Owner, ref.Repo, ref.Number)
	out, err := c.executor.ExecuteWithStdin(string(payload), "gh", "api", "--method", method, path, "--input", "-")
	if err != nil {
		return fmt.Errorf("%s %s: %w\n%s", action, name, err, out)
	}
	return nil
}
