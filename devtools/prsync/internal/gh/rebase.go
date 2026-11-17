package gh

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"strings"

	executor "github.com/jaeyeom/go-cmdexec"
)

// ErrConflict is returned when GitHub cannot rebase a branch because
// it conflicts with its base.
var ErrConflict = errors.New("rebase conflict")

const prIDQuery = `query($owner:String!,$repo:String!,$num:Int!){repository(owner:$owner,name:$repo){pullRequest(number:$num){id}}}`

// updateBranchMutation rebases a pull request's branch onto its base
// on GitHub. expectedHeadOid makes it refuse a branch that moved since
// the caller looked.
const updateBranchMutation = `mutation($id:ID!,$head:GitObjectID!){updatePullRequestBranch(input:{pullRequestId:$id,expectedHeadOid:$head,updateMethod:REBASE}){pullRequest{headRefOid}}}`

func prIDArgs(owner, repo string, number int) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + prIDQuery,
		"-F", "owner=" + owner,
		"-F", "repo=" + repo,
		"-F", "num=" + strconv.Itoa(number),
	}
}

func rebaseBranchArgs(id, head string) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + updateBranchMutation,
		"-f", "id=" + id,
		"-f", "head=" + head,
	}
}

// RebaseBranch rebases pull request number onto its base on GitHub,
// with no checkout, and returns the new head. head is the commit the
// caller expects at the branch tip; a branch that moved is refused. A
// conflict is ErrConflict. The rebase is pushed once: it is never
// retried, even after a transient error.
func (c *Client) RebaseBranch(ctx context.Context, repo string, number int, head string) (string, error) {
	owner, name, err := splitOwnerRepo(repo)
	if err != nil {
		return "", err
	}
	result, err := c.executeChecked(ctx, defaultCallTimeout, validJSONOutput, prIDArgs(owner, name, number)...)
	if err != nil {
		return "", err
	}
	if result.ExitCode != 0 {
		return "", graphqlExecError(result)
	}
	data, err := decodeGraphQLData(result.Output, "pull request id")
	if err != nil {
		return "", err
	}
	var idPayload struct {
		Repository struct {
			PullRequest *struct {
				ID string `json:"id"`
			} `json:"pullRequest"`
		} `json:"repository"`
	}
	if err := json.Unmarshal(data, &idPayload); err != nil {
		return "", fmt.Errorf("decode pull request id: %w", err)
	}
	if idPayload.Repository.PullRequest == nil || idPayload.Repository.PullRequest.ID == "" {
		return "", fmt.Errorf("%w: %s#%d", ErrNotFound, repo, number)
	}
	return c.updateBranch(ctx, idPayload.Repository.PullRequest.ID, head)
}

func (c *Client) updateBranch(ctx context.Context, id, head string) (string, error) {
	if err := CallBudgetFrom(ctx).Take(); err != nil {
		return "", err
	}
	result, err := c.exec.Execute(ctx, executor.ToolConfig{
		Command: c.bin,
		Args:    rebaseBranchArgs(id, head),
		Timeout: defaultCallTimeout,
	})
	if err != nil {
		return "", runErr(c.bin, err)
	}
	text := result.Stderr + "\n" + result.Output
	if strings.Contains(strings.ToLower(text), "conflict") {
		return "", fmt.Errorf("%w: %s", ErrConflict, strings.TrimSpace(text))
	}
	if result.ExitCode != 0 {
		return "", &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
	}
	data, err := decodeGraphQLData(result.Output, "update branch")
	if err != nil {
		return "", err
	}
	var payload struct {
		UpdatePullRequestBranch *struct {
			PullRequest struct {
				HeadRefOid string `json:"headRefOid"`
			} `json:"pullRequest"`
		} `json:"updatePullRequestBranch"`
	}
	if err := json.Unmarshal(data, &payload); err != nil {
		return "", fmt.Errorf("decode update branch: %w", err)
	}
	if payload.UpdatePullRequestBranch == nil {
		return "", errors.New("update branch: no pull request returned")
	}
	return payload.UpdatePullRequestBranch.PullRequest.HeadRefOid, nil
}
