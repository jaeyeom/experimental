package start

import (
	"context"
	"errors"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
)

// HerdrAPI is the herdr surface Start calls.
// FindWorkspace lists workspaces and resolves one label. Tests fake this
// instead of running herdr.
type HerdrAPI interface {
	Workspaces(ctx context.Context) ([]herdr.Workspace, error)
	Tabs(ctx context.Context) ([]herdr.Tab, error)
	FindWorkspace(ctx context.Context, label string) (herdr.Workspace, bool, error)
	// env entries are KEY=VALUE for the new pane's shell, such as FACTORY_ITEM_SOURCE.
	CreateWorkspace(ctx context.Context, label, cwd string, env []string) (herdr.Workspace, herdr.Pane, error)
	CreateTab(ctx context.Context, workspaceID, label, cwd string, env []string) (herdr.Tab, herdr.Pane, error)
	StartAgent(ctx context.Context, name, kind, paneID string, timeout time.Duration, agentArgs []string) error
	Prompt(ctx context.Context, target, text string, until []string, timeout time.Duration) error
	// WaitAgent returns the agent_status it matched among until.
	WaitAgent(ctx context.Context, target string, until []string, timeout time.Duration) (string, error)
	// ReadAgent returns the agent's visible screen as text.
	ReadAgent(ctx context.Context, target string, lines int) (string, error)
	SendKeys(ctx context.Context, target string, keys ...string) error
	// CloseTab closes a tab this start opened when the agent never ran.
	CloseTab(ctx context.Context, tabID string) (string, error)
}

// ClientHerdr adapts a herdr client to HerdrAPI.
type ClientHerdr struct {
	client *herdr.Client
}

// NewHerdr returns a ClientHerdr backed by client.
func NewHerdr(client *herdr.Client) (ClientHerdr, error) {
	if client == nil {
		return ClientHerdr{}, errors.New("herdr: expected a client, found nil")
	}
	return ClientHerdr{client: client}, nil
}

func (c ClientHerdr) Workspaces(ctx context.Context) ([]herdr.Workspace, error) {
	return c.client.Workspaces(ctx) //nolint:wrapcheck // propagate context cancellation
}

func (c ClientHerdr) Tabs(ctx context.Context) ([]herdr.Tab, error) {
	return c.client.Tabs(ctx) //nolint:wrapcheck // propagate context cancellation
}

func (c ClientHerdr) FindWorkspace(ctx context.Context, label string) (herdr.Workspace, bool, error) {
	list, err := c.client.Workspaces(ctx)
	if err != nil {
		return herdr.Workspace{}, false, err //nolint:wrapcheck // propagate context cancellation
	}
	return herdr.FindWorkspace(list, label) //nolint:wrapcheck // thin adapter to herdr.Client
}

func (c ClientHerdr) CreateWorkspace(ctx context.Context, label, cwd string, env []string) (herdr.Workspace, herdr.Pane, error) {
	return c.client.CreateWorkspace(ctx, label, cwd, env) //nolint:wrapcheck // propagate context cancellation
}

func (c ClientHerdr) CreateTab(ctx context.Context, workspaceID, label, cwd string, env []string) (herdr.Tab, herdr.Pane, error) {
	return c.client.CreateTab(ctx, workspaceID, label, cwd, env) //nolint:wrapcheck // propagate context cancellation
}

func (c ClientHerdr) StartAgent(ctx context.Context, name, kind, paneID string, timeout time.Duration, agentArgs []string) error {
	return c.client.StartAgent(ctx, name, kind, paneID, timeout, agentArgs) //nolint:wrapcheck // propagate context cancellation
}

func (c ClientHerdr) Prompt(ctx context.Context, target, text string, until []string, timeout time.Duration) error {
	return c.client.Prompt(ctx, target, text, until, timeout) //nolint:wrapcheck // propagate context cancellation
}

func (c ClientHerdr) WaitAgent(ctx context.Context, target string, until []string, timeout time.Duration) (string, error) {
	return c.client.WaitAgent(ctx, target, until, timeout) //nolint:wrapcheck // propagate context cancellation
}

func (c ClientHerdr) ReadAgent(ctx context.Context, target string, lines int) (string, error) {
	return c.client.ReadAgent(ctx, target, lines) //nolint:wrapcheck // propagate context cancellation
}

func (c ClientHerdr) SendKeys(ctx context.Context, target string, keys ...string) error {
	return c.client.SendKeys(ctx, target, keys...) //nolint:wrapcheck // propagate context cancellation
}

func (c ClientHerdr) CloseTab(ctx context.Context, tabID string) (string, error) {
	return c.client.CloseTab(ctx, tabID) //nolint:wrapcheck // propagate context cancellation
}
