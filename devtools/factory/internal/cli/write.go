package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

func newLinkCmd(ctx context.Context) *cobra.Command {
	cmd := &cobra.Command{
		Use:           "link <blocker> <blocked>",
		Short:         "Record that blocker blocks blocked",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			return twoIDs("link", args)
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runLink(ctx, cmd, args[0], args[1])
		},
	}
	return cmd
}

func newUnlinkCmd(ctx context.Context) *cobra.Command {
	cmd := &cobra.Command{
		Use:           "unlink <blocker> <blocked>",
		Short:         "Remove the blocks link from blocker to blocked",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			return twoIDs("unlink", args)
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runUnlink(ctx, cmd, args[0], args[1])
		},
	}
	return cmd
}

func newReleaseCmd(ctx context.Context) *cobra.Command {
	cmd := &cobra.Command{
		Use:           "release <id>",
		Short:         "Undo a claim and mark the latest active claim released",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			return oneID("release", args)
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runRelease(ctx, cmd, args[0])
		},
	}
	return cmd
}

func newFileCmd(ctx context.Context) *cobra.Command {
	var req fileRequest
	cmd := &cobra.Command{
		Use:           "file",
		Short:         "File a follow-up and print only its new id",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("file: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runFile(ctx, cmd, req)
		},
	}
	cmd.Flags().StringVar(&req.title, "title", "", "title of the new item")
	cmd.Flags().StringVar(&req.body, "body", "", "body text")
	cmd.Flags().StringVar(&req.bodyFile, "body-file", "", "path of a file to use as the body")
	cmd.Flags().StringVar(&req.parent, "parent", "", "parent id")
	cmd.Flags().StringVar(&req.project, "project", "", "Jira project key to file in (default: create_project, then the parent's or first linked key's project)")
	cmd.Flags().StringArrayVar(&req.blocks, "blocks", nil, "id this item blocks (repeatable)")
	cmd.Flags().StringArrayVar(&req.blockedBy, "blocked-by", nil, "id that blocks this item (repeatable)")
	cmd.Flags().StringArrayVar(&req.labels, "label", nil, "label to set (repeatable)")
	cmd.Flags().StringVar(&req.source, "source", "", "source key (kind:name) to file in")
	return cmd
}

func oneID(command string, args []string) error {
	if len(args) != 1 {
		return usageError{err: fmt.Errorf("%s: expected one id, found %d", command, len(args))}
	}
	if args[0] == "" {
		return usageError{err: fmt.Errorf("%s: expected a non-empty id, found empty", command)}
	}
	return nil
}

func twoIDs(command string, args []string) error {
	if len(args) != 2 {
		return usageError{err: fmt.Errorf("%s: expected blocker and blocked, found %d arguments", command, len(args))}
	}
	if args[0] == "" || args[1] == "" {
		return usageError{err: fmt.Errorf("%s: expected non-empty blocker and blocked, found %q and %q", command, args[0], args[1])}
	}
	return nil
}

type fileRequest struct {
	title     string
	body      string
	bodyFile  string
	parent    string
	project   string
	blocks    []string
	blockedBy []string
	labels    []string
	source    string
}

func runLink(ctx context.Context, cmd *cobra.Command, blocker, blocked string) error {
	return runWrite(ctx, cmd, "link", func(sess *writeSession) (string, error) {
		err := sess.edge("link", blocker, blocked, func(src source.Source) error {
			return src.Link(sess.ctx, blocker, blocked)
		})
		return fmt.Sprintf("link %s %s", blocker, blocked), err
	})
}

func runUnlink(ctx context.Context, cmd *cobra.Command, blocker, blocked string) error {
	return runWrite(ctx, cmd, "unlink", func(sess *writeSession) (string, error) {
		err := sess.edge("unlink", blocker, blocked, func(src source.Source) error {
			return src.Unlink(sess.ctx, blocker, blocked)
		})
		return fmt.Sprintf("unlink %s %s", blocker, blocked), err
	})
}

func runRelease(ctx context.Context, cmd *cobra.Command, id string) error {
	return runWrite(ctx, cmd, "release", func(sess *writeSession) (string, error) {
		removed, err := sess.release(id)
		if removed {
			return "release " + id + "; removed from the mirror: no longer matches the source query", err
		}
		return "release " + id, err
	})
}

func runFile(ctx context.Context, cmd *cobra.Command, req fileRequest) error {
	body, err := fileBody(cmd, req.body, req.bodyFile)
	if err != nil {
		return err
	}
	req.body = body
	if err := warnLiteralNewline(cmd.ErrOrStderr(), body); err != nil {
		return err
	}
	return runWrite(ctx, cmd, "file", func(sess *writeSession) (string, error) {
		id, err := sess.file(req)
		if err != nil {
			if id == "" {
				return "file " + req.title, err
			}
			return "filed " + id, err
		}
		if _, err := fmt.Fprintln(cmd.OutOrStdout(), id); err != nil {
			return "filed " + id, fmt.Errorf("write file id: %w", err)
		}
		return "filed " + id, nil
	})
}

// literalNewlineWarning is printed when a body has the two characters \n and
// no real newline, usually from --body "a\nb" in a shell.
const literalNewlineWarning = `factory: warning: body contains a literal \n and no newline; use --body-file or $'...' quoting`

// warnLiteralNewline prints literalNewlineWarning. It does not change body.
func warnLiteralNewline(w io.Writer, body string) error {
	return warnLiteralNewlineFor(w, "body", "--body-file", body)
}

func warnLiteralNewlineFor(w io.Writer, field, fileFlag, text string) error {
	if strings.Contains(text, "\n") || !strings.Contains(text, `\n`) {
		return nil
	}
	if _, err := fmt.Fprintf(w, "factory: warning: %s contains a literal \\n and no newline; use %s or $'...' quoting\n", field, fileFlag); err != nil {
		return fmt.Errorf("write warning: %w", err)
	}
	return nil
}

func fileBody(cmd *cobra.Command, body, bodyFile string) (string, error) {
	bodySet := cmd.Flags().Changed("body")
	fileSet := cmd.Flags().Changed("body-file")
	switch {
	case bodySet && fileSet:
		return "", usageError{err: errors.New("file: expected only one of --body and --body-file, found both")}
	case fileSet:
		return readBodyFile(bodyFile)
	default:
		return body, nil
	}
}

func readBodyFile(path string) (string, error) {
	if path == "" {
		return "", usageError{err: errors.New("file: expected a --body-file path, found empty")}
	}
	raw, err := os.ReadFile(path) //nolint:gosec // G304: the path is the user-supplied --body-file.
	if err != nil {
		return "", fmt.Errorf("file --body-file: %w", err)
	}
	return string(raw), nil
}

type writeSession struct {
	ctx    context.Context
	st     *store.Store
	cfg    *config.Config
	srcs   map[string]source.Source
	errOut io.Writer // Warnings.
}

func runWrite(ctx context.Context, cmd *cobra.Command, command string, op func(*writeSession) (string, error)) (err error) {
	sess, err := openWrite(ctx, cmd, command)
	if err != nil {
		return err
	}
	defer func() {
		err = errors.Join(err, sess.close())
	}()
	summary, opErr := op(sess)
	return sess.finish(command, summary, opErr)
}

// openWrite loads the config, store, and sources for one write command.
// link, unlink, file, and release ignore dry_run: the user asked for the write.
func openWrite(ctx context.Context, cmd *cobra.Command, command string) (*writeSession, error) {
	if ctx == nil {
		return nil, fmt.Errorf("%s: expected a context, found nil", command)
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return nil, err
	}
	st, err := openDB(ctx, cmd)
	if err != nil {
		return nil, err
	}
	deps, err := sourceDeps()
	if err != nil {
		return nil, closeStore(st, err)
	}
	srcs, err := buildSources(cfg, deps)
	if err != nil {
		return nil, closeStore(st, err)
	}
	return &writeSession{ctx: ctx, st: st, cfg: cfg, srcs: srcs, errOut: cmd.ErrOrStderr()}, nil
}

func closeStore(st *store.Store, err error) error {
	if st == nil {
		return err
	}
	if cerr := st.Close(); cerr != nil {
		return errors.Join(err, fmt.Errorf("close store: %w", cerr))
	}
	return err
}

func (w *writeSession) close() error {
	if w == nil || w.st == nil {
		return nil
	}
	if err := w.st.Close(); err != nil {
		return fmt.Errorf("close store: %w", err)
	}
	return nil
}

func (w *writeSession) finish(command, summary string, opErr error) error {
	if opErr != nil {
		summary = summary + ": " + opErr.Error()
	}
	detail, err := writeDetailJSON(oneLine(summary), opErr)
	if err != nil {
		return errors.Join(opErr, err)
	}
	outcome := "ok"
	if opErr != nil {
		outcome = "error"
	}
	recErr := w.st.AppendRun(w.ctx, store.Run{
		StartedAt:  time.Now().UTC(),
		Command:    command,
		Outcome:    outcome,
		DetailJSON: detail,
	})
	return errors.Join(opErr, recErr)
}

func writeDetailJSON(summary string, opErr error) (string, error) {
	row := struct {
		Summary string `json:"summary"`
		Error   string `json:"error,omitempty"`
	}{Summary: summary}
	if opErr != nil {
		row.Error = opErr.Error()
	}
	raw, err := json.Marshal(row)
	if err != nil {
		return "", fmt.Errorf("write detail: %w", err)
	}
	return string(raw), nil
}

func oneLine(text string) string {
	text = strings.ReplaceAll(text, "\r\n", "; ")
	text = strings.ReplaceAll(text, "\n", "; ")
	text = strings.ReplaceAll(text, "\r", "; ")
	return text
}

func (w *writeSession) edge(command, blocker, blocked string, op func(source.Source) error) error {
	left, err := w.resolve(blocker)
	if err != nil {
		return err
	}
	right, err := w.resolve(blocked)
	if err != nil {
		return err
	}
	if left.key != right.key {
		return usageError{err: fmt.Errorf("%s: expected %s and %s in one source, found %s and %s", command, blocker, blocked, left.key, right.key)}
	}
	if err := op(left.src); err != nil {
		return err
	}
	return w.refresh(left.key, blocker, blocked)
}

// release undoes the claim in the source, marks the latest active claim
// released, and refreshes the item's mirror row. When the item no longer
// matches the source query, for example an assignee query after the
// assignee is cleared, the row is removed so next stops listing it.
// It reports whether the row was removed.
func (w *writeSession) release(id string) (bool, error) {
	resolved, err := w.resolve(id)
	if err != nil {
		return false, err
	}
	if err := resolved.src.Release(w.ctx, id); err != nil {
		return false, err //nolint:wrapcheck // propagate context cancellation
	}
	claimErr := w.releaseClaim(resolved.key, id)
	removed, refreshErr := syncer.Refresh(w.ctx, w.st, resolved.key, resolved.src, id)
	return removed, errors.Join(claimErr, refreshErr)
}

func (w *writeSession) releaseClaim(sourceName, id string) error {
	claims, err := w.st.ActiveClaims(w.ctx)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	claim, ok := latestActive(claims, sourceName, id)
	if !ok {
		return nil
	}
	if err := w.st.UpdateClaim(w.ctx, claim.Source, claim.ID, claim.ClaimedAt, store.Released, claim.Detail, claim.TabID, claim.WorkspaceID); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	w.releaseWorktree(claim)
	return nil
}

func (w *writeSession) warnWorktree(format string, args ...any) {
	if _, err := fmt.Fprintf(w.errOut, format, args...); err != nil {
		// Best-effort warning output; release already succeeded in the store.
		return
	}
}

func (w *writeSession) releaseWorktree(claim store.Claim) {
	if claim.Worktree == "" {
		return
	}
	lane, err := claimLane(w.cfg, claim.Lane)
	if err != nil {
		w.warnWorktree("warning: left worktree %s on disk: %v\n", claim.Worktree, err)
		return
	}
	if lane.Remote() {
		return
	}
	repoDir := lane.Checkout
	if repoDir == "" {
		srcCfg, err := configFor(w.cfg, claim.Source)
		if err != nil {
			w.warnWorktree("warning: left worktree %s on disk: %v\n", claim.Worktree, err)
			return
		}
		repoDir = srcCfg.Cwd
	}
	exec := newCommandExec()
	if exec == nil {
		w.warnWorktree("warning: left worktree %s on disk: command executor unavailable\n", claim.Worktree)
		return
	}
	out, err := gate.GitStatus(exec)(w.ctx, claim.Worktree)
	if err != nil {
		w.warnWorktree("warning: left worktree %s on disk: %v\n", claim.Worktree, err)
		return
	}
	if strings.TrimSpace(out) != "" {
		w.warnWorktree("warning: left dirty worktree %s on disk (uncommitted changes)\n", claim.Worktree)
		return
	}
	if err := gate.GitWorktreeRemove(exec)(w.ctx, repoDir, claim.Worktree); err != nil {
		w.warnWorktree("warning: remove worktree %s failed: %v\n", claim.Worktree, err)
	}
}

func latestActive(claims []store.Claim, sourceName, id string) (store.Claim, bool) {
	var found store.Claim
	ok := false
	for _, claim := range claims {
		if claim.Source != sourceName || claim.ID != id {
			continue
		}
		if !ok || claim.ClaimedAt.After(found.ClaimedAt) {
			found = claim
			ok = true
		}
	}
	return found, ok
}

func (w *writeSession) file(req fileRequest) (string, error) {
	if req.title == "" {
		return "", usageError{err: errors.New("file: expected a non-empty --title, found empty")}
	}
	key, err := w.fileSource(req)
	if err != nil {
		return "", err
	}
	if err := w.warnPublic(key); err != nil {
		return "", err
	}
	if err := w.warnProject(key, req.project); err != nil {
		return "", err
	}
	src := w.srcs[key]
	if src == nil {
		return "", fmt.Errorf("source %s: expected a source, found nil", key)
	}
	id, err := src.Create(w.ctx, source.CreateRequest{
		Title:     req.title,
		Body:      req.body,
		Parent:    req.parent,
		Project:   req.project,
		Labels:    req.labels,
		Blocks:    req.blocks,
		BlockedBy: req.blockedBy,
	})
	if err != nil {
		return id, err //nolint:wrapcheck // propagate context cancellation
	}
	if id == "" {
		return "", errors.New("file: expected a new id, found empty")
	}
	if err := w.refresh(key, fileIDs(id, req)...); err != nil {
		return id, fmt.Errorf("file: created %s: %w", id, err)
	}
	return id, nil
}

func fileIDs(id string, req fileRequest) []string {
	return slices.Concat([]string{id}, req.blocks, req.blockedBy)
}

// fileSource is the one source file may write to. A worker's item source,
// FACTORY_ITEM_SOURCE from factory start, is the only one: a different
// --source is refused outright, with no override, and a source this
// machine does not configure is refused too. Without FACTORY_ITEM_SOURCE
// (a coordinator), --source is required; there is no default source.
func (w *writeSession) fileSource(req fileRequest) (string, error) {
	item := w.cfg.ItemSource
	if item == "" {
		if req.source == "" {
			return "", usageError{err: fmt.Errorf("file: --source required when %s is not set: expected one of %s, found none; factory never picks a source for you", config.ItemSourceEnv, w.sourceList())}
		}
		return w.explicitSource(req)
	}
	if _, ok := w.srcs[item]; !ok {
		return "", fmt.Errorf("file: source %s is not configured on this machine; report back to your coordinator instead", item)
	}
	if req.source != "" && req.source != item {
		return "", fmt.Errorf("file: this item is from %s; factory never files into %s", item, req.source)
	}
	req.source = item
	return w.explicitSource(req)
}

// warnPublic prints a warning when key is a public GitHub repository.
// Filing there is allowed only because it is the item's own source.
// A visibility that cannot be read is said too.
// warnProject warns that --project is ignored by a source with no projects.
func (w *writeSession) warnProject(key, project string) error {
	if project == "" {
		return nil
	}
	cfg, err := configFor(w.cfg, key)
	if err != nil || cfg.Kind == "jira" {
		return err
	}
	if _, err := fmt.Fprintf(w.errOut, "factory: warning: --project %s is ignored for %s, which has no projects\n", project, key); err != nil {
		return fmt.Errorf("write warning: %w", err)
	}
	return nil
}

func (w *writeSession) warnPublic(key string) error {
	cfg, err := configFor(w.cfg, key)
	if err != nil || cfg.Kind != "github" {
		return err
	}
	exec := newCommandExec()
	if exec == nil {
		return errors.New("file: expected an executor, found nil")
	}
	client, err := ghClient(exec, cfg.GHBin)
	if err != nil {
		return err
	}
	var line string
	visibility, err := client.Visibility(w.ctx, cfg.Repo)
	switch {
	case err != nil:
		line = fmt.Sprintf("factory: warning: could not read whether %s is public (%v); the follow-up may be public", cfg.Repo, err)
	case visibility == gh.VisibilityPublic:
		line = fmt.Sprintf("factory: warning: %s is a public repository; this follow-up will be public", cfg.Repo)
	default:
		return nil
	}
	if _, err := fmt.Fprintln(w.errOut, line); err != nil {
		return fmt.Errorf("write warning: %w", err)
	}
	return nil
}

func (w *writeSession) explicitSource(req fileRequest) (string, error) {
	if _, ok := w.srcs[req.source]; !ok {
		return "", usageError{err: fmt.Errorf("file --source: expected one of %s, found %q", w.sourceList(), req.source)}
	}
	if err := w.idsInSource(req.source, req.blocks, req.blockedBy); err != nil {
		return "", err
	}
	return req.source, nil
}

func (w *writeSession) idsInSource(key string, groups ...[]string) error {
	for _, group := range groups {
		for _, id := range group {
			resolved, err := w.resolve(id)
			if err != nil {
				return err
			}
			if resolved.key != key {
				return usageError{err: fmt.Errorf("file: expected %s in %s, found it in %s", id, key, resolved.key)}
			}
		}
	}
	return nil
}

func (w *writeSession) sourceList() string {
	return formatKeyList(sortedSourceKeys(w.srcs))
}

type resolvedID struct {
	key string
	src source.Source
}

func (w *writeSession) resolve(id string) (resolvedID, error) {
	if id == "" {
		return resolvedID{}, usageError{err: errors.New("item: expected a non-empty id, found empty")}
	}
	items, err := w.st.FindItem(w.ctx, id)
	if err != nil {
		return resolvedID{}, err //nolint:wrapcheck // propagate context cancellation
	}
	switch len(items) {
	case 0:
		return w.resolveLive(id)
	case 1:
		return w.byKey(id, items[0].Source)
	default:
		names := make([]string, len(items))
		for i, it := range items {
			names[i] = it.Source
		}
		return resolvedID{}, usageError{err: ambiguousItem(id, names)}
	}
}

func (w *writeSession) resolveLive(id string) (resolvedID, error) {
	candidates, err := w.ownerCandidates(id)
	if err != nil {
		return resolvedID{}, err
	}
	matched, err := w.probe(candidates, id)
	if err != nil {
		return resolvedID{}, err
	}
	switch len(matched) {
	case 0:
		return resolvedID{}, usageError{err: fmt.Errorf("item %s: expected one item, found none", id)}
	case 1:
		return w.byKey(id, matched[0])
	default:
		return resolvedID{}, usageError{err: ambiguousItem(id, matched)}
	}
}

func (w *writeSession) probe(candidates []resolvedID, id string) ([]string, error) {
	matched := make([]string, 0, 1)
	for _, candidate := range candidates {
		_, err := candidate.src.Get(w.ctx, id)
		if err == nil {
			matched = append(matched, candidate.key)
			continue
		}
		if errors.Is(err, source.ErrNotFound) {
			continue
		}
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	return matched, nil
}

func (w *writeSession) ownerCandidates(id string) ([]resolvedID, error) {
	if source.IsJiraKey(id) {
		return w.sourcesOfKind("jira", "")
	}
	if !strings.Contains(id, "#") {
		return nil, usageError{err: fmt.Errorf("item %q: expected a Jira key or owner/repo#number, found %q", id, id)}
	}
	repo, _, err := source.ParseGitHubID(id)
	if err != nil {
		return nil, usageError{err: err}
	}
	return w.sourcesOfKind("github", repo)
}

func (w *writeSession) sourcesOfKind(kind, repo string) ([]resolvedID, error) {
	out := make([]resolvedID, 0, len(w.srcs))
	for _, key := range sortedSourceKeys(w.srcs) {
		cfg, err := configFor(w.cfg, key)
		if err != nil {
			return nil, err
		}
		if cfg.Kind != kind || (repo != "" && cfg.Repo != repo) {
			continue
		}
		src := w.srcs[key]
		if src == nil {
			return nil, fmt.Errorf("source %s: expected a source, found nil", key)
		}
		out = append(out, resolvedID{key: key, src: src})
	}
	return out, nil
}

func (w *writeSession) byKey(id, key string) (resolvedID, error) {
	src, ok := w.srcs[key]
	if !ok {
		return resolvedID{}, fmt.Errorf("item %s: expected a configured source, found %s", id, key)
	}
	if src == nil {
		return resolvedID{}, fmt.Errorf("source %s: expected a source, found nil", key)
	}
	return resolvedID{key: key, src: src}, nil
}

func (w *writeSession) refresh(key string, ids ...string) error {
	src, ok := w.srcs[key]
	if !ok || src == nil {
		return fmt.Errorf("source %s: expected a configured source, found none", key)
	}
	unique, err := uniqueRefreshIDs(ids)
	if err != nil {
		return err
	}
	items := make([]source.Item, 0, len(unique))
	for _, id := range unique {
		it, err := src.Get(w.ctx, id)
		if err != nil {
			return err //nolint:wrapcheck // propagate context cancellation
		}
		if it.ID != id {
			return fmt.Errorf("get %s: expected id %q, found %q", id, id, it.ID)
		}
		if it.Source != key {
			return fmt.Errorf("get %s: expected source %q, found %q", id, key, it.Source)
		}
		items = append(items, it)
	}
	return w.st.UpsertItems(w.ctx, key, items) //nolint:wrapcheck // propagate context cancellation
}

func uniqueRefreshIDs(ids []string) ([]string, error) {
	seen := make(map[string]struct{}, len(ids))
	out := make([]string, 0, len(ids))
	for _, id := range ids {
		if id == "" {
			return nil, errors.New("refresh: expected a non-empty id, found empty")
		}
		if _, ok := seen[id]; ok {
			continue
		}
		seen[id] = struct{}{}
		out = append(out, id)
	}
	return out, nil
}

func ambiguousItem(id string, sources []string) error {
	return fmt.Errorf("item %s: expected one source, found %s", id, strings.Join(sources, ", "))
}

func sortedSourceKeys(srcs map[string]source.Source) []string {
	keys := make([]string, 0, len(srcs))
	for key := range srcs {
		keys = append(keys, key)
	}
	slices.Sort(keys)
	return keys
}

func formatKeyList(keys []string) string {
	if len(keys) == 0 {
		return "(none)"
	}
	return strings.Join(keys, ", ")
}
