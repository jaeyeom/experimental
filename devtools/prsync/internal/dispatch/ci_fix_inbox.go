package dispatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/google/renameio/v2"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

const (
	inboxSubdirDone  = "done"
	inboxSubdirStale = "stale"
	inboxSubdirBad   = "bad"
)

// InboxRequest is one CI-fix hint request JSON file in ci_fix_inbox.
type InboxRequest struct {
	Repo    string    `json:"repo"`
	PR      int       `json:"pr"`
	HeadSHA string    `json:"head_sha"` //nolint:tagliatelle // inbox wire format
	Hint    string    `json:"hint"`
	Writer  string    `json:"writer,omitempty"`
	Time    time.Time `json:"time,omitempty"`
}

// InboxRecord is the archived JSON written to ci_fix_inbox/done/ with the
// dispatch outcome.
type InboxRecord struct {
	Repo            string    `json:"repo"`
	PR              int       `json:"pr"`
	HeadSHA         string    `json:"head_sha"` //nolint:tagliatelle // inbox wire format
	Hint            string    `json:"hint"`
	Writer          string    `json:"writer,omitempty"`
	Time            time.Time `json:"time,omitempty"`
	Outcome         string    `json:"outcome,omitempty"`
	Detail          string    `json:"detail,omitempty"`
	ObservedHeadSHA string    `json:"observed_head_sha,omitempty"` //nolint:tagliatelle // inbox wire format
	ProcessedAt     time.Time `json:"processed_at,omitempty"`      //nolint:tagliatelle // inbox wire format
}

// InboxEntry is a discovered .json file in ci_fix_inbox.
type InboxEntry struct {
	Name    string
	Path    string
	Request InboxRequest
	Err     error
}

// InboxOptions configures ProcessCIFixInbox.
type InboxOptions struct {
	InboxDir   string
	Doc        scan.Document
	PRs        []string
	SkipPRs    map[string]bool
	RunnerPane string
	Force      bool
	Remote     func(machine string) Herdr
	Waker      Waker
	Opener     func(machine string) TabOpener
	Ledger     *ledger.Store
	Clock      Clock
	Sleeper    Sleeper
}

// ReadCIFixInbox reads all non-dotfile .json files in inboxDir in sorted order.
// Subdirectories (done/, stale/, bad/) and dotfiles (such as .tmp-*) are
// ignored. A missing inboxDir returns nil, nil.
func ReadCIFixInbox(inboxDir string) ([]InboxEntry, error) {
	if inboxDir == "" {
		return nil, nil
	}
	dirEntries, err := os.ReadDir(inboxDir)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil, nil
		}
		return nil, fmt.Errorf("read ci_fix_inbox %s: %w", inboxDir, err)
	}
	var out []InboxEntry
	for _, de := range dirEntries {
		name := de.Name()
		if de.IsDir() || strings.HasPrefix(name, ".") || filepath.Ext(name) != ".json" {
			continue
		}
		path := filepath.Join(inboxDir, name)
		data, err := os.ReadFile(path) //nolint:gosec // configured ci_fix_inbox entry
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				continue
			}
			out = append(out, InboxEntry{Name: name, Path: path, Err: fmt.Errorf("read %s: %w", name, err)})
			continue
		}
		req, parseErr := parseInboxRequest(data)
		out = append(out, InboxEntry{
			Name:    name,
			Path:    path,
			Request: req,
			Err:     parseErr,
		})
	}
	return out, nil
}

func parseInboxRequest(data []byte) (InboxRequest, error) {
	var req InboxRequest
	if err := json.Unmarshal(data, &req); err != nil {
		return InboxRequest{}, fmt.Errorf("parse JSON: %w", err)
	}
	req.Repo = strings.TrimSpace(req.Repo)
	req.HeadSHA = strings.TrimSpace(req.HeadSHA)
	if !validInboxRepo(req.Repo) {
		return InboxRequest{}, errors.New("missing or invalid repo (want owner/repo)")
	}
	if req.PR <= 0 {
		return InboxRequest{}, errors.New("missing or invalid pr (want > 0)")
	}
	if req.HeadSHA == "" {
		return InboxRequest{}, errors.New("missing head_sha")
	}
	if strings.TrimSpace(req.Hint) == "" {
		return InboxRequest{}, errors.New("missing hint")
	}
	return req, nil
}

func validInboxRepo(repo string) bool {
	owner, name, ok := strings.Cut(repo, "/")
	if !ok || owner == "" || name == "" || strings.Contains(name, "/") {
		return false
	}
	return !strings.ContainsAny(repo, " \t\r\n")
}

// ProcessCIFixInbox evaluates and dispatches CI-fix hint requests from
// ci_fix_inbox against Doc, respecting policy_ci_fix (off | dry-run | auto)
// and per-head ledger dedupe.
func ProcessCIFixInbox(ctx context.Context, h Herdr, store StateStore, cfg config.Config, opts InboxOptions, now time.Time) (Document, error) {
	dryRun := cfg.DryRun || cfg.Policy.CIFix == config.PolicyDryRun
	doc := Document{
		GeneratedAt: now.UTC().Format(time.RFC3339),
		DryRun:      dryRun,
		Results:     []Item{},
	}
	inboxDir := opts.InboxDir
	if inboxDir == "" {
		inboxDir = cfg.CIFixInboxDir()
	}
	entries, err := ReadCIFixInbox(inboxDir)
	if err != nil {
		return doc, err
	}
	if len(entries) == 0 {
		return doc, nil
	}
	prFilter, err := buildPRFilter(opts.PRs)
	if err != nil {
		return doc, err
	}
	if cfg.Policy.CIFix == config.PolicyOff {
		doc.Results = policyOffInboxItems(entries, prFilter)
		return doc, nil
	}
	return runInboxEntries(ctx, h, store, cfg, opts, inboxDir, entries, prFilter, dryRun, now, doc)
}

func buildPRFilter(prs []string) (map[string]bool, error) {
	if len(prs) == 0 {
		return nil, nil
	}
	filter := make(map[string]bool, len(prs))
	for _, raw := range prs {
		repo, num, err := ParsePR(raw)
		if err != nil {
			return nil, err
		}
		filter[prKey(repo, num)] = true
	}
	return filter, nil
}

func policyOffInboxItems(entries []InboxEntry, prFilter map[string]bool) []Item {
	var items []Item
	for _, entry := range entries {
		if entry.Err != nil {
			if len(prFilter) == 0 {
				items = append(items, Item{
					Action:    ActionSkippedPolicyOff,
					Kind:      KindCIFix,
					InboxFile: entry.Name,
					Detail:    "policy_ci_fix is off",
				})
			}
			continue
		}
		key := prKey(entry.Request.Repo, entry.Request.PR)
		if len(prFilter) > 0 && !prFilter[key] {
			continue
		}
		items = append(items, Item{
			Repo:      entry.Request.Repo,
			Number:    entry.Request.PR,
			Action:    ActionSkippedPolicyOff,
			Kind:      KindCIFix,
			InboxFile: entry.Name,
			Detail:    "policy_ci_fix is off",
		})
	}
	if items == nil {
		return []Item{}
	}
	return items
}

func runInboxEntries(
	ctx context.Context,
	h Herdr,
	store StateStore,
	cfg config.Config,
	opts InboxOptions,
	inboxDir string,
	entries []InboxEntry,
	prFilter map[string]bool,
	dryRun bool,
	now time.Time,
	doc Document,
) (Document, error) {
	byKey := make(map[string]scan.PR, len(opts.Doc.PRs))
	for _, pr := range opts.Doc.PRs {
		byKey[prKey(pr.Repo, pr.Number)] = pr
	}
	inaccessible := make(map[string]bool, len(opts.Doc.InaccessibleRepos))
	for _, repo := range opts.Doc.InaccessibleRepos {
		inaccessible[repo] = true
	}

	var settledTimeoutErr error
	for _, entry := range entries {
		if err := ctx.Err(); err != nil {
			return doc, fmt.Errorf("ci_fix_inbox: %w", err)
		}
		item, stop, runErr := processOneInboxEntry(ctx, h, store, cfg, opts, inboxDir, entry, prFilter, byKey, inaccessible, dryRun, now)
		if item.Action != "" {
			doc.Results = append(doc.Results, item)
		}
		if stop {
			return doc, runErr
		}
		if errors.Is(runErr, ErrSettleTimeout) && settledTimeoutErr == nil {
			settledTimeoutErr = runErr
		}
	}
	return doc, settledTimeoutErr
}

func processOneInboxEntry(
	ctx context.Context,
	h Herdr,
	store StateStore,
	cfg config.Config,
	opts InboxOptions,
	inboxDir string,
	entry InboxEntry,
	prFilter map[string]bool,
	byKey map[string]scan.PR,
	inaccessible map[string]bool,
	dryRun bool,
	now time.Time,
) (Item, bool, error) {
	if entry.Err != nil {
		item, err := handleBadInboxEntry(ctx, inboxDir, entry, dryRun)
		return item, err != nil, err
	}
	key := prKey(entry.Request.Repo, entry.Request.PR)
	if len(prFilter) > 0 && !prFilter[key] {
		return Item{}, false, nil
	}
	if opts.SkipPRs[key] {
		return Item{
			Repo:      entry.Request.Repo,
			Number:    entry.Request.PR,
			Action:    ActionSkippedBusy,
			Kind:      KindCIFix,
			InboxFile: entry.Name,
			Detail:    "dispatched earlier this round",
		}, false, nil
	}
	pr, ok := byKey[key]
	if staleItem, stale, err := checkInboxPRStale(ctx, inboxDir, entry, pr, ok, inaccessible[entry.Request.Repo], dryRun); err != nil || stale {
		return staleItem, err != nil, err
	}
	if dedupedItem, deduped, err := checkInboxLedgerDedupe(ctx, inboxDir, entry, pr, opts, dryRun, now); err != nil || deduped {
		return dedupedItem, err != nil, err
	}
	return dispatchInboxEntry(ctx, h, store, cfg, opts, inboxDir, entry, pr, dryRun, now)
}

func handleBadInboxEntry(ctx context.Context, inboxDir string, entry InboxEntry, dryRun bool) (Item, error) {
	runlog.FromContext(ctx).Warn("ci_fix_inbox_bad", "file", entry.Name, "error", entry.Err.Error())
	writeStatus(ctx, fmt.Sprintf("ci_fix_inbox: malformed request %s: %v", entry.Name, entry.Err))
	if !dryRun {
		if err := moveToInboxSubdir(entry.Path, inboxDir, inboxSubdirBad, entry.Name); err != nil {
			return Item{}, err
		}
	}
	return Item{
		Action:    ActionBadRequest,
		Kind:      KindCIFix,
		InboxFile: entry.Name,
		Detail:    entry.Err.Error(),
	}, nil
}

func checkInboxPRStale(
	ctx context.Context,
	inboxDir string,
	entry InboxEntry,
	pr scan.PR,
	found bool,
	repoInaccessible bool,
	dryRun bool,
) (Item, bool, error) {
	req := entry.Request
	if !found {
		if repoInaccessible {
			return Item{
				Repo:      req.Repo,
				Number:    req.PR,
				Action:    ActionSkippedNotFound,
				Kind:      KindCIFix,
				InboxFile: entry.Name,
				Detail:    "repository inaccessible",
			}, true, nil
		}
		return archiveStaleInboxEntry(ctx, inboxDir, entry, "pr not found or no longer open", dryRun)
	}
	if pr.State != "" && pr.State != "open" {
		return archiveStaleInboxEntry(ctx, inboxDir, entry, fmt.Sprintf("pr state is %s", pr.State), dryRun)
	}
	if pr.HeadSHA != req.HeadSHA {
		return archiveStaleInboxEntry(ctx, inboxDir, entry, fmt.Sprintf("head changed: inbox=%s current=%s", req.HeadSHA, pr.HeadSHA), dryRun)
	}
	return Item{}, false, nil
}

func archiveStaleInboxEntry(ctx context.Context, inboxDir string, entry InboxEntry, detail string, dryRun bool) (Item, bool, error) {
	runlog.FromContext(ctx).Info("ci_fix_inbox_stale", "file", entry.Name, "repo", entry.Request.Repo, "number", entry.Request.PR, "detail", detail)
	if !dryRun {
		if err := moveToInboxSubdir(entry.Path, inboxDir, inboxSubdirStale, entry.Name); err != nil {
			return Item{}, true, err
		}
	}
	return Item{
		Repo:      entry.Request.Repo,
		Number:    entry.Request.PR,
		Action:    ActionSkippedStale,
		Kind:      KindCIFix,
		InboxFile: entry.Name,
		Detail:    detail,
	}, true, nil
}

func checkInboxLedgerDedupe(
	ctx context.Context,
	inboxDir string,
	entry InboxEntry,
	pr scan.PR,
	opts InboxOptions,
	dryRun bool,
	now time.Time,
) (Item, bool, error) {
	if opts.Force || opts.Ledger == nil {
		return Item{}, false, nil
	}
	key := prKey(pr.Repo, pr.Number)
	already, err := opts.Ledger.HasDispatched(key, pr.HeadSHA, KindCIFix)
	if err != nil {
		return Item{}, false, fmt.Errorf("check ledger: %w", err)
	}
	if !already {
		return Item{}, false, nil
	}
	detail := fmt.Sprintf("already dispatched in ledger for head %s", pr.HeadSHA)
	runlog.FromContext(ctx).Info("ci_fix_inbox_deduped", "file", entry.Name, "pr", key, "head_sha", pr.HeadSHA)
	if !dryRun {
		if err := archiveDoneInboxEntry(entry, inboxDir, ActionSkippedDeduped, detail, pr.HeadSHA, now); err != nil {
			return Item{}, true, err
		}
	}
	return Item{
		Repo:      pr.Repo,
		Number:    pr.Number,
		Action:    ActionSkippedDeduped,
		Kind:      KindCIFix,
		InboxFile: entry.Name,
		Detail:    detail,
	}, true, nil
}

func dispatchInboxEntry(
	ctx context.Context,
	h Herdr,
	store StateStore,
	cfg config.Config,
	opts InboxOptions,
	inboxDir string,
	entry InboxEntry,
	pr scan.PR,
	dryRun bool,
	now time.Time,
) (Item, bool, error) {
	subCfg := cfg
	subCfg.DryRun = dryRun
	subReq := Request{
		Doc: scan.Document{
			GeneratedAt: opts.Doc.GeneratedAt,
			Author:      opts.Doc.Author,
			Repos:       opts.Doc.Repos,
			PRs:         []scan.PR{pr},
		},
		PRs:        []string{prKey(pr.Repo, pr.Number)},
		RunnerPane: opts.RunnerPane,
		CIFix:      true,
		Hint:       entry.Request.Hint,
		Force:      opts.Force,
		Remote:     opts.Remote,
		Waker:      opts.Waker,
		Opener:     opts.Opener,
		Ledger:     opts.Ledger,
		Clock:      opts.Clock,
		Sleeper:    opts.Sleeper,
	}
	subDoc, runErr := Run(ctx, h, store, subCfg, subReq, now)
	if errors.Is(runErr, ErrLeaseRequired) {
		return Item{}, true, runErr
	}
	if len(subDoc.Results) == 0 {
		if runErr != nil {
			return Item{}, true, runErr
		}
		return Item{}, false, nil
	}
	item := subDoc.Results[0]
	item.InboxFile = entry.Name
	if dryRun {
		return item, false, runErr
	}
	if inboxConsumesAction(item.Action) {
		if err := archiveDoneInboxEntry(entry, inboxDir, item.Action, item.Detail, pr.HeadSHA, now); err != nil {
			return item, true, err
		}
	}
	if errors.Is(runErr, ErrTimeout) || errors.Is(runErr, ErrDirtyTree) || errors.Is(runErr, ErrFailed) {
		return item, true, runErr
	}
	return item, false, runErr
}

func inboxConsumesAction(action string) bool {
	switch action {
	case ActionDispatched, ActionDispatchedTimeout, ActionDispatchedBlocked,
		ActionOpenedTabAndDispatched, ActionSkippedDeduped, ActionFailed, ActionOpenTabFailed:
		return true
	default:
		return false
	}
}

func moveToInboxSubdir(srcPath, inboxDir, subdir, filename string) error {
	dstDir := filepath.Join(inboxDir, subdir)
	if err := os.MkdirAll(dstDir, 0o700); err != nil {
		return fmt.Errorf("create %s dir: %w", subdir, err)
	}
	dstPath := uniqueInboxDest(dstDir, filename)
	if err := os.Rename(srcPath, dstPath); err != nil {
		return fmt.Errorf("move %s to %s: %w", filename, subdir, err)
	}
	return nil
}

func archiveDoneInboxEntry(entry InboxEntry, inboxDir, outcome, detail, observedHead string, now time.Time) error {
	dstDir := filepath.Join(inboxDir, inboxSubdirDone)
	if err := os.MkdirAll(dstDir, 0o700); err != nil {
		return fmt.Errorf("create done dir: %w", err)
	}
	rec := InboxRecord{
		Repo:            entry.Request.Repo,
		PR:              entry.Request.PR,
		HeadSHA:         entry.Request.HeadSHA,
		Hint:            entry.Request.Hint,
		Writer:          entry.Request.Writer,
		Time:            entry.Request.Time,
		Outcome:         outcome,
		Detail:          detail,
		ObservedHeadSHA: observedHead,
		ProcessedAt:     now.UTC(),
	}
	data, err := json.MarshalIndent(rec, "", "  ")
	if err != nil {
		return fmt.Errorf("encode done record: %w", err)
	}
	data = append(data, '\n')
	dstPath := uniqueInboxDest(dstDir, entry.Name)
	if err := renameio.WriteFile(dstPath, data, 0o600); err != nil {
		return fmt.Errorf("write done record %s: %w", entry.Name, err)
	}
	if err := os.Remove(entry.Path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("remove inbox file %s: %w", entry.Name, err)
	}
	return nil
}

func uniqueInboxDest(dir, filename string) string {
	candidate := filepath.Join(dir, filename)
	if _, err := os.Stat(candidate); errors.Is(err, fs.ErrNotExist) {
		return candidate
	}
	ext := filepath.Ext(filename)
	stem := strings.TrimSuffix(filename, ext)
	for i := 1; ; i++ {
		next := filepath.Join(dir, fmt.Sprintf("%s-%d%s", stem, i, ext))
		if _, err := os.Stat(next); errors.Is(err, fs.ErrNotExist) {
			return next
		}
	}
}
