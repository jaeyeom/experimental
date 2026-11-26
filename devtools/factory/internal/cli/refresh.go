package cli

import (
	"context"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/backlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

// nowUTC is the clock mirror age and auto-sync share.
// Tests replace it.
var nowUTC = func() time.Time { return time.Now().UTC() }

// refreshBeforeRead syncs each selected source older than its max_age.
// noSync, or a zero TTL, skips that refresh. A failed sync is a warning.
// The command still answers from the mirror.
func refreshBeforeRead(ctx context.Context, cmd *cobra.Command, cfg *config.Config, st *store.Store, only string, noSync bool) error {
	if noSync {
		return nil
	}
	cfgs, err := sourcesFor(cfg, only)
	if err != nil {
		return err
	}
	now := nowUTC()
	stale, err := staleSources(ctx, st, cfgs, cfg.Sync.MaxAge, now)
	if err != nil {
		return err
	}
	if len(stale) == 0 {
		return nil
	}
	return autoSync(ctx, cmd, cfg, st, stale, now)
}

func staleSources(ctx context.Context, st *store.Store, cfgs []config.Source, top time.Duration, now time.Time) ([]config.Source, error) {
	var stale []config.Source
	for i := range cfgs {
		ttl := cfgs[i].EffectiveMaxAge(top)
		if ttl <= 0 {
			continue
		}
		key, err := sourceKey(cfgs[i].Kind, cfgs[i].Name)
		if err != nil {
			return nil, err
		}
		last, err := st.LastSync(ctx, key)
		if err != nil {
			return nil, err //nolint:wrapcheck // propagate context cancellation
		}
		if sourceStale(last, now, ttl) {
			stale = append(stale, cfgs[i])
		}
	}
	return stale, nil
}

func sourceStale(last, now time.Time, ttl time.Duration) bool {
	if ttl <= 0 {
		return false
	}
	if last.IsZero() {
		return true
	}
	age := now.Sub(last)
	return age > ttl
}

func autoSync(ctx context.Context, cmd *cobra.Command, cfg *config.Config, st *store.Store, stale []config.Source, now time.Time) error {
	deps, err := sourceDeps()
	if err != nil {
		return err
	}
	built, err := buildSources(cfg, deps)
	if err != nil {
		return err
	}
	selected := make(map[string]source.Source, len(stale))
	for i := range stale {
		key, err := sourceKey(stale[i].Kind, stale[i].Name)
		if err != nil {
			return err
		}
		src, ok := built[key]
		if !ok {
			return fmt.Errorf("auto-sync %s: expected a source, found none", key)
		}
		selected[key] = src
	}
	results, syncErr := syncer.Sync(ctx, st, selected, now, syncer.Options{
		FullSyncEvery:   cfg.Defaults.FullSyncEvery,
		AfterSync:       backlog.Hook(st, cfg),
		ExternalLookups: cfg.ExternalLookups(),
	})
	if syncErr != nil && len(results) == 0 {
		return syncErr //nolint:wrapcheck // propagate context cancellation
	}
	for _, res := range results {
		if res.Err == nil {
			continue
		}
		if err := warnAutoSync(ctx, cmd, st, res, now); err != nil {
			return err
		}
	}
	return writeSyncWarnings(cmd.ErrOrStderr(), "auto-sync", results)
}

func warnAutoSync(ctx context.Context, cmd *cobra.Command, st *store.Store, res syncer.Result, now time.Time) error {
	last, err := st.LastSync(ctx, res.Source)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	_, werr := fmt.Fprintf(cmd.ErrOrStderr(), "warning: auto-sync %s failed, mirror age %s: %s\n", res.Source, ageText(last, now), res.Err.Error())
	if werr != nil {
		return fmt.Errorf("write auto-sync warning: %w", werr)
	}
	return nil
}

func writeMirrorAge(ctx context.Context, cmd *cobra.Command, st *store.Store, cfgs []config.Source, top time.Duration, asJSON bool) error {
	line, err := mirrorAgeLine(ctx, st, cfgs, top, nowUTC())
	if err != nil {
		return err
	}
	w := cmd.OutOrStdout()
	if asJSON {
		w = cmd.ErrOrStderr()
	}
	return writeLine(w, line)
}

func writeLine(w io.Writer, line string) error {
	if _, err := fmt.Fprintln(w, line); err != nil {
		return fmt.Errorf("write mirror age: %w", err)
	}
	return nil
}

func mirrorAgeLine(ctx context.Context, st *store.Store, cfgs []config.Source, top time.Duration, now time.Time) (string, error) {
	unknown := false
	stale := false
	var oldest time.Duration
	seen := false
	for i := range cfgs {
		key, err := sourceKey(cfgs[i].Kind, cfgs[i].Name)
		if err != nil {
			return "", err
		}
		last, err := st.LastSync(ctx, key)
		if err != nil {
			return "", err //nolint:wrapcheck // propagate context cancellation
		}
		ttl := cfgs[i].EffectiveMaxAge(top)
		if last.IsZero() {
			unknown = true
			if ttl > 0 {
				stale = true
			}
			continue
		}
		age := ageOf(last, now)
		if !seen || age > oldest {
			oldest = age
			seen = true
		}
		if ttl > 0 && age > ttl {
			stale = true
		}
	}
	return formatMirrorAge(unknown, stale, oldest, seen), nil
}

func formatMirrorAge(unknown, stale bool, oldest time.Duration, seen bool) string {
	text := "unknown"
	if !unknown && seen {
		text = formatAge(oldest)
	}
	if stale {
		return "mirror age " + text + " (stale)"
	}
	return "mirror age " + text
}

func ageText(last, now time.Time) string {
	if last.IsZero() {
		return "unknown"
	}
	return formatAge(ageOf(last, now))
}

func ageOf(last, now time.Time) time.Duration {
	age := now.Sub(last)
	if age < 0 {
		return 0
	}
	return age
}

// formatAge prints a duration the way the mirror age line does.
// Zero seconds are dropped when a larger unit is present, so 3h12m0s is 3h12m.
func formatAge(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	d = d.Truncate(time.Second)
	if d == 0 {
		return "0s"
	}
	text := d.String()
	if strings.HasSuffix(text, "m0s") || strings.HasSuffix(text, "h0s") {
		return strings.TrimSuffix(text, "0s")
	}
	return text
}
