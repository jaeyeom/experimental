package authwatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func makeCredsPayload(accessExp, refreshExp time.Time, accessTok, refreshTok string) []byte {
	return []byte(fmt.Sprintf(`{
		"claudeAiOauth": {
			"accessToken": %q,
			"refreshToken": %q,
			"expiresAt": %d,
			"refreshTokenExpiresAt": %d
		}
	}`, accessTok, refreshTok, accessExp.UnixMilli(), refreshExp.UnixMilli()))
}

func TestCheckLevelsNotificationsAndRestart(t *testing.T) {
	now := time.Date(2026, 11, 18, 12, 0, 0, 0, time.UTC)
	statePath := filepath.Join(t.TempDir(), "state.json")

	const secretAccess = "sk-ant-oat01-SECRET-ACCESS-TOKEN"   //nolint:gosec // G101: test fixture
	const secretRefresh = "sk-ant-ort01-SECRET-REFRESH-TOKEN" //nolint:gosec // G101: test fixture

	var currentPayload []byte
	var currentErr error
	var notifications [][]string

	cfg := DefaultConfig()
	cfg.Notify = []string{"my-notify", "{profile}", "{level}", "{time_left}", "{since}"}

	runCheck := func(at time.Time) Report {
		st, err := LoadState(statePath)
		if err != nil {
			t.Fatal(err)
		}
		rep, next := Check(context.Background(), Input{
			Config:  cfg,
			State:   st,
			Now:     at,
			BaseEnv: []string{"HOME=/tmp/home"},
			ReadStore: func(_ context.Context, _ ProfileConfig, _ []string) ([]byte, error) {
				if currentErr != nil {
					return nil, currentErr
				}
				return currentPayload, nil
			},
			Notify: func(_ context.Context, argv, env []string) error {
				for _, a := range argv {
					if strings.Contains(a, secretAccess) || strings.Contains(a, secretRefresh) {
						t.Fatalf("token leaked in notifier argv: %v", argv)
					}
				}
				for _, e := range env {
					if strings.Contains(e, secretAccess) || strings.Contains(e, secretRefresh) {
						t.Fatalf("token leaked in notifier env: %v", env)
					}
				}
				notifications = append(notifications, argv)
				return nil
			},
		})
		if err := SaveState(statePath, next); err != nil {
			t.Fatal(err)
		}
		stateBytes, err := os.ReadFile(statePath)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Contains(string(stateBytes), secretAccess) || strings.Contains(string(stateBytes), secretRefresh) {
			t.Fatalf("token leaked in state file: %s", stateBytes)
		}
		return rep
	}

	// 1. Baseline ok: does not notify.
	currentPayload = makeCredsPayload(now.Add(4*time.Hour), now.Add(10*24*time.Hour), secretAccess, secretRefresh)
	rep := runCheck(now)
	if rep.Profiles[0].Level != LevelOK || len(notifications) != 0 {
		t.Fatalf("baseline ok: got %+v, notifications=%v", rep.Profiles[0], notifications)
	}

	// 2. Warn (within 72h): notifies once.
	now = now.Add(1 * time.Hour)
	currentPayload = makeCredsPayload(now.Add(4*time.Hour), now.Add(70*time.Hour), secretAccess, secretRefresh)
	rep = runCheck(now)
	if rep.Profiles[0].Level != LevelWarn || len(notifications) != 1 {
		t.Fatalf("warn: got %+v, notifications=%v", rep.Profiles[0], notifications)
	}
	// Restart / re-check at same level: does NOT notify again.
	now = now.Add(5 * time.Minute)
	rep = runCheck(now)
	if rep.Profiles[0].Level != LevelWarn || len(notifications) != 1 {
		t.Fatalf("warn repeat: got %+v, notifications=%v", rep.Profiles[0], notifications)
	}

	// 3. Critical (within 24h): notifies once.
	now = now.Add(48 * time.Hour)
	currentPayload = makeCredsPayload(now.Add(4*time.Hour), now.Add(20*time.Hour), secretAccess, secretRefresh)
	rep = runCheck(now)
	if rep.Profiles[0].Level != LevelCritical || len(notifications) != 2 {
		t.Fatalf("critical: got %+v, notifications=%v", rep.Profiles[0], notifications)
	}

	// 4. Access token expires; 9m past expiry is NOT failing yet, 10m past expiry IS failing with start time.
	accessExp := now.Add(1 * time.Hour)
	currentPayload = makeCredsPayload(accessExp, now.Add(100*time.Hour), secretAccess, secretRefresh)
	rep = runCheck(accessExp.Add(9 * time.Minute))
	if rep.Profiles[0].Level != LevelOK {
		t.Fatalf("expected ok at +9m, got %s", rep.Profiles[0].Level)
	}

	failStart := accessExp.Add(10 * time.Minute)
	rep = runCheck(failStart)
	if rep.Profiles[0].Level != LevelFailing {
		t.Fatalf("expected failing at +10m, got %s", rep.Profiles[0].Level)
	}
	if !rep.Profiles[0].Since.Equal(failStart) {
		t.Errorf("failing since = %v, want %v", rep.Profiles[0].Since, failStart)
	}
	// Still failing 5m later: since is preserved, no duplicate notification.
	countBefore := len(notifications)
	rep = runCheck(failStart.Add(5 * time.Minute))
	if rep.Profiles[0].Level != LevelFailing || !rep.Profiles[0].Since.Equal(failStart) || len(notifications) != countBefore {
		t.Fatalf("failing repeat: got %+v, notifications=%d", rep.Profiles[0], len(notifications))
	}

	// 5. Recovery to ok: notifies once.
	recTime := failStart.Add(10 * time.Minute)
	currentPayload = makeCredsPayload(recTime.Add(4*time.Hour), recTime.Add(30*24*time.Hour), secretAccess, secretRefresh)
	rep = runCheck(recTime)
	if rep.Profiles[0].Level != LevelOK {
		t.Fatalf("expected ok on recovery, got %s", rep.Profiles[0].Level)
	}
	if len(notifications) != countBefore+1 || notifications[len(notifications)-1][2] != "ok" {
		t.Fatalf("expected one recovery notification, got %v", notifications)
	}

	// 6. Missing store -> logged-out, notifies once.
	currentErr = ErrStoreNotFound
	rep = runCheck(recTime.Add(5 * time.Minute))
	if rep.Profiles[0].Level != LevelLoggedOut || rep.WorstExitCode() != 3 {
		t.Fatalf("expected logged-out (exit 3), got %+v", rep.Profiles[0])
	}

	// 7. Unknown format -> unknown-format (exit 4), notifies once.
	currentErr = nil
	currentPayload = []byte(`{"claudeAiOauth": {"expiresAt": "bad-type"}}`)
	rep = runCheck(recTime.Add(10 * time.Minute))
	if rep.Profiles[0].Level != LevelUnknownFormat || rep.WorstExitCode() != 4 {
		t.Fatalf("expected unknown-format (exit 4), got %+v", rep.Profiles[0])
	}

	// 8. Unreadable store -> ReadError (exit 5).
	currentErr = errors.New("permission denied")
	rep = runCheck(recTime.Add(15 * time.Minute))
	if rep.Profiles[0].ReadError == "" || rep.WorstExitCode() != 5 {
		t.Fatalf("expected ReadError (exit 5), got %+v", rep.Profiles[0])
	}
}

func TestTwoProfilesIndependentAndCLIStatusAndReportFailure(t *testing.T) {
	now := time.Date(2026, 11, 18, 12, 0, 0, 0, time.UTC)
	cfg := DefaultConfig()
	cfg.UseCLIStatus = true
	cfg.Notify = []string{"notify"}
	cfg.Profiles = []ProfileConfig{
		{Name: "work", ConfigDir: "/tmp/work"},
		{Name: "personal", ConfigDir: "/tmp/personal"},
	}

	var notified []string
	in := Input{
		Config:  cfg,
		Now:     now,
		BaseEnv: []string{"HOME=/tmp"},
		ReadStore: func(_ context.Context, p ProfileConfig, _ []string) ([]byte, error) {
			if p.Name == "work" {
				return makeCredsPayload(now.Add(4*time.Hour), now.Add(30*24*time.Hour), "tok1", "ref1"), nil
			}
			return makeCredsPayload(now.Add(4*time.Hour), now.Add(10*time.Hour), "tok2", "ref2"), nil
		},
		CLIStatus: func(_ context.Context, _, configDir string, _ []string) (bool, error) {
			return configDir == "/tmp/work", nil
		},
		Notify: func(_ context.Context, argv, _ []string) error {
			notified = append(notified, argv[1]+":"+argv[2])
			return nil
		},
	}

	rep, st := Check(context.Background(), in)
	if len(rep.Profiles) != 2 {
		t.Fatalf("profiles = %d", len(rep.Profiles))
	}
	if rep.Profiles[0].Level != LevelOK {
		t.Errorf("work level = %s, want ok", rep.Profiles[0].Level)
	}
	if rep.Profiles[1].Level != LevelLoggedOut {
		t.Errorf("personal level = %s, want logged-out (from CLI status)", rep.Profiles[1].Level)
	}
	if len(notified) != 1 || notified[0] != "personal:logged-out" {
		t.Errorf("notified = %v", notified)
	}

	// ReportFailure on work profile.
	in.State = st
	in.Now = now.Add(1 * time.Minute)
	in.CLIStatus = nil
	pr, st, err := ReportFailure(context.Background(), in, "work")
	if err != nil {
		t.Fatal(err)
	}
	if pr.Level != LevelFailing || !pr.Since.Equal(in.Now) {
		t.Fatalf("report-failure got %+v", pr)
	}

	// Next check with same access token keeps failing.
	in.State = st
	in.Now = now.Add(2 * time.Minute)
	rep, _ = Check(context.Background(), in)
	if rep.Profiles[0].Level != LevelFailing {
		t.Errorf("expected work to remain failing, got %s", rep.Profiles[0].Level)
	}

	// FormatStatus JSON and one-line check.
	stRep, err := Status(context.Background(), in, "work")
	if err != nil {
		t.Fatal(err)
	}
	js, err := FormatStatus(stRep, true, false)
	if err != nil {
		t.Fatal(err)
	}
	var decoded Report
	if err := json.Unmarshal([]byte(js), &decoded); err != nil {
		t.Fatal(err)
	}
	if len(decoded.Profiles) != 1 || decoded.Profiles[0].Profile != "work" {
		t.Errorf("unexpected JSON status: %s", js)
	}
	line, err := FormatStatus(stRep, false, true)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.HasPrefix(line, "work:failing") {
		t.Errorf("unexpected line status: %q", line)
	}
}
