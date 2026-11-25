package secretwatch

import (
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestBuildJournalctlArgs(t *testing.T) {
	got := buildJournalctlArgs(Query{
		Kind:        JournalUser,
		AfterCursor: "cur1",
		Until:       "2026-11-25 12:00:00 UTC",
		Unit:        "deploy.service",
		Follow:      true,
	})
	want := []string{
		"--output=json",
		"--no-pager",
		"--show-cursor",
		"--user",
		"--after-cursor=cur1",
		"--until=2026-11-25 12:00:00 UTC",
		"--unit=deploy.service",
		"--follow",
	}
	if !slices.Equal(got, want) {
		t.Fatalf("args = %v, want %v", got, want)
	}

	sysArgs := buildJournalctlArgs(Query{Kind: JournalSystem, Since: "1 hour ago"})
	wantSys := []string{"--output=json", "--no-pager", "--show-cursor", "--system", "--since=1 hour ago"}
	if !slices.Equal(sysArgs, wantSys) {
		t.Fatalf("sysArgs = %v, want %v", sysArgs, wantSys)
	}
}

func TestParseJournalStream(t *testing.T) {
	secret := "ghp_" + strings.Repeat("z9Y8", 9)
	byteArrayMsg := make([]string, len(secret))
	for i := 0; i < len(secret); i++ {
		byteArrayMsg[i] = fmt.Sprintf("%d", secret[i])
	}

	stream := strings.Join([]string{
		"-- Boot 1234 --",
		fmt.Sprintf(`{"__CURSOR":"c1","__REALTIME_TIMESTAMP":"1764113975000000","_SYSTEMD_USER_UNIT":"app.service","_COMM":"curl","_PID":"42","MESSAGE":"Bearer %s"}`, secret),
		fmt.Sprintf(`{"__CURSOR":"c2","__REALTIME_TIMESTAMP":"1764113976000000","SYSLOG_IDENTIFIER":"sh","SYSLOG_PID":"99","MESSAGE":[%s]}`, strings.Join(byteArrayMsg, ",")),
		"-- cursor: c2",
	}, "\n")

	var entries []Entry
	tail, err := parseJournalStream(JournalUser, strings.NewReader(stream), func(e Entry) error {
		entries = append(entries, e)
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if tail != "c2" {
		t.Errorf("tail = %q, want c2", tail)
	}
	if len(entries) != 2 {
		t.Fatalf("len(entries) = %d, want 2", len(entries))
	}
	if entries[0].Unit != "app.service" || entries[0].Process != "curl" || entries[0].PID != "42" {
		t.Errorf("entry[0] = %+v", entries[0])
	}
	if wantTS := time.UnixMicro(1764113975000000).UTC(); !entries[0].Timestamp.Equal(wantTS) {
		t.Errorf("entry[0].Timestamp = %v, want %v", entries[0].Timestamp, wantTS)
	}
	if entries[1].Message != secret || entries[1].Process != "sh" || entries[1].PID != "99" {
		t.Errorf("entry[1] = %+v", entries[1])
	}
}

func TestParseJournalErrorDoesNotLeakSecret(t *testing.T) {
	secret := "ghp_" + strings.Repeat("a1B2", 9)
	bad := fmt.Sprintf(`{"MESSAGE": %q, broken`, secret)
	_, err := parseJournalStream(JournalUser, strings.NewReader(bad), func(Entry) error { return nil })
	if err == nil {
		t.Fatal("expected parse error")
	}
	if strings.Contains(err.Error(), secret) {
		t.Fatalf("parse error leaked secret: %v", err)
	}
}
