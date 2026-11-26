package config_test

import (
	"strings"
	"testing"
)

func TestLoadDeferKeys(t *testing.T) {
	cfg, err := newFake(t).load(tomlDoc(githubLines()))
	if err != nil {
		t.Fatalf("github defaults: %v", err)
	}
	if cfg.Sources[0].PriorityField != "Priority" || cfg.Sources[0].DeferField != "Start date" || cfg.Sources[0].DueField != "Target date" {
		t.Fatalf("github field defaults = priority:%q defer:%q due:%q", cfg.Sources[0].PriorityField, cfg.Sources[0].DeferField, cfg.Sources[0].DueField)
	}

	cfg, err = newFake(t).load(tomlDoc(append(githubLines(),
		`defer_label_prefix = "not-before:"`,
		`priority_field = "Severity"`,
		`defer_field = "Begin date"`,
		`due_field = ""`,
	)))
	if err != nil || cfg.Sources[0].DeferLabelPrefix != "not-before:" ||
		cfg.Sources[0].PriorityField != "Severity" || cfg.Sources[0].DeferField != "Begin date" || cfg.Sources[0].DueField != "" {
		t.Fatalf("github overrides: %+v, %v", cfg, err)
	}

	cfg, err = newFake(t).load(tomlDoc(append(jiraLines(), `defer_field = "customfield_12345"`)))
	if err != nil || cfg.Sources[0].DeferField != "customfield_12345" {
		t.Fatalf("jira field: %v", err)
	}

	for _, tc := range []struct {
		lines []string
		want  string
	}{
		{append(jiraLines(), `defer_label_prefix = "nb:"`), `field "defer_label_prefix": expected it only on a github source`},
		{append(jiraLines(), `priority_field = "Priority"`), `field "priority_field": expected it only on a github source`},
		{append(jiraLines(), `due_field = "Target date"`), `field "due_field": expected it only on a github source`},
		{append(jiraLines(), `defer_field = "custom field"`), `field "defer_field": expected a Jira field id`},
		{append(githubLines(), `priority_field = " Priority"`), `field "priority_field": expected no leading or trailing spaces`},
		{append(githubLines(), `defer_field = "Start date "`), `field "defer_field": expected no leading or trailing spaces`},
		{append(githubLines(), `due_field = " Target date"`), `field "due_field": expected no leading or trailing spaces`},
	} {
		_, err := newFake(t).load(tomlDoc(tc.lines))
		if err == nil || !strings.Contains(err.Error(), tc.want) {
			t.Fatalf("err = %v, want %q", err, tc.want)
		}
	}
}
