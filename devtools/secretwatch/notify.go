package secretwatch

import (
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/notify"
)

// NotifyVars builds the placeholder and environment bindings for a Finding.
// Secret values are never included.
func NotifyVars(f Finding) []notify.Var {
	ts := ""
	if !f.Time.IsZero() {
		ts = f.Time.UTC().Format(time.RFC3339)
	}
	return []notify.Var{
		{Placeholder: "{time}", EnvKey: "SECRETWATCH_TIME", Positional: true, Value: ts},
		{Placeholder: "{journal}", EnvKey: "SECRETWATCH_JOURNAL", Positional: true, Value: string(f.Journal)},
		{Placeholder: "{unit}", EnvKey: "SECRETWATCH_UNIT", Positional: true, Value: f.Unit},
		{Placeholder: "{process}", EnvKey: "SECRETWATCH_PROCESS", Positional: true, Value: f.Process},
		{Placeholder: "{pid}", EnvKey: "SECRETWATCH_PID", Positional: true, Value: f.PID},
		{Placeholder: "{rule}", EnvKey: "SECRETWATCH_RULE", Positional: true, Value: f.Rule},
		{Placeholder: "{prefix}", EnvKey: "SECRETWATCH_PREFIX", Positional: true, Value: f.MaskedPrefix},
		{Placeholder: "{cursor}", EnvKey: "SECRETWATCH_CURSOR", Positional: true, Value: f.Cursor},
	}
}

// BuildNotifyCommand prepares argv and env for invoking a configured notify
// command on f without a shell.
func BuildNotifyCommand(template, baseEnv []string, f Finding) ([]string, []string) {
	return notify.BuildCommand(template, baseEnv, NotifyVars(f))
}
