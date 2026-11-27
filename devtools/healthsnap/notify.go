package healthsnap

import (
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/pkg/notify"
)

func defaultAgentbusTemplate() []string {
	return []string{"agentbus", "send", "{recipient}", "{message}"}
}

// ResolveNotifyRoute chooses the command template and recipient for an alert.
// It returns ok=false when no delivery target is configured for that route.
func resolveNotifyRoute(cfg NotifyConfig, judgment bool) (tmpl []string, recipient string, ok bool) {
	if judgment {
		recipient = cfg.JudgmentRecipient
		if recipient == "" {
			recipient = cfg.Recipient
		}
		tmpl = cfg.JudgmentCommand
		if len(tmpl) == 0 {
			tmpl = cfg.Command
		}
	} else {
		recipient = cfg.Recipient
		tmpl = cfg.Command
	}

	if len(tmpl) == 0 {
		if recipient == "" {
			return nil, "", false
		}
		return defaultAgentbusTemplate(), recipient, true
	}
	if recipient == "" && commandUsesRecipient(tmpl) {
		return nil, "", false
	}
	return slices.Clone(tmpl), recipient, true
}

func commandUsesRecipient(tmpl []string) bool {
	for _, arg := range tmpl {
		if strings.Contains(arg, "{recipient}") {
			return true
		}
	}
	return false
}

// NotifyVars builds placeholder and environment variable bindings for an alert.
func NotifyVars(recipient string, a AlertRecord) []notify.Var {
	valStr := ""
	if a.Value != nil {
		valStr = formatScalar(a.Value)
	}
	return []notify.Var{
		{Placeholder: "{recipient}", EnvKey: "HEALTHSNAP_RECIPIENT", Positional: false, Value: recipient},
		{Placeholder: "{event}", EnvKey: "HEALTHSNAP_EVENT", Positional: false, Value: string(a.Event)},
		{Placeholder: "{rule}", EnvKey: "HEALTHSNAP_RULE", Positional: false, Value: a.RuleID},
		{Placeholder: "{check}", EnvKey: "HEALTHSNAP_CHECK", Positional: false, Value: a.CheckID},
		{Placeholder: "{state}", EnvKey: "HEALTHSNAP_STATE", Positional: false, Value: string(a.State)},
		{Placeholder: "{previous}", EnvKey: "HEALTHSNAP_PREVIOUS", Positional: false, Value: string(a.Previous)},
		{Placeholder: "{value}", EnvKey: "HEALTHSNAP_VALUE", Positional: false, Value: valStr},
		{Placeholder: "{judgment}", EnvKey: "HEALTHSNAP_JUDGMENT", Positional: false, Value: strconv.FormatBool(a.Judgment)},
		{Placeholder: "{error}", EnvKey: "HEALTHSNAP_ERROR", Positional: false, Value: a.Error},
		{Placeholder: "{summary}", EnvKey: "HEALTHSNAP_SUMMARY", Positional: false, Value: a.Message},
		{Placeholder: "{message}", EnvKey: "HEALTHSNAP_MESSAGE", Positional: true, Value: a.Message},
	}
}

// BuildNotifyCommand builds argv and env for executing a notification command
// directly without a shell.
func BuildNotifyCommand(template, baseEnv []string, recipient string, a AlertRecord) ([]string, []string) {
	return notify.BuildCommand(template, baseEnv, NotifyVars(recipient, a))
}

func formatAlertMessage(a AlertRecord, detail string) string {
	verb := strings.ToUpper(string(a.Event))
	msg := fmt.Sprintf("[healthsnap] %s %s (%s -> %s) check=%s", verb, a.RuleID, a.Previous, a.State, a.CheckID)
	if a.Value != nil {
		msg += " value=" + formatScalar(a.Value)
	}
	if a.Error != "" {
		msg += " error=" + a.Error
	}
	if detail != "" {
		msg += ": " + detail
	}
	return msg
}
