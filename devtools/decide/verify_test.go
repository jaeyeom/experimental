package decide

import "testing"

func TestMessageFromHumanAndHumanUI(t *testing.T) {
	tests := []struct {
		name      string
		m         Message
		fromHuman bool
		humanUI   bool
	}{
		{"inbox pick", Message{Author: "human", Via: ViaUI, OptionKey: "a"}, true, true},
		{"human from the command line", Message{Author: "human", Via: ViaCLI}, true, false},
		{"stored before via", Message{Author: "human"}, true, false},
		{"agent sent as the user", Message{Author: "human", Via: ViaUI, By: "worker"}, false, false},
		{"agent reply", Message{Author: "agentbus", Via: ViaUI}, false, false},
	}
	for _, tt := range tests {
		if got := tt.m.FromHuman(); got != tt.fromHuman {
			t.Errorf("%s: FromHuman = %v, want %v", tt.name, got, tt.fromHuman)
		}
		if got := tt.m.HumanUI(); got != tt.humanUI {
			t.Errorf("%s: HumanUI = %v, want %v", tt.name, got, tt.humanUI)
		}
	}
}

func TestAnswerHumanUI(t *testing.T) {
	var none *Answer
	if none.HumanUI() {
		t.Error("nil answer is HumanUI")
	}
	if !(&Answer{AnsweredBy: "human", Via: ViaUI}).HumanUI() {
		t.Error("inbox answer is not HumanUI")
	}
	if (&Answer{AnsweredBy: "human", Via: ViaCLI}).HumanUI() {
		t.Error("command line answer is HumanUI")
	}
	if (&Answer{AnsweredBy: "agent", Via: ViaUI}).HumanUI() {
		t.Error("agent answer is HumanUI")
	}
}
