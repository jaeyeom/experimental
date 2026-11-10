package remotecall

import "testing"

func TestHerdrMachineUp(t *testing.T) {
	cases := []struct {
		name, stdout, stderr string
		want                 bool
	}{
		{"reachable", `[{"id":"x","label":"linux","status":"reachable","error":null}]`, "", true},
		{"auth status on a live host", `[{"label":"m2","status":"auth_required","error":"locked"}]`, "", true},
		{"unreachable", `[{"label":"m2","status":"unreachable","error":"connect failed"}]`, "", false},
		{"disabled", `[{"label":"m2","status":"disabled","error":null}]`, "", false},
		{"empty status", `[{"label":"m2","status":""}]`, "", false},
		{"unparsed text with a down marker", "machine m2 is offline", "", false},
		{"unparsed text without a marker", "ok", "", true},
	}
	for _, tc := range cases {
		if got := HerdrMachineUp(tc.stdout, tc.stderr); got != tc.want {
			t.Errorf("%s: HerdrMachineUp = %v, want %v", tc.name, got, tc.want)
		}
	}
}
