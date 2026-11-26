package cli

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
)

func TestPrintGate(t *testing.T) {
	tests := []struct {
		name string
		res  gate.Result
		want string
	}{
		{
			name: "pass with claims and prs",
			res:  gate.Result{OK: true, InFlight: 3, MaxInFlight: 5},
			want: "OK: 3 in flight (max 5)\n",
		},
		{
			name: "pass with nothing in flight",
			res:  gate.Result{OK: true, InFlight: 0, MaxInFlight: 3},
			want: "OK: 0 in flight (max 3)\n",
		},
		{
			name: "refusal at the cap",
			res:  gate.Result{Reasons: []string{"5 items in flight (max 5)"}, InFlight: 5, MaxInFlight: 5},
			want: "5 items in flight (max 5)\n",
		},
		{
			name: "refusal for a hold",
			res:  gate.Result{Reasons: []string{"hold file /h exists"}, InFlight: 0, MaxInFlight: 3},
			want: "hold file /h exists\n",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var b strings.Builder
			if err := printGate(&b, tc.res, false); err != nil {
				t.Fatal(err)
			}
			if b.String() != tc.want {
				t.Fatalf("printGate() = %q, want %q", b.String(), tc.want)
			}
		})
	}
}
