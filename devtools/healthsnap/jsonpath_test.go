package healthsnap

import (
	"reflect"
	"testing"
)

func TestEvalJSONPath(t *testing.T) {
	healthJSON := `{"daemon":"up","transport":"ok","problems":[],"meta":{"count":3,"dash-key":"v"}}`
	heldJSON := `[{"id":"m-1","age_seconds":120,"status":"held"},{"id":"m-2","age_seconds":3600,"status":"ok"}]`
	emptyArr := `[]`

	tests := []struct {
		name    string
		payload string
		path    string
		want    any
		wantErr bool
	}{
		{name: "root field string", payload: healthJSON, path: "$.daemon", want: "up"},
		{name: "nested field number", payload: healthJSON, path: "$.meta.count", want: float64(3)},
		{name: "bracket quoted key", payload: healthJSON, path: `$.meta["dash-key"]`, want: "v"},
		{name: "single quoted key", payload: healthJSON, path: `$.meta['dash-key']`, want: "v"},
		{name: "empty array length", payload: healthJSON, path: "$.problems.length()", want: float64(0)},
		{name: "root array length", payload: heldJSON, path: "$.length()", want: float64(2)},
		{name: "array index", payload: heldJSON, path: "$[1].id", want: "m-2"},
		{name: "negative array index", payload: heldJSON, path: "$[-1].id", want: "m-2"},
		{name: "wildcard max", payload: heldJSON, path: "$[*].age_seconds.max()", want: float64(3600)},
		{name: "wildcard dot before bracket", payload: heldJSON, path: "$.[*].age_seconds.min()", want: float64(120)},
		{name: "wildcard sum", payload: heldJSON, path: "$[*].age_seconds.sum()", want: float64(3720)},
		{name: "wildcard count", payload: heldJSON, path: "$[*].age_seconds.count()", want: float64(2)},
		{name: "filter eq count", payload: heldJSON, path: `$[?(@.status == "held")].length()`, want: float64(1)},
		{name: "filter neq count", payload: heldJSON, path: `$[?(@.status != "ok")].length()`, want: float64(1)},
		{name: "filter numeric gt", payload: heldJSON, path: `$[?(@.age_seconds > 1000)].length()`, want: float64(1)},
		{name: "empty array wildcard max", payload: emptyArr, path: "$[*].age_seconds.max()", want: float64(0)},
		{name: "empty array filter length", payload: emptyArr, path: `$[?(@.status != "ok")].length()`, want: float64(0)},
		{name: "string length", payload: healthJSON, path: "$.daemon.length()", want: float64(2)},
		{name: "missing key errors", payload: healthJSON, path: "$.missing", wantErr: true},
		{name: "index out of bounds errors", payload: emptyArr, path: "$[0].id", wantErr: true},
		{name: "invalid json errors", payload: "{not json", path: "$.daemon", wantErr: true},
		{name: "invalid path syntax errors", payload: healthJSON, path: "daemon", wantErr: true},
		{name: "unclosed bracket errors", payload: healthJSON, path: "$[0", wantErr: true},
		{name: "trailing dot errors", payload: healthJSON, path: "$.", wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			jp, parseErr := CompileJSONPath(tc.path)
			if parseErr != nil {
				if !tc.wantErr {
					t.Fatalf("CompileJSONPath(%q) unexpected error: %v", tc.path, parseErr)
				}
				return
			}
			got, err := jp.EvalBytes([]byte(tc.payload))
			if (err != nil) != tc.wantErr {
				t.Fatalf("EvalBytes(%q) error = %v, wantErr %v", tc.path, err, tc.wantErr)
			}
			if !tc.wantErr && !reflect.DeepEqual(got, tc.want) {
				t.Errorf("EvalBytes(%q) = %#v (%T), want %#v (%T)", tc.path, got, got, tc.want, tc.want)
			}
		})
	}
}
