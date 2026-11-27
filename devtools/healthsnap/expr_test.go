package healthsnap

import "testing"

func TestCompileAndEvalExpr(t *testing.T) {
	vars := map[string]any{
		"exit":           1,
		"value":          float64(85),
		"age":            float64(7500), // 2h5m in seconds
		"status":         "down",
		"matched":        true,
		"msg":            "fatal: connection reset",
		"missing":        nil,
		"durationString": "2h15m",
	}

	tests := []struct {
		name    string
		expr    string
		want    bool
		wantErr bool
	}{
		{name: "exit nonzero", expr: "exit != 0", want: true},
		{name: "exit zero false", expr: "exit == 0", want: false},
		{name: "numeric gt", expr: "value > 80", want: true},
		{name: "numeric lt", expr: "value < 80", want: false},
		{name: "numeric gte", expr: "value >= 85", want: true},
		{name: "numeric lte", expr: "value <= 85", want: true},
		{name: "duration literal", expr: "age > 2h", want: true},
		{name: "duration literal less", expr: "age < 3h", want: true},
		{name: "duration string comparison", expr: "durationString > 2h", want: true},
		{name: "string equality", expr: `status == "down"`, want: true},
		{name: "string inequality", expr: `status != "up"`, want: true},
		{name: "boolean bare ident", expr: "matched", want: true},
		{name: "negation", expr: "!matched", want: false},
		{name: "compound and", expr: `exit != 0 && status == "down"`, want: true},
		{name: "compound or", expr: `exit == 0 || value > 80`, want: true},
		{name: "parentheses precedence", expr: `(exit == 0 || status == "up") && value > 80`, want: false},
		{name: "regex matches", expr: `msg matches "fatal|panic"`, want: true},
		{name: "contains", expr: `msg contains "connection"`, want: true},
		{name: "nil equality", expr: `missing == nil`, want: true},
		{name: "nil inequality", expr: `status != nil`, want: true},
		{name: "numeric comparison with nil errors", expr: "missing > 10", wantErr: true},
		{name: "unknown ident errors", expr: "bogus > 1", wantErr: true},
		{name: "non bool root errors", expr: "value", wantErr: true},
		{name: "invalid regex in matches", expr: `msg matches "[unclosed"`, wantErr: true},
		{name: "syntax error unclosed paren", expr: "(exit == 0", wantErr: true},
		{name: "empty expr", expr: "   ", wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			compiled, err := CompileExpr(tc.expr)
			if err != nil {
				if !tc.wantErr {
					t.Fatalf("CompileExpr(%q) unexpected error: %v", tc.expr, err)
				}
				return
			}
			got, err := compiled.Eval(vars)
			if (err != nil) != tc.wantErr {
				t.Fatalf("Eval(%q) error = %v, wantErr %v", tc.expr, err, tc.wantErr)
			}
			if !tc.wantErr && got != tc.want {
				t.Errorf("Eval(%q) = %v, want %v", tc.expr, got, tc.want)
			}
		})
	}
}
