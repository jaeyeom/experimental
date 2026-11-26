package decide

import (
	"bytes"
	"encoding/json"
	"fmt"
	"math"
	"strconv"
	"strings"
	"time"
)

// Known A2UI-aligned dashboard panel kinds.
const (
	PanelKindText  = "Text"
	PanelKindList  = "List"
	PanelKindGauge = "Gauge"
	PanelKindChart = "Chart"
	PanelKindTable = "Table"
)

// List row / gauge threshold state values.
const (
	PanelStateOK   = "ok"
	PanelStateWarn = "warn"
	PanelStateFail = "fail"
)

// MaxPanelTableRows is the maximum number of data rows shown in a Table panel
// before truncating with "+N more".
const MaxPanelTableRows = 5

// PanelTextData is the data payload for PanelKindText.
type PanelTextData struct {
	Text string `json:"text"`
}

// PanelListRow is one row in PanelListData.
type PanelListRow struct {
	Label string `json:"label"`
	Value string `json:"value"`
	State string `json:"state,omitempty"`
}

// PanelListData is the data payload for PanelKindList.
type PanelListData struct {
	Rows []PanelListRow `json:"rows"`
}

// PanelGaugeData is the data payload for PanelKindGauge.
type PanelGaugeData struct {
	Value float64  `json:"value"`
	Max   float64  `json:"max"`
	Warn  *float64 `json:"warn,omitempty"`
	Fail  *float64 `json:"fail,omitempty"`
	Unit  string   `json:"unit,omitempty"`
}

// PanelChartPoint is one sample in PanelChartData.
type PanelChartPoint struct {
	T string  `json:"t"`
	V float64 `json:"v"`
}

// PanelChartProjection is an optional limit crossing forecast for PanelChartData.
type PanelChartProjection struct {
	To float64 `json:"to"`
	At string  `json:"at"`
}

// PanelChartData is the data payload for PanelKindChart.
type PanelChartData struct {
	Series     []PanelChartPoint     `json:"series"`
	Unit       string                `json:"unit,omitempty"`
	Limit      *float64              `json:"limit,omitempty"`
	Projection *PanelChartProjection `json:"projection,omitempty"`
}

// PanelTableData is the data payload for PanelKindTable.
type PanelTableData struct {
	Columns []string `json:"columns"`
	Rows    [][]any  `json:"rows"`
}

func validatePanelText(data json.RawMessage) error {
	var raw struct {
		Text *string `json:"text"`
	}
	if err := decodeJSONObject(data, &raw); err != nil {
		return err
	}
	if raw.Text == nil {
		return fmt.Errorf("missing required field %q", "text")
	}
	return nil
}

func validatePanelList(data json.RawMessage) error {
	var raw struct {
		Rows []struct {
			Label *string `json:"label"`
			Value *string `json:"value"`
			State string  `json:"state"`
		} `json:"rows"`
	}
	if err := decodeJSONObject(data, &raw); err != nil {
		return err
	}
	if raw.Rows == nil {
		return fmt.Errorf("missing required field %q", "rows")
	}
	for i, row := range raw.Rows {
		if row.Label == nil {
			return fmt.Errorf("rows[%d]: missing required field %q", i, "label")
		}
		if row.Value == nil {
			return fmt.Errorf("rows[%d]: missing required field %q", i, "value")
		}
		switch row.State {
		case "", PanelStateOK, PanelStateWarn, PanelStateFail:
		default:
			return fmt.Errorf("rows[%d]: state %q must be ok, warn, or fail", i, row.State)
		}
	}
	return nil
}

func validatePanelGauge(data json.RawMessage) error {
	var raw struct {
		Value *float64 `json:"value"`
		Max   *float64 `json:"max"`
		Warn  *float64 `json:"warn"`
		Fail  *float64 `json:"fail"`
		Unit  string   `json:"unit"`
	}
	if err := decodeJSONObject(data, &raw); err != nil {
		return err
	}
	if raw.Value == nil {
		return fmt.Errorf("missing required field %q", "value")
	}
	if err := checkFinite(*raw.Value, "value"); err != nil {
		return err
	}
	if raw.Max == nil {
		return fmt.Errorf("missing required field %q", "max")
	}
	if err := checkFinite(*raw.Max, "max"); err != nil {
		return err
	}
	if *raw.Max <= 0 {
		return fmt.Errorf("max must be > 0")
	}
	if raw.Warn != nil {
		if err := checkFinite(*raw.Warn, "warn"); err != nil {
			return err
		}
	}
	if raw.Fail != nil {
		if err := checkFinite(*raw.Fail, "fail"); err != nil {
			return err
		}
	}
	return nil
}

type rawChartPoint struct {
	T *string  `json:"t"`
	V *float64 `json:"v"`
}

type rawChartProjection struct {
	To *float64 `json:"to"`
	At *string  `json:"at"`
}

func validatePanelChart(data json.RawMessage) error {
	var raw struct {
		Series     []rawChartPoint     `json:"series"`
		Unit       string              `json:"unit"`
		Limit      *float64            `json:"limit"`
		Projection *rawChartProjection `json:"projection"`
	}
	if err := decodeJSONObject(data, &raw); err != nil {
		return err
	}
	if err := validateChartSeries(raw.Series); err != nil {
		return err
	}
	if raw.Limit != nil {
		if err := checkFinite(*raw.Limit, "limit"); err != nil {
			return err
		}
	}
	return validateChartProjection(raw.Projection)
}

func validateChartSeries(series []rawChartPoint) error {
	if series == nil {
		return fmt.Errorf("missing required field %q", "series")
	}
	for i, pt := range series {
		if pt.T == nil || strings.TrimSpace(*pt.T) == "" {
			return fmt.Errorf("series[%d]: missing required field %q", i, "t")
		}
		if pt.V == nil {
			return fmt.Errorf("series[%d]: missing required field %q", i, "v")
		}
		if err := checkFinite(*pt.V, fmt.Sprintf("series[%d].v", i)); err != nil {
			return err
		}
	}
	return nil
}

func validateChartProjection(proj *rawChartProjection) error {
	if proj == nil {
		return nil
	}
	if proj.To == nil {
		return fmt.Errorf("projection: missing required field %q", "to")
	}
	if err := checkFinite(*proj.To, "projection.to"); err != nil {
		return err
	}
	if proj.At == nil || strings.TrimSpace(*proj.At) == "" {
		return fmt.Errorf("projection: missing required field %q", "at")
	}
	if _, err := time.Parse(time.RFC3339Nano, *proj.At); err != nil {
		return fmt.Errorf("projection.at %q must be RFC 3339", *proj.At)
	}
	return nil
}

func validatePanelTable(data json.RawMessage) error {
	var raw struct {
		Columns []string `json:"columns"`
		Rows    [][]any  `json:"rows"`
	}
	if err := decodeJSONObject(data, &raw); err != nil {
		return err
	}
	if raw.Columns == nil {
		return fmt.Errorf("missing required field %q", "columns")
	}
	if raw.Rows == nil {
		return fmt.Errorf("missing required field %q", "rows")
	}
	for i, row := range raw.Rows {
		if row == nil {
			return fmt.Errorf("rows[%d] must be an array", i)
		}
		for j, cell := range row {
			switch cell.(type) {
			case nil, string, float64, bool:
			default:
				return fmt.Errorf("rows[%d][%d] must be a scalar value", i, j)
			}
		}
	}
	return nil
}

func decodeJSONObject(data json.RawMessage, dst any) error {
	trimmed := bytes.TrimSpace(data)
	if len(trimmed) == 0 || trimmed[0] != '{' {
		return fmt.Errorf("must be a JSON object")
	}
	if err := json.Unmarshal(trimmed, dst); err != nil {
		return fmt.Errorf("parse JSON: %w", err)
	}
	return nil
}

func checkFinite(v float64, field string) error {
	if math.IsNaN(v) || math.IsInf(v, 0) {
		return fmt.Errorf("%s must be finite", field)
	}
	return nil
}

// GaugeState evaluates g against its warn/fail thresholds and returns
// PanelStateFail, PanelStateWarn, or PanelStateOK.
func GaugeState(g PanelGaugeData) string {
	if g.Fail != nil && g.Warn != nil && *g.Fail < *g.Warn {
		switch {
		case g.Value <= *g.Fail:
			return PanelStateFail
		case g.Value <= *g.Warn:
			return PanelStateWarn
		default:
			return PanelStateOK
		}
	}
	switch {
	case g.Fail != nil && g.Value >= *g.Fail:
		return PanelStateFail
	case g.Warn != nil && g.Value >= *g.Warn:
		return PanelStateWarn
	default:
		return PanelStateOK
	}
}

// FormatTableCell formats a scalar table cell as text.
func FormatTableCell(v any) string {
	switch x := v.(type) {
	case nil:
		return ""
	case string:
		return x
	case bool:
		return strconv.FormatBool(x)
	case float64:
		return FormatPanelNumber(x)
	default:
		return fmt.Sprint(x)
	}
}

// FormatPanelNumber formats a float concisely without trailing .0 on integers.
func FormatPanelNumber(v float64) string {
	if math.Abs(v-math.Round(v)) < 1e-9 && math.Abs(v) < 1e15 {
		return strconv.FormatInt(int64(math.Round(v)), 10)
	}
	return strconv.FormatFloat(v, 'f', -1, 64)
}

// FormatPanelUpdatedAgo formats updatedAt relative to now as "updated 3h ago"
// or "updated now".
func FormatPanelUpdatedAgo(updatedAt, now time.Time) string {
	age := PanelAge(now.Sub(updatedAt))
	if age == "now" {
		return "updated now"
	}
	return "updated " + age + " ago"
}
