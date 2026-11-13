package diskwatch

import "testing"

func TestParseAmount(t *testing.T) {
	t.Parallel()
	pct, err := ParseAmount("15%")
	if err != nil || !pct.IsPercent || pct.Percent != 15 {
		t.Fatalf("15%% = %+v, %v", pct, err)
	}
	got, err := ParseAmount("40G")
	if err != nil || got.Bytes != 40<<30 {
		t.Fatalf("40G = %+v, %v", got, err)
	}
	if _, err := ParseAmount("nope"); err == nil {
		t.Fatal("expected an error")
	}
}

func TestAmountCompare(t *testing.T) {
	t.Parallel()
	low, _ := ParseAmount("15%")
	high, _ := ParseAmount("25%")
	less, err := low.Less(high)
	if err != nil || !less {
		t.Fatalf("15%% < 25%% = %v, %v", less, err)
	}
	bytes, _ := ParseAmount("40G")
	if _, err := low.Less(bytes); err == nil {
		t.Fatal("mixed units should fail")
	}
	if !low.Below(100, 1000) { // 10%
		t.Fatal("10% should be below 15%")
	}
	if low.Below(150, 1000) { // 15% is not below 15%
		t.Fatal("15% should not be below 15%")
	}
	if !high.Above(300, 1000) {
		t.Fatal("30% should be above 25%")
	}
}
