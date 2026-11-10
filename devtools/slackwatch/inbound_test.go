package slackwatch

import (
	"reflect"
	"testing"
	"time"
)

func TestListInboundSince(t *testing.T) {
	s := openTestStore(t)
	for _, m := range []struct {
		msg  Message
		kind string
	}{
		{Message{Channel: "D1", TS: "1700000100.000200", User: "U2", Text: "late"}, inboundDM},
		{Message{Channel: "C1", TS: "1700000000.000100", User: "U1", Text: "early <@ME>"}, inboundMention},
		{Message{Channel: "D1", TS: "999999999.000100", User: "U3", Text: "ancient"}, inboundDM},
	} {
		if err := s.addInbound(m.msg, m.kind); err != nil {
			t.Fatal(err)
		}
	}

	all, err := s.ListInbound(time.Time{})
	if err != nil {
		t.Fatal(err)
	}
	var tss []string
	for _, in := range all {
		tss = append(tss, in.TS)
	}
	if want := []string{"999999999.000100", "1700000000.000100", "1700000100.000200"}; !reflect.DeepEqual(tss, want) {
		t.Errorf("all ts = %q, want %q", tss, want)
	}

	got, err := s.ListInbound(time.Unix(1700000050, 0))
	if err != nil {
		t.Fatal(err)
	}
	want := []Inbound{{Channel: "D1", TS: "1700000100.000200", Author: "U2", Text: "late", Kind: "dm"}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("since = %+v, want %+v", got, want)
	}

	none, err := s.ListInbound(time.Unix(1800000000, 0))
	if err != nil {
		t.Fatal(err)
	}
	if none == nil || len(none) != 0 {
		t.Errorf("none = %#v, want an empty non-nil slice", none)
	}
}
