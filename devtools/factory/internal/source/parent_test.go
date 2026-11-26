package source

import "testing"

func TestAncestors(t *testing.T) {
	t.Parallel()

	items := map[string]Item{
		"CHILD": {ID: "CHILD", Parent: "STORY"},
		"STORY": {ID: "STORY", Parent: "EPIC"},
		"EPIC":  {ID: "EPIC", Parent: "TOP"},
		"B":     {ID: "B", Parent: "C"},
		"C":     {ID: "C", Parent: "B"},
		"SELF":  {ID: "SELF", Parent: "SELF"},
	}

	tests := []struct {
		name string
		id   string
		want []string
	}{
		{name: "chain", id: "CHILD", want: []string{"STORY", "EPIC", "TOP"}},
		{name: "direct only", id: "EPIC", want: []string{"TOP"}},
		{name: "cycle stops", id: "B", want: []string{"C"}},
		{name: "self parent", id: "SELF", want: nil},
		{name: "none", id: "TOP", want: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it := items[tc.id]
			if tc.id == "TOP" {
				it = Item{ID: "TOP"}
			}
			got := Ancestors(it, items)
			if len(got) != len(tc.want) {
				t.Fatalf("Ancestors(%s) = %v, want %v", tc.id, got, tc.want)
			}
			for i := range got {
				if got[i] != tc.want[i] {
					t.Fatalf("Ancestors(%s) = %v, want %v", tc.id, got, tc.want)
				}
			}
		})
	}
}

func TestAncestorsUnmirroredStops(t *testing.T) {
	t.Parallel()

	sub := Item{ID: "SUB", Parent: "STORY"}
	got := Ancestors(sub, map[string]Item{"SUB": sub})
	if len(got) != 1 || got[0] != "STORY" {
		t.Fatalf("Ancestors = %v, want [STORY]", got)
	}
	related, direct := AncestorRelation("EPIC", sub, map[string]Item{"SUB": sub})
	if related || direct {
		t.Fatal("grandparent was visible through an unmirrored parent")
	}
	related, direct = AncestorRelation("STORY", sub, nil)
	if !related || !direct {
		t.Fatalf("direct missing parent related=%v direct=%v", related, direct)
	}
}

func TestAncestorRelation(t *testing.T) {
	t.Parallel()

	items := map[string]Item{
		"CHILD": {ID: "CHILD", Parent: "STORY"},
		"STORY": {ID: "STORY", Parent: "EPIC"},
		"EPIC":  {ID: "EPIC"},
	}
	child := items["CHILD"]
	if related, direct := AncestorRelation("STORY", child, items); !related || !direct {
		t.Fatalf("story related=%v direct=%v", related, direct)
	}
	if related, direct := AncestorRelation("EPIC", child, items); !related || direct {
		t.Fatalf("epic related=%v direct=%v", related, direct)
	}
	if related, direct := AncestorRelation("CHILD", child, items); related || direct {
		t.Fatal("item counted itself as an ancestor")
	}
	if related, direct := AncestorRelation("", child, items); related || direct {
		t.Fatal("empty id counted as an ancestor")
	}
}
