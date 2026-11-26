package source

// Ancestors walks Parent the same way parent_rank does.
// The first id is the direct parent. It is included even when that parent
// is not in items. A later id is included only when the item before it is
// in items. A repeated id stops the walk, so a parent cycle cannot loop.
func Ancestors(it Item, items map[string]Item) []string {
	seen := map[string]struct{}{it.ID: {}}
	var out []string
	cur := it.Parent
	for cur != "" {
		if _, ok := seen[cur]; ok {
			return out
		}
		seen[cur] = struct{}{}
		out = append(out, cur)
		parent, ok := items[cur]
		if !ok {
			return out
		}
		cur = parent.Parent
	}
	return out
}

// AncestorRelation reports whether ancestor is on it.Parent's chain.
// direct is true when ancestor is it.Parent.
func AncestorRelation(ancestor string, it Item, items map[string]Item) (related bool, direct bool) {
	if ancestor == "" {
		return false, false
	}
	for i, id := range Ancestors(it, items) {
		if id == ancestor {
			return true, i == 0
		}
	}
	return false, false
}
