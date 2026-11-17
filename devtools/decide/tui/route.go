package tui

import "github.com/jaeyeom/experimental/devtools/decide"

// routeChoices is the key help after the reply route warning.
const routeChoices = "  w worker  b both  o owner only  esc cancel"

// askRoute holds a reply to item behind the route prompt when another
// agent is working on it and no route was picked for this reply yet.
func (m *Model) askRoute(item *decide.Item) bool {
	if m.replyRoute != "" || !item.HasOtherWorker() {
		return false
	}
	m.confirmKind = confirmRoute
	m.status = decide.RouteWarning(item) + routeChoices
	return true
}

// routeKey answers the route prompt. w, b, and o send to the worker,
// both, or the owner only. n and Esc cancel and keep the draft.
func (m *Model) routeKey(r rune) error {
	route := ""
	switch r {
	case 'w', 'W':
		route = decide.RouteWorker
	case 'b', 'B':
		route = decide.RouteBoth
	case 'o', 'O':
		route = decide.RouteOwner
	case 'n', 'N', '\x1b':
		m.confirmKind = confirmNone
		m.status = ""
		return nil
	default:
		return nil
	}
	m.confirmKind = confirmNone
	m.replyRoute = route
	return m.submitReply()
}
