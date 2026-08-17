package scan

// PRFacts is the I/O-free input to readiness classification.
type PRFacts struct {
	Unaddressed    bool
	CIState        string
	ReviewDecision string
	ReviewRequests []string
	IsDraft        bool
	Mergeable      string
}

// Bucket returns needs_you | draft | waiting | ready.
func Bucket(p PRFacts) string {
	needsYou := p.Unaddressed ||
		p.CIState == "failing" ||
		(p.ReviewDecision == "APPROVED" && len(p.ReviewRequests) > 0)
	if needsYou {
		return "needs_you"
	}
	if p.IsDraft {
		return "draft"
	}
	if !p.IsDraft &&
		p.ReviewDecision == "APPROVED" &&
		len(p.ReviewRequests) == 0 &&
		p.CIState == "green" &&
		p.Mergeable != "CONFLICTING" {
		return "ready"
	}
	return "waiting"
}
