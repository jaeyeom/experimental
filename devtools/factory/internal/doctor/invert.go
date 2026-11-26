package doctor

import "github.com/jaeyeom/experimental/devtools/factory/internal/priority"

func priorityInversions(in Input) ([]Finding, error) {
	rows, err := priority.Find(in.Snapshot.Items, in.Config)
	if err != nil {
		return nil, err //nolint:wrapcheck // internal factory error already carries context
	}
	out := make([]Finding, 0, len(rows))
	for _, row := range rows {
		out = append(out, finding(
			CheckPriorityInversion,
			SeverityWarn,
			priority.Format(row),
			"factory priorities --apply",
			row.BlockerID, row.TargetID,
		))
	}
	return out, nil
}
