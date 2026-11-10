package slackwatch

import (
	"database/sql"
	"fmt"
	"time"
)

// Inbound is a recorded DM to the token owner or a mention of them. Kind
// is dm or mention.
type Inbound struct {
	Channel string `json:"channel"`
	TS      string `json:"ts"`
	Author  string `json:"author"`
	Text    string `json:"text"`
	Kind    string `json:"kind"`
}

// ListInbound returns the inbound messages posted at or after since, in
// ts order. A zero since returns them all.
func (s *Store) ListInbound(since time.Time) ([]Inbound, error) {
	var from int64
	if !since.IsZero() {
		from = since.Unix()
	}
	out := []Inbound{}
	err := s.queryEach(func(rows *sql.Rows) error {
		var in Inbound
		if err := rows.Scan(&in.Channel, &in.TS, &in.Author, &in.Text, &in.Kind); err != nil {
			return err //nolint:wrapcheck // Wrapped by queryEach.
		}
		out = append(out, in)
		return nil
	}, `SELECT channel, ts, author, text, kind FROM inbound
WHERE CAST(ts AS REAL) >= ? ORDER BY `+tsOrder, from)
	if err != nil {
		return nil, fmt.Errorf("list inbound: %w", err)
	}
	return out, nil
}
