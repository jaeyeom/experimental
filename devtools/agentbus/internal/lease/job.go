package lease

import (
	"errors"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// jobHolderPrefix starts the holder handle of a checkout a job holds.
const jobHolderPrefix = "job:"

// JobHolder is the holder handle of a checkout job id holds, such as
// job:j-42.
func JobHolder(id string) string { return jobHolderPrefix + id }

// IsJobHolder reports whether handle is a job's, not a pane's. A job
// has no pane: it is never checked in on, told of waiters, or taken
// over. Its heartbeat in the job queue stands for the soft deadline.
func IsJobHolder(handle string) bool { return strings.HasPrefix(handle, jobHolderPrefix) }

// paneHolder reports whether h is an agent in a pane: not the user, and
// not a job.
func paneHolder(h *holder) bool {
	return h != nil && h.Handle != UserHolder && !IsJobHolder(h.Handle)
}

// ClaimForJob grants a free checkout to job id, with why naming the
// runner, or refuses. It never queues: a job that cannot have the
// checkout now stays ready for a later claim. The same job claiming
// again is granted, so a claim cut short can be retried. The holder has
// no deadline; Release with JobHolder(id) ends the hold.
func (t *Table) ClaimForJob(checkout, id, why string) (Reply, error) {
	if id == "" {
		return Reply{}, errors.New("a job lease needs a job id")
	}
	handle := JobHolder(id)
	return t.update(checkout, func(rec *record, now time.Time) (Reply, []eventlog.Event, error) {
		switch {
		case rec.Holder != nil && rec.Holder.Handle == handle:
			return Reply{Result: ResultGranted}, nil, nil
		case rec.Holder != nil:
			return Reply{}, nil, fmt.Errorf("%s is %s by %s", checkout, rec.State, rec.Holder.Handle)
		case rec.State != StateFree:
			return Reply{}, nil, fmt.Errorf("%s is %s", checkout, rec.State)
		}
		rec.State = StateHeld
		rec.Holder = &holder{Handle: handle, Since: now, Why: why}
		rec.Released = nil
		claim := eventlog.Event{Event: eventlog.EventLeaseClaim, Actor: handle, Detail: "job; why: " + why}
		grant := eventlog.Event{Event: eventlog.EventLeaseGrant, Actor: handle, Detail: grantDetail(rec)}
		return Reply{Result: ResultGranted}, []eventlog.Event{claim, grant}, nil
	})
}
