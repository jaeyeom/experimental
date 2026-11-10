package remotecall

import (
	"encoding/json"
	"strings"
)

// herdrDownMarkers in `herdr machine status` output mean the host is down.
var herdrDownMarkers = []string{
	"unreachable",
	"offline",
	"disconnected",
	"disabled",
	"refused",
	"no route",
	"could not resolve",
	`"down"`,
}

// HerdrMachineUp reports whether `herdr machine status <m> --json` says the
// host answered, for use in a Probe. herdr prints
// [{"id", "label", "status", "error"}] with status "reachable" for a host
// it reached. Any status other than a down status counts as answered, so an
// auth failure on a live host still reads as up. Output that does not parse
// falls back to looking for down markers.
func HerdrMachineUp(stdout, stderr string) bool {
	var machines []struct {
		Status string `json:"status"`
	}
	if err := json.Unmarshal([]byte(stdout), &machines); err == nil && len(machines) > 0 {
		status := strings.ToLower(machines[0].Status)
		for _, marker := range herdrDownMarkers {
			if strings.Trim(marker, `"`) == status {
				return false
			}
		}
		return status != ""
	}
	return !containsAny(strings.ToLower(stdout+"\n"+stderr), herdrDownMarkers)
}
