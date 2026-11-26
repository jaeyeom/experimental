package config

import (
	"errors"
	"fmt"
	"slices"
	"strings"
)

// OrderedLanes returns every lane in the order a tick tries them: the
// [lanes] default names first, in that order, then the other lanes in
// config order. No configured lanes returns nil.
func (c *Config) OrderedLanes() []Lane {
	if !c.HasLanes() {
		return nil
	}
	out := make([]Lane, 0, len(c.Lanes))
	seen := make(map[string]struct{}, len(c.Lanes))
	for _, name := range c.LaneOrder.Default {
		lane, ok := c.LaneNamed(name)
		if !ok {
			continue
		}
		if _, dup := seen[name]; dup {
			continue
		}
		seen[name] = struct{}{}
		out = append(out, lane)
	}
	for _, lane := range c.Lanes {
		if _, ok := seen[lane.Name]; !ok {
			out = append(out, lane)
		}
	}
	return out
}

// LaneSource is src with the lane checkout as its cwd. The lane's
// [lane.worker], [lane.lease], [lane.output], and start_prompt_append keys
// override the source's.
func LaneSource(src Source, lane Lane) Source {
	src.Cwd = lane.Checkout
	src.Worker = lane.Worker.Over(src.Worker)
	if lane.MaxWorkers > 0 {
		src.Worker.MaxWorkers = lane.MaxWorkers
	}
	src.Lease = lane.applyLease(src.Lease)
	src.Output = lane.Output.Over(src.Output)
	if lane.startPromptAppendSet {
		src.StartPromptAppend = lane.StartPromptAppend
	}
	return src
}

// HasLanes reports whether any [[lane]] is configured.
// No lanes means one implicit local lane and the behavior before lanes.
func (c *Config) HasLanes() bool {
	return c != nil && len(c.Lanes) > 0
}

// LaneNamed returns the lane called name.
func (c *Config) LaneNamed(name string) (Lane, bool) {
	if c == nil {
		return Lane{}, false
	}
	for _, lane := range c.Lanes {
		if lane.Name == name {
			return lane, true
		}
	}
	return Lane{}, false
}

// LabelLane returns the lane an item's labels route it to, and false when
// no lane label matches: such an item may start on any default lane. Labels
// that match more than one lane are a config error, as in RouteLanes.
func (c *Config) LabelLane(labels []string) (Lane, bool, error) {
	if !c.HasLanes() {
		return Lane{}, false, nil
	}
	var matched []Lane
	for _, lane := range c.Lanes {
		if len(laneHits(lane.Labels, labels)) > 0 {
			matched = append(matched, lane)
		}
	}
	switch len(matched) {
	case 0:
		return Lane{}, false, nil
	case 1:
		return matched[0], true, nil
	default:
		_, err := c.RouteLanes(labels)
		return Lane{}, false, err
	}
}

// RouteLanes returns the lanes an item with labels may start on, in
// preference order. An item that carries a lane label goes to that lane
// only. An item whose labels match more than one lane is a config error.
// An item with no lane label uses [lanes] default, in order.
// No configured lanes returns nil and no error.
func (c *Config) RouteLanes(labels []string) ([]Lane, error) {
	if !c.HasLanes() {
		return nil, nil
	}
	var matched []Lane
	var parts []string
	for _, lane := range c.Lanes {
		hits := laneHits(lane.Labels, labels)
		if len(hits) == 0 {
			continue
		}
		matched = append(matched, lane)
		parts = append(parts, fmt.Sprintf("%s (%s)", lane.Name, strings.Join(hits, ", ")))
	}
	switch len(matched) {
	case 0:
		return c.defaultLanes()
	case 1:
		return matched, nil
	default:
		return nil, fmt.Errorf("lane config: expected item labels to match at most one lane, found %s", strings.Join(parts, "; "))
	}
}

func laneHits(laneLabels, itemLabels []string) []string {
	var hits []string
	for _, label := range laneLabels {
		if slices.Contains(itemLabels, label) && !slices.Contains(hits, label) {
			hits = append(hits, label)
		}
	}
	return hits
}

func (c *Config) defaultLanes() ([]Lane, error) {
	out := make([]Lane, 0, len(c.LaneOrder.Default))
	for _, name := range c.LaneOrder.Default {
		lane, ok := c.LaneNamed(name)
		if !ok {
			return nil, fmt.Errorf("lanes.default: expected a [[lane]] name, found %q", name)
		}
		out = append(out, lane)
	}
	if len(out) == 0 {
		return nil, errors.New("lanes.default: expected at least one lane name, found none")
	}
	return out, nil
}

// PickLane resolves a --machine value against the lanes routed for an item.
// value is a lane name, or a herdr machine that exactly one lane uses.
// A lane that is not in routed is refused, and the error names the routed lanes.
func (c *Config) PickLane(value string, routed []Lane) (Lane, error) {
	if !c.HasLanes() {
		return Lane{}, fmt.Errorf("--machine %s: expected [[lane]] config, found none", value)
	}
	if value == "" {
		return Lane{}, errors.New("--machine: expected a lane name or a machine, found empty")
	}
	lane, err := c.laneFor(value)
	if err != nil {
		return Lane{}, err
	}
	for _, ok := range routed {
		if ok.Name == lane.Name {
			return lane, nil
		}
	}
	return Lane{}, fmt.Errorf("--machine %s: lane %s is not compatible with the item labels; expected one of %s", value, lane.Name, laneNames(routed))
}

func (c *Config) laneFor(value string) (Lane, error) {
	if lane, ok := c.LaneNamed(value); ok {
		return lane, nil
	}
	var found []Lane
	for _, lane := range c.Lanes {
		if lane.Machine == value {
			found = append(found, lane)
		}
	}
	switch len(found) {
	case 1:
		return found[0], nil
	case 0:
		return Lane{}, fmt.Errorf("--machine %s: expected a lane name or a lane machine, found none (lanes: %s)", value, laneNames(c.Lanes))
	default:
		return Lane{}, fmt.Errorf("--machine %s: expected one lane on that machine, found %s; pass the lane name", value, laneNames(found))
	}
}

func laneNames(lanes []Lane) string {
	if len(lanes) == 0 {
		return "(none)"
	}
	names := make([]string, len(lanes))
	for i, lane := range lanes {
		names[i] = lane.Name
	}
	return strings.Join(names, ", ")
}

func validateLanes(lanes []Lane, order LaneOrder, env Env) error {
	if len(lanes) == 0 {
		return nil
	}
	names := make(map[string]struct{}, len(lanes))
	owners := make(map[string]string)
	for i := range lanes {
		if err := validateLane(i, lanes[i], names, owners, env); err != nil {
			return err
		}
	}
	return validateLaneOrder(order, names)
}

func validateLane(i int, lane Lane, names map[string]struct{}, owners map[string]string, env Env) error {
	if lane.Name == "" {
		return fmt.Errorf("lane %d field \"name\": expected a non-empty name, found empty", i+1)
	}
	if strings.ContainsAny(lane.Name, " \t\r\n:") {
		return fmt.Errorf("lane %q field \"name\": expected a name without spaces or colons, found %q", lane.Name, lane.Name)
	}
	if _, ok := names[lane.Name]; ok {
		return fmt.Errorf("lane %q field \"name\": expected a unique name, found duplicate", lane.Name)
	}
	names[lane.Name] = struct{}{}
	if lane.MaxWorkers < 1 {
		return fmt.Errorf("lane %q field \"max_workers\": expected a positive integer, found %d", lane.Name, lane.MaxWorkers)
	}
	if len(lane.Exec) > 0 && !lane.Remote() {
		return fmt.Errorf("lane %q field \"exec\": expected it only on a remote lane (with machine), found it on a local lane", lane.Name)
	}
	for _, part := range lane.Exec {
		if part == "" {
			return fmt.Errorf("lane %q field \"exec\": expected non-empty entries such as [\"ssh\", \"HOST\"], found an empty one", lane.Name)
		}
	}
	if err := validateLaneCheckout(lane, env); err != nil {
		return err
	}
	for _, label := range lane.Labels {
		if label == "" {
			return fmt.Errorf("lane %q field \"labels\": expected a non-empty label, found empty", lane.Name)
		}
		if owner, ok := owners[label]; ok && owner != lane.Name {
			return fmt.Errorf("lane %q field \"labels\": expected label %q in one lane, found it in %s too", lane.Name, label, owner)
		}
		owners[label] = lane.Name
	}
	return nil
}

func validateLaneCheckout(lane Lane, env Env) error {
	if lane.Checkout == "" {
		return fmt.Errorf("lane %q field \"checkout\": expected a non-empty path, found empty", lane.Name)
	}
	if lane.Remote() {
		return nil
	}
	info, err := env.Stat(lane.Checkout)
	if err != nil {
		return fmt.Errorf("lane %q field \"checkout\": expected a directory, found %s: %w", lane.Name, lane.Checkout, err)
	}
	if !info.IsDir() {
		return fmt.Errorf("lane %q field \"checkout\": expected a directory, found a file: %s", lane.Name, lane.Checkout)
	}
	return nil
}

func validateLaneOrder(order LaneOrder, names map[string]struct{}) error {
	if len(order.Default) == 0 {
		return errors.New("lanes.default: expected at least one lane name, found none")
	}
	seen := make(map[string]struct{}, len(order.Default))
	for _, name := range order.Default {
		if _, ok := names[name]; !ok {
			return fmt.Errorf("lanes.default: expected a [[lane]] name, found %q", name)
		}
		if _, ok := seen[name]; ok {
			return fmt.Errorf("lanes.default: expected each lane once, found %q twice", name)
		}
		seen[name] = struct{}{}
	}
	return nil
}
