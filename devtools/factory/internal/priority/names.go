package priority

import (
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// builtinRanks are the Jira names factory already maps when priority_ranks
// does not override them. The first name of a rank is the one suggest writes.
var builtinRanks = []struct {
	name string
	rank int
}{
	{name: "Highest", rank: 0},
	{name: "High", rank: 1},
	{name: "Medium", rank: 2},
	{name: "Low", rank: 3},
	{name: "Lowest", rank: 4},
}

type table struct {
	names    map[int]string
	problems map[int]error
}

func newTable(cfg config.Source) (table, error) {
	switch cfg.Kind {
	case "jira":
		return jiraTable(cfg)
	case "github":
		return githubTable(cfg)
	default:
		return table{}, fmt.Errorf("priority: expected kind jira or github, found %q", cfg.Kind)
	}
}

func (t table) lookup(rank int) (string, error) {
	if err := t.problems[rank]; err != nil {
		return "", err
	}
	return t.names[rank], nil
}

// builtinGitHubRanks are GitHub's starter Priority issue field options when
// priority_labels is empty and priority_ranks does not override them.
var builtinGitHubRanks = []struct {
	name string
	rank int
}{
	{name: "Urgent", rank: 0},
	{name: "High", rank: 1},
	{name: "Medium", rank: 2},
	{name: "Low", rank: 3},
}

func jiraTable(cfg config.Source) (table, error) {
	return rankedTable(builtinRanks, cfg.PriorityRanks)
}

func rankedTable(builtins []struct {
	name string
	rank int
}, overrides map[string]int,
) (table, error) {
	rankOf := make(map[string]int, len(builtins)+len(overrides))
	for _, built := range builtins {
		rankOf[built.name] = built.rank
	}
	for name, rank := range overrides {
		if name == "" {
			return table{}, fmt.Errorf("priority: expected a non-empty priority name, found empty")
		}
		rankOf[name] = rank
	}
	grouped := map[int][]string{}
	for name, rank := range rankOf {
		grouped[rank] = append(grouped[rank], name)
	}
	out := table{names: map[int]string{}, problems: map[int]error{}}
	for rank, names := range grouped {
		chosen, err := chooseCanonical(builtins, rank, names)
		if err != nil {
			out.problems[rank] = err
			continue
		}
		out.names[rank] = chosen
	}
	return out, nil
}

func chooseCanonical(builtins []struct {
	name string
	rank int
}, rank int, names []string,
) (string, error) {
	slices.Sort(names)
	for _, built := range builtins {
		if built.rank == rank && slices.Contains(names, built.name) {
			return built.name, nil
		}
	}
	if len(names) == 1 {
		return names[0], nil
	}
	return "", fmt.Errorf("priority: expected one name for rank %d, found %s", rank, quoteNames(names))
}

func githubTable(cfg config.Source) (table, error) {
	if len(cfg.PriorityLabels) == 0 {
		if cfg.PriorityField == "" {
			return table{names: map[int]string{}, problems: map[int]error{}}, nil
		}
		return rankedTable(builtinGitHubRanks, cfg.PriorityRanks)
	}
	grouped := map[int][]string{}
	for name, rank := range cfg.PriorityLabels {
		if name == "" {
			return table{}, fmt.Errorf("priority: expected a non-empty priority label, found empty")
		}
		grouped[rank] = append(grouped[rank], name)
	}
	out := table{names: map[int]string{}, problems: map[int]error{}}
	for rank, names := range grouped {
		slices.Sort(names)
		if len(names) == 1 {
			out.names[rank] = names[0]
			continue
		}
		out.problems[rank] = fmt.Errorf("priority: expected one name for rank %d, found %s", rank, quoteNames(names))
	}
	return out, nil
}

func quoteNames(names []string) string {
	quoted := make([]string, len(names))
	for i, name := range names {
		quoted[i] = strconv.Quote(name)
	}
	return strings.Join(quoted, ", ")
}
