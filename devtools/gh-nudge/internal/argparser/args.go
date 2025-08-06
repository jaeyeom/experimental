package argparser

import (
	"fmt"
	"strings"
)

// ArgParser represents a parsed command line with options and positional
// arguments.
type ArgParser struct {
	options     map[string][]string // option name -> values
	positionals []string            // positional arguments
	help        bool                // whether help was requested
}

// NewArgParser creates a new argument parser from the given arguments.
func NewArgParser(args []string) *ArgParser {
	parser := &ArgParser{
		options:     make(map[string][]string),
		positionals: make([]string, 0),
		help:        false,
	}

	parser.parse(args)
	return parser
}

// parse processes the arguments according to standard CLI conventions.
func (p *ArgParser) parse(args []string) {
	i := 0

	for i < len(args) {
		arg := args[i]

		// Everything after -- is treated as positional arguments
		if arg == "--" {
			i++
			p.addRemainingAsPositional(args[i:])
			break
		}

		// Check for help flags
		if p.isHelpFlag(arg) {
			p.help = true
			i++
			continue
		}

		// Handle different argument types
		if consumed := p.handleArgument(arg, args, i); consumed > 0 {
			i += consumed
			continue
		}

		// Positional argument
		p.positionals = append(p.positionals, arg)
		i++
	}
}

func (p *ArgParser) isHelpFlag(arg string) bool {
	return arg == "-h" || arg == "--help"
}

func (p *ArgParser) addRemainingAsPositional(args []string) {
	p.positionals = append(p.positionals, args...)
}

func (p *ArgParser) handleArgument(arg string, args []string, i int) int {
	// Long options (--option or --option=value)
	if strings.HasPrefix(arg, "--") {
		return p.handleLongOption(arg, args, i)
	}

	// Short options (-o or -o value)
	if strings.HasPrefix(arg, "-") && len(arg) > 1 {
		return p.handleShortOption(arg, args, i)
	}

	return 0 // Not an option
}

func (p *ArgParser) handleLongOption(arg string, args []string, i int) int {
	if strings.Contains(arg, "=") {
		// --option=value format
		parts := strings.SplitN(arg, "=", 2)
		optionName := parts[0][2:] // remove --
		optionValue := parts[1]
		p.addOption(optionName, optionValue)
		return 1
	}

	// --option format, next arg might be value
	optionName := arg[2:] // remove --
	return p.handleOptionWithPossibleValue(optionName, args, i)
}

func (p *ArgParser) handleShortOption(arg string, args []string, i int) int {
	optionName := arg[1:] // remove -
	return p.handleOptionWithPossibleValue(optionName, args, i)
}

func (p *ArgParser) handleOptionWithPossibleValue(optionName string, args []string, i int) int {
	// Check if next argument is a value (not starting with - and not empty)
	if p.hasNextValue(args, i) {
		if p.isBooleanFlag(optionName) {
			p.addOption(optionName, "true")
			return 1
		}
		p.addOption(optionName, args[i+1])
		return 2 // consumed current arg and next arg
	}
	// Boolean flag
	p.addOption(optionName, "true")
	return 1
}

func (p *ArgParser) hasNextValue(args []string, i int) bool {
	return i+1 < len(args) && !strings.HasPrefix(args[i+1], "-") && args[i+1] != ""
}

// addOption adds an option value, supporting multiple values for the same option.
func (p *ArgParser) addOption(name, value string) {
	if _, exists := p.options[name]; !exists {
		p.options[name] = make([]string, 0)
	}
	p.options[name] = append(p.options[name], value)
}

// isBooleanFlag checks if an option is a known boolean flag.
func (p *ArgParser) isBooleanFlag(name string) bool {
	// Common boolean flags that shouldn't consume the next argument
	booleanFlags := map[string]bool{
		"verbose":          true,
		"v":                true,
		"debug":            true,
		"d":                true,
		"quiet":            true,
		"q":                true,
		"help":             true,
		"h":                true,
		"version":          true,
		"V":                true,
		"force":            true,
		"recursive":        true,
		"r":                true,
		"all":              true,
		"a":                true,
		"dry-run":          true,
		"compress":         true,
		"detailed":         true,
		"pretty":           true,
		"json":             true,
		"yaml":             true,
		"backup":           true,
		"merge":            true,
		"overwrite":        true,
		"list":             true,
		"l":                true,
		"release":          true,
		"status":           true,
		"migrate":          true,
		"verify":           true,
		"defragment":       true,
		"include-metadata": true,
	}

	return booleanFlags[name]
}

// HasOption checks if an option was provided.
func (p *ArgParser) HasOption(name string) bool {
	_, exists := p.options[name]
	return exists
}

// GetOption returns the first value for an option, or empty string if not
// found.
func (p *ArgParser) GetOption(name string) string {
	if values, exists := p.options[name]; exists && len(values) > 0 {
		return values[0]
	}
	return ""
}

// GetOptionValues returns all values for an option.
func (p *ArgParser) GetOptionValues(name string) []string {
	if values, exists := p.options[name]; exists {
		return values
	}
	return nil
}

// GetPositional returns the positional argument at the given index.
func (p *ArgParser) GetPositional(index int) string {
	if index >= 0 && index < len(p.positionals) {
		return p.positionals[index]
	}
	return ""
}

// GetPositionals returns all positional arguments.
func (p *ArgParser) GetPositionals() []string {
	return p.positionals
}

// PositionalCount returns the number of positional arguments.
func (p *ArgParser) PositionalCount() int {
	return len(p.positionals)
}

// IsHelp returns true if help was requested.
func (p *ArgParser) IsHelp() bool {
	return p.help
}

// ValidateOptions checks if all provided options are in the allowed list.
func (p *ArgParser) ValidateOptions(allowedOptions []string) error {
	allowed := make(map[string]bool)
	for _, opt := range allowedOptions {
		allowed[opt] = true
	}

	for optionName := range p.options {
		if !allowed[optionName] {
			return fmt.Errorf("unknown option: --%s", optionName)
		}
	}

	return nil
}

// RequirePositionals ensures the minimum number of positional arguments.
func (p *ArgParser) RequirePositionals(minCount int, usage string) error {
	if len(p.positionals) < minCount {
		return fmt.Errorf("insufficient arguments. Usage: %s", usage)
	}
	return nil
}

// RequireExactPositionals ensures exactly the specified number of positional arguments.
func (p *ArgParser) RequireExactPositionals(count int, usage string) error {
	if len(p.positionals) != count {
		return fmt.Errorf("expected %d arguments, got %d. Usage: %s", count, len(p.positionals), usage)
	}
	return nil
}
