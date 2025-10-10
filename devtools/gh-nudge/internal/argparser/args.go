// Package argparser provides a simple command-line argument parser that follows
// standard Unix conventions for parsing flags and positional arguments.
//
// It supports:
//   - Short flags: -v, -h
//   - Long flags: --verbose, --help
//   - Flags with values: --output=file, --output file
//   - Boolean flags that don't consume the next argument
//   - Positional arguments
//   - Double dash (--) to mark end of flags
//   - Help flag detection (-h, --help)
//
// Example usage:
//
//	parser := NewArgParser(os.Args[1:])
//	if parser.IsHelp() {
//	    fmt.Println("Usage: myprogram [options] files...")
//	    return
//	}
//	verbose := parser.HasOption("verbose")
//	output := parser.GetOption("output")
//	files := parser.GetPositionals()
package argparser

import (
	"fmt"
	"strings"
)

// ArgParser represents a parsed command line with options and positional
// arguments.
type ArgParser struct {
	options      map[string][]string // option name -> values
	positionals  []string            // positional arguments
	help         bool                // whether help was requested
	booleanFlags map[string]bool     // configurable boolean flags
}

// Option is a functional option for configuring ArgParser.
type Option func(*ArgParser)

// WithBooleanFlags configures custom boolean flags for the parser.
// These flags will not consume the next argument as their value.
func WithBooleanFlags(flags ...string) Option {
	return func(p *ArgParser) {
		for _, flag := range flags {
			p.booleanFlags[flag] = true
		}
	}
}

// NewArgParser creates a new argument parser from the given arguments.
// It accepts optional functional options to configure the parser behavior.
func NewArgParser(args []string, options ...Option) *ArgParser {
	parser := &ArgParser{
		options:      make(map[string][]string),
		positionals:  make([]string, 0),
		help:         false,
		booleanFlags: getDefaultBooleanFlags(),
	}

	// Apply functional options
	for _, option := range options {
		option(parser)
	}

	parser.parse(args)
	return parser
}

// parse processes the arguments according to standard CLI conventions.
// It handles flags, options with values, help flags, and positional arguments.
// Everything after -- is treated as positional arguments.
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
		if consumed := p.handleArgument(args[i:]); consumed > 0 {
			i += consumed
			continue
		}

		// Positional argument
		p.positionals = append(p.positionals, arg)
		i++
	}
}

// isHelpFlag checks if the argument is a help flag (-h or --help).
func (p *ArgParser) isHelpFlag(arg string) bool {
	return arg == "-h" || arg == "--help"
}

// addRemainingAsPositional adds all remaining arguments as positional arguments.
// This is used when -- is encountered to treat everything after as positional.
func (p *ArgParser) addRemainingAsPositional(args []string) {
	p.positionals = append(p.positionals, args...)
}

// handleArgument processes a single argument and returns the number of
// arguments consumed. It determines whether the argument is a long option,
// short option, or positional argument.
//
// Parameters:
//   - args: slice starting with the current argument (args[0] is the argument
//     to process)
//
// Returns the number of arguments consumed (1 for flag, 2 for option with value, 0 for positional).
func (p *ArgParser) handleArgument(args []string) int {
	if len(args) == 0 {
		return 0
	}
	arg := args[0]

	// Long options (--option or --option=value)
	if strings.HasPrefix(arg, "--") {
		return p.handleLongOption(args)
	}

	// Short options (-o or -o value)
	if strings.HasPrefix(arg, "-") && len(arg) > 1 {
		return p.handleShortOption(args)
	}

	return 0 // Not an option
}

// handleLongOption processes long options (--option or --option=value).
//
// Parameters:
//   - args: slice starting with the long option to process
//
// Returns the number of arguments consumed.
func (p *ArgParser) handleLongOption(args []string) int {
	arg := args[0]
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
	return p.handleOptionWithPossibleValue(optionName, args)
}

// handleShortOption processes short options (-o or -o value).
//
// Parameters:
//   - args: slice starting with the short option to process
//
// Returns the number of arguments consumed.
func (p *ArgParser) handleShortOption(args []string) int {
	arg := args[0]
	optionName := arg[1:] // remove -
	return p.handleOptionWithPossibleValue(optionName, args)
}

// handleOptionWithPossibleValue processes an option that may or may not have a value.
// It checks if the next argument is a value or if this is a boolean flag.
//
// Parameters:
//   - optionName: the name of the option (without dashes)
//   - args: slice starting with the current option
//
// Returns the number of arguments consumed.
func (p *ArgParser) handleOptionWithPossibleValue(optionName string, args []string) int {
	// Check if next argument is a value (not starting with - and not empty)
	if p.hasNextValue(args) {
		if p.isBooleanFlag(optionName) {
			p.addOption(optionName, "true")
			return 1
		}
		p.addOption(optionName, args[1])
		return 2 // consumed current arg and next arg
	}
	// Boolean flag
	p.addOption(optionName, "true")
	return 1
}

// hasNextValue checks if there's a next argument that could be a value
// (not starting with - and not empty).
//
// Parameters:
//   - args: slice where args[1] would be the potential value
func (p *ArgParser) hasNextValue(args []string) bool {
	return len(args) > 1 && !strings.HasPrefix(args[1], "-") && args[1] != ""
}

// addOption adds an option value, supporting multiple values for the same
// option.
func (p *ArgParser) addOption(name, value string) {
	if _, exists := p.options[name]; !exists {
		p.options[name] = make([]string, 0)
	}
	p.options[name] = append(p.options[name], value)
}

// getDefaultBooleanFlags returns the default set of boolean flags for backward compatibility.
func getDefaultBooleanFlags() map[string]bool {
	return map[string]bool{
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
}

// isBooleanFlag checks if an option is a configured boolean flag. Boolean flags
// don't consume the next argument as their value. This helps distinguish
// between 'cmd -v file.txt' (verbose flag + file) vs 'cmd --output file.txt'
// (output option with value).
//
// Boolean flags can now be configured using WithBooleanFlags() option when
// creating the parser, providing flexibility while maintaining backward
// compatibility with common boolean flags.
func (p *ArgParser) isBooleanFlag(name string) bool {
	return p.booleanFlags[name]
}

// HasOption checks if an option was provided.
//
// WARNING: Do NOT use HasOption for boolean flag checking!
// HasOption returns true if the option exists, regardless of its value.
// This means --force=false will return true, which is incorrect for boolean logic.
//
// For boolean flags, use GetBoolOption() instead:
//
//	// Wrong - treats --force=false as true:
//	if parser.HasOption("force") { ... }
//
//	// Correct - properly handles --force=false:
//	if parser.GetBoolOption("force") { ... }
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

// GetBoolOption returns the boolean value of a flag option.
// It returns true if the flag is present and set to a truthy value,
// false otherwise.
//
// IMPORTANT: This method ALWAYS returns false when the flag is absent.
// It is designed for "opt-in" flags that default to false.
// If you need a flag that defaults to true (e.g., --no-verify to disable),
// you must handle that logic separately in your code.
//
// Truthy values: "true", "1", "yes", "y", "on"
// Falsy values: "false", "0", "no", "n", "off", or absent
//
// Examples:
//
//	--verbose           -> true  (implicitly set to "true")
//	--verbose=true      -> true
//	--verbose=false     -> false
//	--verbose=1         -> true
//	--verbose=0         -> false
//	(no flag)           -> false (always defaults to false)
func (p *ArgParser) GetBoolOption(name string) bool {
	value := p.GetOption(name)
	if value == "" {
		return false
	}

	// Normalize to lowercase for comparison
	switch value {
	case "true", "1", "yes", "y", "on":
		return true
	case "false", "0", "no", "n", "off":
		return false
	default:
		// For any other value, treat as false
		return false
	}
}

// GetOptionValues returns all values for an option.
// Returns nil if the option was not provided.
func (p *ArgParser) GetOptionValues(name string) []string {
	if values, exists := p.options[name]; exists {
		return values
	}
	return nil
}

// GetPositional returns the positional argument at the given index.
// Returns empty string if the index is out of bounds.
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
