package executor

import (
	"context"
	"os/exec"
	"strings"
)

// CommandBuilder defines the strategy for building an exec.Cmd from command and arguments.
// This allows different execution strategies (direct execution vs shell execution) to be
// used without coupling the executor to specific tool behaviors.
type CommandBuilder interface {
	// Build creates an exec.Cmd configured for the given command and arguments.
	Build(ctx context.Context, command string, args []string) *exec.Cmd
}

// DirectCommandBuilder executes commands directly without a shell intermediary.
// This is the default and preferred method for most commands as it's more secure
// and avoids shell interpretation issues.
type DirectCommandBuilder struct{}

// Build creates a command that executes directly.
func (d *DirectCommandBuilder) Build(ctx context.Context, command string, args []string) *exec.Cmd {
	// #nosec G204 - This is intentional as we need to execute external tools with user-provided arguments
	return exec.CommandContext(ctx, command, args...)
}

// ShellCommandBuilder executes commands through a shell (sh -c).
// This is useful for tools with client-server architectures (like Bazel, Gradle)
// that work better when executed in a proper shell environment.
type ShellCommandBuilder struct{}

// Build creates a command that executes through a shell.
func (s *ShellCommandBuilder) Build(ctx context.Context, command string, args []string) *exec.Cmd {
	fullCommand := buildShellCommand(command, args)
	// #nosec G204 - This is intentional as we need to execute external tools with user-provided arguments
	return exec.CommandContext(ctx, "sh", "-c", fullCommand)
}

// buildShellCommand constructs a properly quoted shell command string.
// All arguments and the command itself are quoted to prevent shell injection.
func buildShellCommand(command string, args []string) string {
	// Always quote the command to prevent injection via command name
	parts := []string{shellQuote(command)}

	// Always quote all arguments to prevent injection via shell metacharacters
	for _, arg := range args {
		parts = append(parts, shellQuote(arg))
	}
	return strings.Join(parts, " ")
}

// shellQuote safely quotes a string for shell execution using single quotes.
// Single quotes preserve the literal value of all characters, making it safe
// from shell injection. Any single quotes in the input are properly escaped.
func shellQuote(s string) string {
	// Use single quotes to preserve literal values and prevent interpretation
	// of shell metacharacters like $, `, ;, |, &, etc.
	// To include a literal single quote, we end the quoted string, add an
	// escaped single quote, and start a new quoted string: '...'\'...'
	// which is represented as: '..."'"...
	return "'" + strings.ReplaceAll(s, "'", "'\"'\"'") + "'"
}
