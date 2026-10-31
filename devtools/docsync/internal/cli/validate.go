package cli

import (
	"fmt"
	"io"
	"path/filepath"
	"strings"

	"github.com/jaeyeom/experimental/devtools/docsync/internal/mapping"
	"github.com/spf13/cobra"
)

func newValidateCmd(stdout io.Writer) *cobra.Command {
	var configPath string
	cmd := &cobra.Command{
		Use:   "validate",
		Short: "Lint a docsync.yml mapping file",
		Args:  cobra.NoArgs,
		RunE: func(_ *cobra.Command, _ []string) error {
			return runValidate(stdout, configPath)
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "path to docsync.yml")
	return cmd
}

func runValidate(stdout io.Writer, configPath string) error {
	path, err := resolveMappingPath(configPath)
	if err != nil {
		return err
	}
	m, err := mapping.ParseFile(path)
	if err != nil {
		return wrapMappingOpenErr(configPath, err)
	}
	issues := append(m.SchemaIssues(), m.Lint()...)
	if err := writeValidateReport(stdout, filepath.Base(m.Path), issues); err != nil {
		return err
	}
	return validateExit(issues)
}

func writeValidateReport(w io.Writer, label string, issues []mapping.Issue) error {
	var b strings.Builder
	if len(issues) == 0 {
		b.WriteString(label)
		b.WriteString(": ok\n")
	} else {
		fmt.Fprintf(&b, "%s: %d problems\n", label, len(issues))
		for _, iss := range issues {
			fmt.Fprintf(&b, "  %-5s  %s\n", iss.Severity, iss.Message)
		}
	}
	if _, err := io.WriteString(w, b.String()); err != nil {
		return fmt.Errorf("write result: %w", err)
	}
	return nil
}

func validateExit(issues []mapping.Issue) error {
	if hasSeverity(issues, mapping.SeverityError) {
		return &ExitError{Code: ExitUsage}
	}
	if hasSeverity(issues, mapping.SeverityWarn) {
		return &ExitError{Code: ExitDocsAffected}
	}
	return nil
}

func hasSeverity(issues []mapping.Issue, sev mapping.Severity) bool {
	for _, iss := range issues {
		if iss.Severity == sev {
			return true
		}
	}
	return false
}
