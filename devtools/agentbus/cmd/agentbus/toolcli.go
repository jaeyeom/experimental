package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newAgentbusTool() toolcli.Tool {
	return newAgentbusToolWithEnv(os.Environ())
}

func newAgentbusToolWithEnv(env []string) toolcli.Tool {
	return toolcli.Tool{
		Name:        "agentbus",
		MainPackage: "github.com/jaeyeom/experimental/devtools/agentbus/cmd/agentbus",
		Companions: []toolcli.Companion{
			{
				Name:          "agentbusd",
				Package:       "github.com/jaeyeom/experimental/devtools/agentbus/cmd/agentbusd",
				DaemonService: "agentbusd",
			},
		},
		SkillText: skillText,
		ConfigPath: func() (string, bool) {
			p, err := config.Path(env)
			return p, err == nil
		},
		CheckConfig: func() error {
			if _, err := config.Load(env); err != nil {
				return fmt.Errorf("load agentbus config: %w", err)
			}
			return nil
		},
		LookPath: func(file string) (string, error) {
			return lookPathIn(env, file)
		},
		RequiredCommands:  []string{"herdr"},
		ExtraDoctorChecks: agentbusDoctorChecks(env),
		DoctorUsage:       "agentbus doctor [--sandbox] [--json] [--offline]",
		RegisterDoctorFlags: func(fs *flag.FlagSet) func(context.Context, io.Writer, io.Writer) (int, bool) {
			sandbox := fs.Bool("sandbox", false, "print the sandbox allowlist this machine needs and test each connection")
			return func(ctx context.Context, stdout, stderr io.Writer) (int, bool) {
				if !*sandbox {
					return 0, false
				}
				return runDoctorSandbox(ctx, env, stdout, stderr), true
			}
		},
	}
}

func shouldHandleToolCLI(args []string) bool {
	if len(args) == 0 {
		return false
	}
	switch args[0] {
	case "version", "update", "doctor", "skill":
		return true
	default:
		return false
	}
}
