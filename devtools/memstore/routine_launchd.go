package memstore

import (
	"bytes"
	"encoding/xml"
	"fmt"
)

// LaunchdPlistOptions configures macOS launchd plist generation for
// `memstore routine tick`.
type LaunchdPlistOptions struct {
	Label           string
	IntervalSeconds int
	BinaryPath      string
	ConfigPath      string
	RoutinesDir     string
}

// RenderLaunchdPlist renders a valid macOS launchd plist XML document that
// periodically invokes `memstore routine tick`.
func RenderLaunchdPlist(opts LaunchdPlistOptions) ([]byte, error) {
	if opts.IntervalSeconds <= 0 {
		return nil, fmt.Errorf("interval-seconds must be positive, got %d", opts.IntervalSeconds)
	}
	label := opts.Label
	if label == "" {
		label = "com.jaeyeom.memstore.tick"
	}
	bin := opts.BinaryPath
	if bin == "" {
		bin = "memstore"
	}

	args := []string{bin}
	if opts.ConfigPath != "" {
		args = append(args, "--config", opts.ConfigPath)
	}
	args = append(args, "routine", "tick")
	if opts.RoutinesDir != "" {
		args = append(args, "--routines-dir", opts.RoutinesDir)
	}

	var b bytes.Buffer
	b.WriteString(`<?xml version="1.0" encoding="UTF-8"?>` + "\n")
	b.WriteString(`<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">` + "\n")
	b.WriteString(`<plist version="1.0">` + "\n")
	b.WriteString("  <dict>\n")
	b.WriteString("    <key>Label</key>\n")
	b.WriteString("    <string>")
	if err := xml.EscapeText(&b, []byte(label)); err != nil {
		return nil, fmt.Errorf("escape label: %w", err)
	}
	b.WriteString("</string>\n")
	b.WriteString("    <key>ProgramArguments</key>\n")
	b.WriteString("    <array>\n")
	for _, arg := range args {
		b.WriteString("      <string>")
		if err := xml.EscapeText(&b, []byte(arg)); err != nil {
			return nil, fmt.Errorf("escape arg %q: %w", arg, err)
		}
		b.WriteString("</string>\n")
	}
	b.WriteString("    </array>\n")
	fmt.Fprintf(&b, "    <key>StartInterval</key>\n    <integer>%d</integer>\n", opts.IntervalSeconds)
	b.WriteString("    <key>RunAtLoad</key>\n    <true/>\n")
	b.WriteString("  </dict>\n")
	b.WriteString("</plist>\n")
	return b.Bytes(), nil
}
