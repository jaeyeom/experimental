package tui

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"time"
)

// ctrlEnterSeqs are the Ctrl+Enter sequences some terminals send with no
// keyboard mode switched on: xterm modifyOtherKeys and the kitty form.
var ctrlEnterSeqs = [][]byte{
	[]byte("\x1b[27;5;13~"),
	[]byte("\x1b[13;5u"),
}

// altEnter is what a Ctrl+Enter sequence becomes. Bubbletea v1 parses it
// as alt+enter, which sends from any composer.
var altEnter = []byte("\x1b\r")

// ctrlEnterWait bounds how long a read that ends partway into a Ctrl+Enter
// sequence waits for the rest. A lone Esc is late by at most this much.
const ctrlEnterWait = 25 * time.Millisecond

// ctrlEnterReader rewrites each Ctrl+Enter sequence to Alt+Enter before
// bubbletea parses the input. Bubbletea v1 reports those sequences only
// as an unexported message whose bytes share its read buffer, so the
// model cannot read them safely. The rewrite stays in the byte stream, so
// keys keep their order and nothing is read after Read returns.
//
// ready reports whether more input arrives within a wait. When a read
// ends with the start of a sequence, Read uses it to finish the sequence
// in the same call. Nil ready never waits.
type ctrlEnterReader struct {
	in    io.Reader
	ready func(time.Duration) bool
	log   io.Writer
}

func newCtrlEnterReader(in io.Reader, ready func(time.Duration) bool, log io.Writer) *ctrlEnterReader {
	return &ctrlEnterReader{in: in, ready: ready, log: log}
}

func (r *ctrlEnterReader) Read(p []byte) (int, error) {
	n, err := r.in.Read(p)
	for err == nil && n < len(p) && r.ready != nil && endsInCtrlEnterPrefix(p[:n]) && r.ready(ctrlEnterWait) {
		var more int
		more, err = r.in.Read(p[n:])
		n += more
	}
	return r.rewrite(p[:n]), err //nolint:wrapcheck // io.Reader passes the input error through
}

// rewrite replaces each Ctrl+Enter sequence in b, in place, and returns
// the new length. Each replacement is shorter than the sequence.
func (r *ctrlEnterReader) rewrite(b []byte) int {
	w := 0
	for i := 0; i < len(b); {
		if seq := ctrlEnterAt(b[i:]); seq != nil {
			if r.log != nil {
				fmt.Fprintf(r.log, "ctrl+enter bytes=%q as alt+enter\n", seq)
			}
			w += copy(b[w:], altEnter)
			i += len(seq)
			continue
		}
		b[w] = b[i]
		w++
		i++
	}
	return w
}

func ctrlEnterAt(b []byte) []byte {
	for _, seq := range ctrlEnterSeqs {
		if bytes.HasPrefix(b, seq) {
			return seq
		}
	}
	return nil
}

// endsInCtrlEnterPrefix reports whether b ends with the start, but not
// the whole, of a Ctrl+Enter sequence.
func endsInCtrlEnterPrefix(b []byte) bool {
	for _, seq := range ctrlEnterSeqs {
		for k := min(len(seq)-1, len(b)); k > 0; k-- {
			if bytes.HasSuffix(b, seq[:k]) {
				return true
			}
		}
	}
	return false
}

// ctrlEnterFile is a terminal file whose reads go through ctrlEnterReader.
// It keeps the file's Fd and Name, so bubbletea still puts the terminal
// in raw mode and can cancel a blocked read.
type ctrlEnterFile struct {
	*os.File
	r *ctrlEnterReader
}

func (f ctrlEnterFile) Read(p []byte) (int, error) {
	return f.r.Read(p)
}

// programInput wraps the inbox input so Ctrl+Enter sends. A file waits
// briefly for a split sequence. log, when set, gets one line per rewrite.
func programInput(in io.Reader, log io.Writer) io.Reader {
	if f, ok := in.(*os.File); ok {
		return ctrlEnterFile{File: f, r: newCtrlEnterReader(f, inputReady(f), log)}
	}
	return newCtrlEnterReader(in, nil, log)
}
