package tui

import (
	"bytes"
	"io"
	"os"
	"reflect"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

// chunk is one Read worth of terminal input. A late chunk is not ready
// within the reader's wait, like a key pressed after a pause.
type chunk struct {
	data string
	late bool
}

// chunkReader returns one chunk per Read, then io.EOF.
type chunkReader struct {
	chunks []chunk
}

func (r *chunkReader) Read(p []byte) (int, error) {
	if len(r.chunks) == 0 {
		return 0, io.EOF
	}
	n := copy(p, r.chunks[0].data)
	if n < len(r.chunks[0].data) {
		r.chunks[0].data = r.chunks[0].data[n:]
	} else {
		r.chunks = r.chunks[1:]
	}
	return n, nil
}

// ready reports whether the next chunk arrives within the wait.
func (r *chunkReader) ready(time.Duration) bool {
	return len(r.chunks) > 0 && !r.chunks[0].late
}

// readAll returns what each Read call of r returned, until io.EOF.
func readAll(t *testing.T, r io.Reader) []string {
	t.Helper()
	var reads []string
	buf := make([]byte, 256)
	for {
		n, err := r.Read(buf)
		if n > 0 {
			reads = append(reads, string(buf[:n]))
		}
		if err == io.EOF {
			return reads
		}
		if err != nil {
			t.Fatal(err)
		}
	}
}

func TestCtrlEnterReader(t *testing.T) {
	for name, tc := range map[string]struct {
		in   []chunk
		want []string
	}{
		"modifyOtherKeys": {
			in:   []chunk{{data: "\x1b[27;5;13~"}},
			want: []string{"\x1b\r"},
		},
		"kitty": {
			in:   []chunk{{data: "\x1b[13;5u"}},
			want: []string{"\x1b\r"},
		},
		"between other input": {
			in:   []chunk{{data: "a\x1b[13;5ub\x1b[27;5;13~c"}},
			want: []string{"a\x1b\rb\x1b\rc"},
		},
		"split across reads": {
			in:   []chunk{{data: "hi\x1b[27;"}, {data: "5;13"}, {data: "~"}},
			want: []string{"hi\x1b\r"},
		},
		"split after Esc": {
			in:   []chunk{{data: "\x1b"}, {data: "[13;5u"}},
			want: []string{"\x1b\r"},
		},
		"lone Esc is not held": {
			in:   []chunk{{data: "\x1b"}, {data: "x", late: true}},
			want: []string{"\x1b", "x"},
		},
		"prefix that never completes": {
			in:   []chunk{{data: "\x1b[27;5"}, {data: "q", late: true}},
			want: []string{"\x1b[27;5", "q"},
		},
		"prefix that turns into another key": {
			in:   []chunk{{data: "\x1b[27;5;1"}, {data: "4~"}},
			want: []string{"\x1b[27;5;14~"},
		},
		"other CSI sequences unchanged": {
			in:   []chunk{{data: "\x1b[13;2u\x1b[27;2;13~\x1b[A\x1b[1;5A\x1b[13u\x1b[27;5;9~"}},
			want: []string{"\x1b[13;2u\x1b[27;2;13~\x1b[A\x1b[1;5A\x1b[13u\x1b[27;5;9~"},
		},
	} {
		src := &chunkReader{chunks: tc.in}
		got := readAll(t, newCtrlEnterReader(src, src.ready, nil))
		if !reflect.DeepEqual(got, tc.want) {
			t.Errorf("%s: reads %q, want %q", name, got, tc.want)
		}
	}
}

func TestCtrlEnterReaderWithoutWaitPassesPrefixes(t *testing.T) {
	src := &chunkReader{chunks: []chunk{{data: "\x1b[13;"}, {data: "5u"}}}
	got := readAll(t, newCtrlEnterReader(src, nil, nil))
	if want := []string{"\x1b[13;", "5u"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("reads %q, want %q", got, want)
	}
}

func TestProgramInputKeepsTerminalFile(t *testing.T) {
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	defer r.Close()
	defer w.Close()
	in := programInput(r, nil)
	f, ok := in.(interface {
		io.ReadWriteCloser
		Fd() uintptr
		Name() string
	})
	if !ok {
		t.Fatalf("program input %T hides the file, so raw mode and cancel break", in)
	}
	if f.Fd() != r.Fd() || f.Name() != r.Name() {
		t.Fatalf("program input fd %d %q, want %d %q", f.Fd(), f.Name(), r.Fd(), r.Name())
	}
	if _, err := w.Write([]byte("\x1b[13;5u")); err != nil {
		t.Fatal(err)
	}
	buf := make([]byte, 16)
	n, err := f.Read(buf)
	if err != nil || string(buf[:n]) != "\x1b\r" {
		t.Fatalf("read %q %v, want Alt+Enter", buf[:n], err)
	}
}

// TestCtrlEnterSendsNoteThroughProgram feeds each Ctrl+Enter sequence
// through the program input and checks it sends the note draft, the same
// as Tab then Enter.
func TestCtrlEnterSendsNoteThroughProgram(t *testing.T) {
	m, led, item := composerModel(t, composerNote)
	sends := typeDraft(t, m)
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyTab})
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
	want := sendResult(t, m, led, item.ID, *sends)

	for name, seq := range map[string]string{
		"modifyOtherKeys": "\x1b[27;5;13~",
		"kitty":           "\x1b[13;5u",
	} {
		m, led, item := composerModel(t, composerNote)
		sent := make(chan struct{}, 1)
		count := 0
		m.runSend = func([]string) error {
			count++
			sent <- struct{}{}
			return nil
		}
		r, w := io.Pipe()
		var out bytes.Buffer
		p := newProgram(m, r, &out, nil)
		done := make(chan error, 1)
		go func() {
			_, err := p.Run()
			done <- err
		}()
		if _, err := w.Write([]byte("hi" + seq)); err != nil {
			t.Fatal(err)
		}
		select {
		case <-sent:
		case <-time.After(5 * time.Second):
			t.Fatalf("%s: note was not sent", name)
		}
		if err := w.Close(); err != nil {
			t.Fatal(err)
		}
		p.Quit()
		if err := <-done; err != nil {
			t.Fatal(err)
		}
		if got := sendResult(t, m, led, item.ID, count); got != want {
			t.Errorf("%s:\n got %s\nwant %s", name, got, want)
		}
	}
}
