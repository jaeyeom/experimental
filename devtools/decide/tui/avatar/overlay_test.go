package avatar

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/muesli/termenv"
)

func TestOverlayServesMemoryFilesFirst(t *testing.T) {
	disk := filepath.Join(t.TempDir(), "d.txt")
	if err := os.WriteFile(disk, []byte("DD"), 0o600); err != nil {
		t.Fatal(err)
	}
	r := NewRenderer(Overlay{
		"hub:0.txt": {Data: []byte("MM")},
		"hub:1.png": {Err: "file not found on the hub"},
	})
	lines, err := r.Render("hub:0.txt", 4, 1, termenv.Ascii)
	if err != nil || !strings.HasPrefix(lines[0], "MM") {
		t.Fatalf("memory file = %q, %v", lines, err)
	}
	if _, err := r.Render("hub:1.png", 4, 1, termenv.Ascii); err == nil || !strings.Contains(err.Error(), "not found on the hub") {
		t.Fatalf("missing memory file = %v", err)
	}
	lines, err = r.Render(disk, 4, 1, termenv.Ascii)
	if err != nil || !strings.HasPrefix(lines[0], "DD") {
		t.Fatalf("disk file = %q, %v", lines, err)
	}
}
