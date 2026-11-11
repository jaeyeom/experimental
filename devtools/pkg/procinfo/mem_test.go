package procinfo

import (
	"context"
	"path/filepath"
	"testing"
)

func TestParseMeminfo(t *testing.T) {
	t.Parallel()

	got, err := ParseMeminfo("MemTotal:       16384000 kB\nMemFree:          100 kB\nMemAvailable:    2048 kB\n")
	if err != nil || got != 2048*1024 {
		t.Fatalf("ParseMeminfo() = %d, %v; want %d", got, err, 2048*1024)
	}
	if _, err := ParseMeminfo("MemTotal: 1 kB\n"); err == nil {
		t.Fatal("ParseMeminfo() without MemAvailable: want an error")
	}
}

func TestParseVMStat(t *testing.T) {
	t.Parallel()

	out := "Mach Virtual Memory Statistics: (page size of 16384 bytes)\n" +
		"Pages free:                                     4461.\n" +
		"Pages active:                                 189098.\n" +
		"Pages inactive:                               188110.\n" +
		"Pages speculative:                               151.\n" +
		"Pages purgeable:                                   2.\n"
	got, err := ParseVMStat(out)
	if want := int64(4461+188110+2) * 16384; err != nil || got != want {
		t.Fatalf("ParseVMStat() = %d, %v; want %d", got, err, want)
	}
	if _, err := ParseVMStat("Pages free: 1.\n"); err == nil {
		t.Fatal("ParseVMStat() without a page size: want an error")
	}
	if _, err := ParseVMStat("Mach Virtual Memory Statistics: (page size of 4096 bytes)\nPages active: 1.\n"); err == nil {
		t.Fatal("ParseVMStat() without free pages: want an error")
	}
}

func TestMeminfoFile(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "meminfo")
	writeFile(t, path, "MemAvailable: 10 kB\n")
	got, err := MeminfoFile{Path: path}.Available(context.Background())
	if err != nil || got != 10*1024 {
		t.Fatalf("Available() = %d, %v", got, err)
	}
}

func TestVMStat(t *testing.T) {
	t.Parallel()

	m := VMStat{Run: func(_ context.Context, name string, args ...string) (string, error) {
		if name != "vm_stat" || len(args) != 0 {
			t.Fatalf("ran %s %v, want vm_stat", name, args)
		}
		return "Mach Virtual Memory Statistics: (page size of 4096 bytes)\nPages free: 1.\nPages inactive: 2.\nPages purgeable: 3.\n", nil
	}}
	got, err := m.Available(context.Background())
	if err != nil || got != 6*4096 {
		t.Fatalf("Available() = %d, %v", got, err)
	}
}
