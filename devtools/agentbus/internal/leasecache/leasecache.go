// Package leasecache remembers, for a short time, that a pane holds the
// lease on a checkout, so the lease hook does not ask the daemon on
// every tool call. Only "you hold it" is cached; "you do not" never is.
//
// Each checkout has a directory of entries, one per pane, holding the
// time the check that allowed it began. Drop stamps the checkout's
// directory, and an entry from a check that began before the stamp is
// stale, so a check racing a release cannot bring a holder back.
package leasecache

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// TTL is how long a positive answer is trusted.
const TTL = 30 * time.Second

// droppedName is the stamp Drop writes in a checkout's directory.
const droppedName = "dropped"

// Cache is a lease answer cache in Dir. An empty Dir turns it off.
type Cache struct {
	Dir string
	// Now is the clock. Nil uses time.Now.
	Now func() time.Time
}

// Allowed reports whether pane was allowed on checkout by a check that
// began less than TTL ago and after the last Drop. An empty pane is
// never cached.
func (c Cache) Allowed(checkout, pane string) bool {
	if c.Dir == "" || pane == "" {
		return false
	}
	dir := c.checkoutDir(checkout)
	checked, ok := readStamp(filepath.Join(dir, entryName(pane)))
	if !ok {
		return false
	}
	if dropped, ok := readStamp(filepath.Join(dir, droppedName)); ok && !checked.After(dropped) {
		return false
	}
	age := c.now().Sub(checked)
	return age >= 0 && age < TTL
}

// Put records that pane holds checkout, by a check that began at
// checked.
func (c Cache) Put(checkout, pane string, checked time.Time) error {
	if c.Dir == "" || pane == "" {
		return nil
	}
	return writeStamp(c.checkoutDir(checkout), entryName(pane), checked)
}

// Drop forgets every pane's answer for checkout.
func (c Cache) Drop(checkout string) error {
	if c.Dir == "" {
		return nil
	}
	return writeStamp(c.checkoutDir(checkout), droppedName, c.now())
}

func (c Cache) now() time.Time {
	if c.Now == nil {
		return time.Now()
	}
	return c.Now()
}

func (c Cache) checkoutDir(checkout string) string {
	return filepath.Join(c.Dir, digest(checkout))
}

func entryName(pane string) string {
	return "pane-" + digest(pane)
}

func digest(s string) string {
	sum := sha256.Sum256([]byte(s))
	return hex.EncodeToString(sum[:16])
}

func readStamp(path string) (time.Time, bool) {
	data, err := os.ReadFile(path)
	if err != nil {
		return time.Time{}, false
	}
	nanos, err := strconv.ParseInt(strings.TrimSpace(string(data)), 10, 64)
	if err != nil {
		return time.Time{}, false
	}
	return time.Unix(0, nanos), true
}

// writeStamp replaces dir/name with t, through a temp file so a reader
// never sees half a stamp.
func writeStamp(dir, name string, t time.Time) error {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("create lease cache: %w", err)
	}
	tmp, err := os.CreateTemp(dir, ".tmp-*")
	if err != nil {
		return fmt.Errorf("write lease cache: %w", err)
	}
	_, werr := tmp.WriteString(strconv.FormatInt(t.UnixNano(), 10) + "\n")
	cerr := tmp.Close()
	if err := errors.Join(werr, cerr); err != nil {
		_ = os.Remove(tmp.Name())
		return fmt.Errorf("write lease cache: %w", err)
	}
	if err := os.Rename(tmp.Name(), filepath.Join(dir, name)); err != nil {
		_ = os.Remove(tmp.Name())
		return fmt.Errorf("write lease cache: %w", err)
	}
	return nil
}
