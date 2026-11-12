package avatar

import (
	"bytes"
	"errors"
	"io"
	"io/fs"
	"time"
)

// MemFile is a file held in memory, or Err, why it could not be had.
type MemFile struct {
	Data []byte
	Err  string
}

// Overlay is an FS that serves its own names from memory and every
// other name from the local file system.
type Overlay map[string]MemFile

// Stat implements FS.
func (o Overlay) Stat(name string) (fs.FileInfo, error) {
	f, ok := o[name]
	if !ok {
		return osFS{}.Stat(name)
	}
	if f.Err != "" {
		return nil, errors.New(f.Err)
	}
	return memInfo{name: name, size: int64(len(f.Data))}, nil
}

// Open implements FS.
func (o Overlay) Open(name string) (io.ReadCloser, error) {
	f, ok := o[name]
	if !ok {
		return osFS{}.Open(name)
	}
	if f.Err != "" {
		return nil, errors.New(f.Err)
	}
	return io.NopCloser(bytes.NewReader(f.Data)), nil
}

type memInfo struct {
	name string
	size int64
}

func (i memInfo) Name() string       { return i.name }
func (i memInfo) Size() int64        { return i.size }
func (i memInfo) Mode() fs.FileMode  { return 0o444 }
func (i memInfo) ModTime() time.Time { return time.Time{} }
func (i memInfo) IsDir() bool        { return false }
func (i memInfo) Sys() any           { return nil }
