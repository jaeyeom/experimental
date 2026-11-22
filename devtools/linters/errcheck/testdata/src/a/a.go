package a

import (
	"errors"
	"fmt"
	"io"
	"os"
)

func fail() error {
	return errors.New("boom")
}

func check() {
	fail()               // want "unchecked error"
	os.Mkdir("x", 0o755) // want "Error return value of `os.Mkdir` is not checked"

	if err := fail(); err != nil {
		_ = err
	}

	_ = fail()
}

func stdExclusions(c io.Closer, w io.Writer) {
	c.Close()
	fmt.Fprintln(w, "hello")
	os.Remove("nonexistent")
	os.Setenv("FOO", "bar")
	os.Stdout.Write([]byte("hi"))
}
