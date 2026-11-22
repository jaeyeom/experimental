package a

import (
	"errors"
	"fmt"
	"os"
)

func unwrapped() error {
	_, err := os.Open("nonexistent")
	return err // want "error returned from external package is unwrapped"
}

func wrapped() error {
	_, err := os.Open("nonexistent")
	if err != nil {
		return fmt.Errorf("open: %w", err)
	}
	return nil
}

func newErr() error {
	return errors.New("boom")
}
