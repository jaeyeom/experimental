// Package coretest is a test helper for the core package.
package coretest

type IDGen []string

func (ig *IDGen) NewID() string {
	first := (*ig)[0]
	*ig = (*ig)[1:]
	return first
}

func NewIDGen(ids ...string) func() string {
	ig := IDGen(ids)
	return ig.NewID
}
