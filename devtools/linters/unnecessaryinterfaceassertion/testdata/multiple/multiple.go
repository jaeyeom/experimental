// Package multiple is a fixture with an interface that has two implementations.
package multiple

type MultiInterface interface {
	DoLotsOfThings()
}

type MultiType1 struct{}

func (t *MultiType1) DoLotsOfThings() {}

type MultiType2 struct{}

func (t *MultiType2) DoLotsOfThings() {}

var (
	_ MultiInterface = (*MultiType1)(nil)
	_ MultiInterface = (*MultiType2)(nil)
)
