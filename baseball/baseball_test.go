package main

import (
	"fmt"
	"math/rand"
	"reflect"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
)

func nonEmpty(v interface{}) bool {
	return reflect.ValueOf(v).Len() > 0
}

func Test_containsInt(t *testing.T) {
	properties := gopter.NewProperties(nil)

	properties.Property("empty contains nothing", prop.ForAll(
		func(v int) bool {
			return !containsInt(nil, v)
		},
		gen.Int(),
	))

	properties.Property("one element list contains the element", prop.ForAll(
		func(v int) bool {
			return containsInt([]int{v}, v)
		},
		gen.Int(),
	))

	properties.Property("any list with the element", prop.ForAll(
		func(vs []int) bool {
			// #nosec: G404
			return containsInt(vs, vs[rand.Intn(len(vs))])
		},
		gen.SliceOf(gen.Int()).SuchThat(nonEmpty),
	))

	type args struct {
		v  int
		vs []int
	}

	properties.Property("any value that doesn't appear", prop.ForAll(
		func(a args) bool {
			return !containsInt(a.vs, a.v)
		},
		gen.IntRange(0, 10).Map(func(n int) args {
			ns, ok := gen.SliceOf(gen.IntRange(0, 10).SuchThat(func(nn int) bool { return n != nn })).Sample()
			if !ok {
				t.Errorf("failed")
			}
			return args{
				v:  n,
				vs: ns.([]int),
			}
		}),
	))

	properties.TestingRun(t)
}

// unique returns true if all elements in v are different.
func unique(v interface{}) bool {
	val := reflect.ValueOf(v)
	size := val.Len()
	for i := 0; i < size-1; i++ {
		for j := i + 1; j < size; j++ {
			iv := val.Index(i).Interface()
			jv := val.Index(j).Interface()
			if reflect.DeepEqual(iv, jv) {
				return false
			}
		}
	}
	return true
}

// Test_unique tests unique function. What? Seriously? Test of test?
func Test_unique(t *testing.T) {
	uniqueStrs := []string{"abc", "", "defg", "x"}
	for i, s := range uniqueStrs {
		if !unique([]rune(s)) {
			t.Errorf("%d. %q is unique but got not unique", i, s)
		}
	}
	dupStrs := []string{"abca", "00", "defgg", "xxx"}
	for i, s := range dupStrs {
		if unique([]rune(s)) {
			t.Errorf("%d. %q is not unique but got unique", i, s)
		}
	}
	if !unique([]string{"aa", "bb"}) {
		t.Errorf("aa != bb")
	}
	if unique([]string{"aa", "aa"}) {
		t.Errorf("aa == aa")
	}
}

func Test_runeIndex(t *testing.T) {
	properties := gopter.NewProperties(nil)

	properties.Property("should not find any rune in empty", prop.ForAll(
		func(r rune) bool {
			return runeIndex(nil, r) == -1
		},
		gen.Rune(),
	))

	properties.Property("should find it", prop.ForAll(
		func(rs []rune) bool {
			for _, r := range rs {
				if rs[runeIndex(rs, r)] != r {
					return false
				}
			}
			return true
		},
		gen.SliceOf(gen.Rune()),
	))
	properties.TestingRun(t)
}

func Test_computeResult(t *testing.T) {
	properties := gopter.NewProperties(nil)

	properties.Property("total count should be # of digits", prop.ForAll(
		func(v []int) bool {
			if len(v)%2 == 1 {
				v = v[1:]
			}
			a, b := v[:len(v)/2], v[len(v)/2:]
			r := computeResult(a, b)
			return r.Strike+r.Ball+r.Out == len(v)/2
		},
		gen.SliceOf(gen.IntRange(0, 9)),
	))

	properties.Property("all strikes for same numbers", prop.ForAll(
		func(v []int) bool {
			r := computeResult(v, v)
			return r.Strike == len(v)
		},
		gen.SliceOf(gen.IntRange(0, 9)),
	))

	for i := 1; i < 10; i++ {
		properties.Property(fmt.Sprintf("all out for different numbers %d digits", i), prop.ForAll(
			func(a []int, b []int) bool {
				r := computeResult(a, b)
				return r.Out == len(a)
			},
			gen.SliceOfN(i, gen.IntRange(0, 9)),
			gen.SliceOfN(i, gen.IntRange(10, 19)),
		))
	}

	properties.Property("shuffle no out", prop.ForAll(
		func(a []int) bool {
			b := append([]int(nil), a...)
			rand.Shuffle(len(b), func(i, j int) { b[i], b[j] = b[j], b[i] })
			r := computeResult(a, b)
			return r.Out == 0
		},
		gen.SliceOf(gen.IntRange(0, 9)),
	))

	properties.TestingRun(t)
}

func TestConfig_validNumRepeats(t *testing.T) {
	properties := gopter.NewProperties(nil)

	properties.Property("unique trials are always valid", prop.ForAll(
		func(charset []rune, maxRepeat int, trial []int) bool {
			c := Config{
				Charset:   charset,
				NumDigits: len(trial),
				MaxRepeat: maxRepeat,
			}
			return c.validNumRepeats(trial)
		},
		gen.SliceOfN(10, gen.Rune()).SuchThat(unique),
		gen.IntRange(0, 3),
		gen.SliceOfN(5, gen.IntRange(0, 9)).SuchThat(unique),
	))

	properties.Property("always valid for big enough numRepeats", prop.ForAll(
		func(charset []rune, trial []int) bool {
			c := Config{
				Charset:   charset,
				NumDigits: len(trial),
				MaxRepeat: len(trial),
			}
			return c.validNumRepeats(trial)
		},
		gen.SliceOfN(10, gen.Rune()).SuchThat(unique),
		gen.SliceOfN(5, gen.IntRange(0, 9)),
	))

	properties.TestingRun(t)
}

func TestConfig_nextOf(t *testing.T) {
	c := Config{
		Charset:   []rune("0123456789"),
		NumDigits: 3,
		MaxRepeat: 1,
	}
	type args struct {
		trial []int
	}
	tests := []struct {
		name     string
		receiver Config
		args     args
		want     []int
	}{
		{"get first", c, args{nil}, []int{0, 0, 0}},
		{"get second", c, args{[]int{0, 0, 0}}, []int{0, 0, 1}},
		{"carry up", c, args{[]int{0, 0, 9}}, []int{0, 1, 0}},
		{"carry double", c, args{[]int{3, 9, 9}}, []int{4, 0, 0}},
		{"end", c, args{[]int{9, 9, 9}}, nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := tt.receiver
			if got := c.nextOf(tt.args.trial); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Config.nextOf() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestConfig_nextValidOf(t *testing.T) {
	c := Config{
		Charset:   []rune("0123456789"),
		NumDigits: 3,
		MaxRepeat: 1,
	}
	c2 := Config{
		Charset:   []rune("0123456789"),
		NumDigits: 3,
		MaxRepeat: 2,
	}
	type args struct {
		trial []int
	}
	tests := []struct {
		name     string
		receiver Config
		args     args
		want     []int
	}{
		{"get first", c, args{nil}, []int{0, 1, 2}},
		{"get second", c, args{[]int{0, 1, 2}}, []int{0, 1, 3}},
		{"carry up", c, args{[]int{0, 1, 9}}, []int{0, 2, 1}},
		{"carry double", c, args{[]int{3, 9, 8}}, []int{4, 0, 1}},
		{"end", c, args{[]int{9, 8, 7}}, nil},

		{"get first", c2, args{nil}, []int{0, 0, 1}},
		{"get second", c2, args{[]int{0, 0, 1}}, []int{0, 0, 2}},
		{"carry up", c2, args{[]int{0, 1, 9}}, []int{0, 2, 0}},
		{"carry double", c2, args{[]int{3, 9, 9}}, []int{4, 0, 0}},
		{"end", c2, args{[]int{9, 9, 8}}, nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := tt.receiver
			if got := c.nextValidOf(tt.args.trial); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Config.nextValidOf() = %v, want %v", got, tt.want)
			}
		})
	}
}

func permutation(n, r int) int {
	result := 1
	for i := 0; i < r; i++ {
		result *= n - i
	}
	return result
}

func TestConfig_ForEach(t *testing.T) {
	properties := gopter.NewProperties(nil)

	properties.Property("should produce unique", prop.ForAll(
		func(numChars, numDigits, maxRepeat int) bool {
			charset, ok := gen.SliceOfN(numChars, gen.Rune()).Sample()
			if !ok {
				t.Errorf("failed to sample %d chars", numChars)
			}
			c := Config{
				Charset:   charset.([]rune),
				NumDigits: numDigits,
				MaxRepeat: maxRepeat,
			}
			var all [][]int
			c.ForEach(func(trial []int) {
				all = append(all, trial)
			})
			return unique(all)
		},
		gen.IntRange(1, 5),
		gen.IntRange(1, 4),
		gen.IntRange(0, 3),
	))

	properties.Property("should count permutation", prop.ForAll(
		func(numChars, numDigits int) bool {
			charset, ok := gen.SliceOfN(numChars, gen.Rune()).SuchThat(unique).Sample()
			if !ok {
				t.Errorf("failed to sample %d chars", numChars)
			}
			c := Config{
				Charset:   charset.([]rune),
				NumDigits: numDigits,
				MaxRepeat: 1,
			}
			var count int
			c.ForEach(func(trial []int) {
				count++
			})
			return count == permutation(numChars, numDigits)
		},
		gen.IntRange(1, 7),
		gen.IntRange(1, 5),
	))

	properties.TestingRun(t)
}

func TestConfig_Convert(t *testing.T) {
	properties := gopter.NewProperties(nil)

	for n := 0; n < 10; n++ {
		properties.Property(fmt.Sprintf("identity for length %d", n), prop.ForAll(
			func(charset []rune, ints []int) bool {
				c := Config{Charset: charset}
				actual, _ := c.StringToInts(c.IntsToString(ints))
				return fmt.Sprint(actual) == fmt.Sprint(ints)
			},
			gen.SliceOfN(n, gen.Rune()).SuchThat(unique),
			gen.SliceOfN(n, gen.IntRange(0, n-1)),
		))
	}

	properties.Property("should find it", prop.ForAll(
		func(rs []rune) bool {
			for _, r := range rs {
				if rs[runeIndex(rs, r)] != r {
					return false
				}
			}
			return true
		},
		gen.SliceOf(gen.Rune()),
	))
	properties.TestingRun(t)
}

func TestConfig_IntsToString(t *testing.T) {
	type fields struct {
		Charset   []rune
		NumDigits int
		MaxRepeat int
	}
	digits := fields{[]rune("0123456789"), 3, 1}
	alphas := fields{[]rune("abcdefghijk"), 3, 1}
	type args struct {
		ints []int
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   string
	}{
		{"digits", digits, args{[]int{0, 4, 7}}, "047"},
		{"digits", digits, args{[]int{9, 2, 3}}, "923"},
		{"digits", digits, args{[]int{3, 8, 2}}, "382"},
		{"alphas", alphas, args{[]int{0, 4, 7}}, "aeh"},
		{"alphas", alphas, args{[]int{10, 2, 7}}, "kch"},
		{"alphas", alphas, args{[]int{5, 4, 3}}, "fed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := Config{
				Charset:   tt.fields.Charset,
				NumDigits: tt.fields.NumDigits,
				MaxRepeat: tt.fields.MaxRepeat,
			}
			if got := c.IntsToString(tt.args.ints); got != tt.want {
				t.Errorf("Config.IntsToString() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestConfig_StringToInts(t *testing.T) {
	type fields struct {
		Charset   []rune
		NumDigits int
		MaxRepeat int
	}
	digits := fields{[]rune("0123456789"), 3, 1}
	alphas := fields{[]rune("abcdefghijk"), 3, 1}
	type args struct {
		s string
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    []int
		wantErr bool
	}{
		{"digits", digits, args{"047"}, []int{0, 4, 7}, false},
		{"digits", digits, args{"923"}, []int{9, 2, 3}, false},

		{"digits", digits, args{"382"}, []int{3, 8, 2}, false},
		{"alphas", alphas, args{"aeh"}, []int{0, 4, 7}, false},
		{"alphas", alphas, args{"kch"}, []int{10, 2, 7}, false},
		{"alphas", alphas, args{"fed"}, []int{5, 4, 3}, false},
		{"alphas error", alphas, args{"fox"}, nil, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := Config{
				Charset:   tt.fields.Charset,
				NumDigits: tt.fields.NumDigits,
				MaxRepeat: tt.fields.MaxRepeat,
			}
			got, err := c.StringToInts(tt.args.s)
			if (err != nil) != tt.wantErr {
				t.Errorf("Config.StringToInts() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Config.StringToInts() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCandidates_AppendTrainings(t *testing.T) {
	conf := Config{
		Charset:   []rune("0123"),
		NumDigits: 2,
		MaxRepeat: 1,
	}
	tests := []struct {
		name             string
		candidates       [][]int
		trainings        []Training
		wantNewCandidate [][]int
	}{{
		name:       "new start",
		candidates: nil,
		trainings: []Training{{
			Trial:  []int{0, 1},
			Result: Result{1, 0, 1},
		}},
		wantNewCandidate: [][]int{{0, 2}, {0, 3}, {2, 1}, {3, 1}},
	}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &Candidates{
				Config:     conf,
				trainings:  nil,
				candidates: tt.candidates,
			}
			if gotNewCandidate := c.AppendTrainings(tt.trainings...); !reflect.DeepEqual(gotNewCandidate, tt.wantNewCandidate) {
				t.Errorf("Candidates.AppendTrainings() = %v, want %v", gotNewCandidate, tt.wantNewCandidate)
			}
		})
	}
}
