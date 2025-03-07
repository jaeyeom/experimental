package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"math/rand"
	"reflect"
	"sync"
	"sync/atomic"
)

// containsInt returns true if i is in ints.
func containsInt(ints []int, i int) bool {
	for _, ii := range ints {
		if i == ii {
			return true
		}
	}
	return false
}

func runeIndex(runes []rune, r rune) int {
	for i, rr := range runes {
		if r == rr {
			return i
		}
	}
	return -1
}

func computeResult(a, b []int) Result {
	if len(a) != len(b) {
		fmt.Println(len(a), len(b))
		panic("length mismatch")
	}
	var r Result
	for i := range a {
		if a[i] == b[i] {
			r.Strike++
			continue
		}
		if containsInt(b, a[i]) {
			r.Ball++
			continue
		}
		r.Out++
	}
	return r
}

type Config struct {
	Charset   []rune
	NumDigits int
	// Maximum number of repeated digits. (Default 1)
	MaxRepeat int
}

func (c Config) validNumRepeats(trial []int) bool {
	maxRepeat := c.MaxRepeat
	if maxRepeat == 0 {
		maxRepeat = 1
	}
	count := make([]int, len(c.Charset))
	for _, t := range trial {
		count[t]++
		if count[t] > maxRepeat {
			return false
		}
	}
	return true
}

func (c Config) valid(trial []int) bool {
	if len(trial) != c.NumDigits {
		return false
	}
	for _, t := range trial {
		if t < 0 || t >= len(c.Charset) {
			return false
		}
	}
	return c.validNumRepeats(trial)
}

// nextOf returns the next trial. It returns nil if it's the end.
func (c Config) nextOf(trial []int) []int {
	if trial == nil {
		return make([]int, c.NumDigits)
	}
	trial = append([]int(nil), trial...)
	for i := c.NumDigits - 1; i > -1; i-- {
		trial[i]++
		if trial[i] < len(c.Charset) {
			return trial
		}
		trial[i] = 0
	}
	return nil
}

// nextValidOf returns the next trial. It returns nil if it's the end.
func (c Config) nextValidOf(trial []int) []int {
	if trial == nil {
		trial = make([]int, c.NumDigits)
	} else {
		trial = c.nextOf(trial)
	}
	for !c.valid(trial) {
		if trial == nil {
			return nil
		}
		trial = c.nextOf(trial)
	}
	return trial
}

// ForEach calls cb for each possible trials.
func (c Config) ForEach(cb func(trial []int)) {
	trial := c.nextValidOf(nil)
	for ; trial != nil; trial = c.nextValidOf(trial) {
		cb(trial)
	}
}

// IntsToString converts ints to string.
func (c Config) IntsToString(ints []int) string {
	r := make([]rune, len(ints))
	for i, n := range ints {
		r[i] = c.Charset[n]
	}
	return string(r)
}

// IntsToString converts string to ints.
func (c Config) StringToInts(s string) ([]int, error) {
	var ints []int
	for _, r := range s {
		ri := runeIndex(c.Charset, r)
		if ri == -1 {
			return nil, fmt.Errorf("invalid character %q in %q", r, s)
		}
		ints = append(ints, ri)
	}
	return ints, nil
}

type Result struct {
	Strike int
	Ball   int
	Out    int
}

type Training struct {
	Trial  []int
	Result Result
}

func AllMeets(trainings []Training, trial []int) bool {
	for _, t := range trainings {
		if computeResult(t.Trial, trial) != t.Result {
			return false
		}
	}
	return true
}

type Candidates struct {
	Config     Config
	trainings  []Training
	candidates [][]int
}

func (c *Candidates) regnerate() {
	if c.trainings != nil {
		panic("trainings not nil")
	}
	c.candidates = nil
	c.Config.ForEach(func(trial []int) {
		c.candidates = append(c.candidates, trial)
	})
}

func (c *Candidates) AppendTrainings(trainings ...Training) (newCandidate [][]int) {
	if c.candidates == nil {
		c.regnerate()
	}
	c.trainings = append(c.trainings, trainings...)
	var newCandidates [][]int
	for _, candidate := range c.candidates {
		if AllMeets(trainings, candidate) {
			newCandidates = append(newCandidates, candidate)
		}
	}
	c.candidates = newCandidates
	return c.candidates
}

type RandomPicker struct {
	candidates Candidates
}

func NewRandomPicker(c Config) *RandomPicker {
	return &RandomPicker{
		candidates: Candidates{Config: c},
	}
}

func (p *RandomPicker) Pick(additional ...Training) []int {
	candidates := p.candidates.AppendTrainings(additional...)
	numCandidates := len(candidates)
	n := rand.Intn(numCandidates)
	return candidates[n]
}

type Picker interface {
	Pick(additional ...Training) []int
}

func countTrials(p Picker, answer []int) int32 {
	pick := p.Pick()
	for numTrials := int32(1); ; numTrials++ {
		if reflect.DeepEqual(answer, pick) {
			return numTrials
		}
		next := Training{
			Trial:  pick,
			Result: computeResult(pick, answer),
		}
		pick = p.Pick(next)
	}
}

func AverageTrials(answers [][]int, pickerFactory func() Picker) float64 {
	var total int32
	var wg sync.WaitGroup
	for _, answer := range answers {
		wg.Add(1)
		go func(answer []int) {
			defer wg.Done()
			p := pickerFactory()
			atomic.AddInt32(&total, countTrials(p, answer))
		}(answer)
	}
	wg.Wait()
	return float64(total) / float64(len(answers))
}

func EvaluatePickers(c Config, times int) {
	randomPicker := NewRandomPicker(c)
	answers := make([][]int, times)
	for i := range answers {
		answers[i] = randomPicker.Pick()
	}
	fmt.Printf("RandomPicker average trials: %.5f\n", AverageTrials(answers, func() Picker { return NewRandomPicker(c) }))
}

func Interactive(c Config, picker Picker) {
	pick := picker.Pick()
	for {
		fmt.Printf("Next pick: %s\n", c.IntsToString(pick))
		fmt.Print("Type result s b o: ")
		var s, b, o int
		_, err := fmt.Scanf("%d %d %d\n", &s, &b, &o)
		if errors.Is(err, io.EOF) {
			return
		}
		if err != nil || s+b+o != c.NumDigits {
			fmt.Println("Please type again")
			continue
		}
		if s == c.NumDigits || (b == 0 && o == 0) {
			fmt.Println("Congrats")
			return
		}
		pick = picker.Pick(Training{
			Trial:  pick,
			Result: Result{s, b, o},
		})
	}
}

func main() {
	numDigits := flag.Int("num_digits", 3, "number of digits")
	charset := flag.String("charset", "0123456789", "character set")
	evaluateTimes := flag.Int("evaluate_times", 0, "do evaluation by this time")
	flag.Parse()
	c := Config{
		Charset:   []rune(*charset),
		NumDigits: *numDigits,
		MaxRepeat: 1,
	}
	p := NewRandomPicker(c)
	if *evaluateTimes > 0 {
		EvaluatePickers(c, *evaluateTimes)
	}
	Interactive(c, p)
}
