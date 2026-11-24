package decide

import (
	"errors"
	"strings"
	"testing"
)

func testProse(n int) string {
	var b strings.Builder
	for b.Len() < n {
		if b.Len() > 0 {
			b.WriteByte(' ')
		}
		b.WriteString("word")
	}
	s := b.String()[:n]
	if strings.HasSuffix(s, " ") {
		s = s[:len(s)-1] + "a"
	}
	return s
}

func defaultShapeLimits() ShapeLimits {
	return ShapeLimits{
		MaxLineLength:      DefaultMaxLineLength,
		MaxFirstLineLength: DefaultMaxFirstLineLength,
		MaxParagraphLines:  DefaultMaxParagraphLines,
		MaxMessageChars:    DefaultMaxMessageChars,
	}
}

func TestCheckShapeLongUnbreakableWords(t *testing.T) {
	limits := defaultShapeLimits()
	url300 := "https://example.com/" + strings.Repeat("a", 280)
	if len(url300) != 300 {
		t.Fatalf("url300 len = %d, want 300", len(url300))
	}
	longPath := "/Users/developer/workspace/github.com/org/project/internal/service/handler/very_long_file_name_for_testing_paths.go"
	if len(longPath) <= DefaultMaxFirstLineLength {
		t.Fatalf("longPath len = %d, want > %d", len(longPath), DefaultMaxFirstLineLength)
	}

	t.Run("300-char URL alone on body line passes", func(t *testing.T) {
		text := "See link\n\n" + url300
		if err := CheckShape(text, limits); err != nil {
			t.Fatalf("CheckShape() = %v, want nil", err)
		}
	})

	t.Run("300-char URL alone on first line passes", func(t *testing.T) {
		if err := CheckShape(url300, limits); err != nil {
			t.Fatalf("CheckShape() = %v, want nil", err)
		}
	})

	t.Run("long file path in first line passes", func(t *testing.T) {
		text := "Review " + longPath + " now"
		if err := CheckShape(text, limits); err != nil {
			t.Fatalf("CheckShape() = %v, want nil", err)
		}
	})

	t.Run("URL plus 170 chars of prose on same line fails with exclusion message", func(t *testing.T) {
		prose170 := testProse(170)
		text := "Verdict\n\n" + url300 + " " + prose170
		err := CheckShape(text, limits)
		if !errors.Is(err, ErrShape) {
			t.Fatalf("CheckShape() = %v, want ErrShape", err)
		}
		want := "line 3 is 171 characters after excluding 1 long word (limit 160)"
		if !strings.Contains(err.Error(), want) {
			t.Errorf("error missing %q:\n%v", want, err)
		}
	})

	t.Run("multiple long words excluded uses plural", func(t *testing.T) {
		prose170 := testProse(170)
		tok := strings.Repeat("h", 64)
		text := "Verdict\n\n" + tok + " " + prose170 + " " + tok
		err := CheckShape(text, limits)
		if !errors.Is(err, ErrShape) {
			t.Fatalf("CheckShape() = %v, want ErrShape", err)
		}
		want := "line 3 is 172 characters after excluding 2 long words (limit 160)"
		if !strings.Contains(err.Error(), want) {
			t.Errorf("error missing %q:\n%v", want, err)
		}
	})

	t.Run("first line with long path plus over-limit prose fails with exclusion message", func(t *testing.T) {
		prose105 := testProse(105)
		text := longPath + " " + prose105
		err := CheckShape(text, limits)
		if !errors.Is(err, ErrShape) {
			t.Fatalf("CheckShape() = %v, want ErrShape", err)
		}
		want := "first line is 106 characters after excluding 1 long word (limit 100)"
		if !strings.Contains(err.Error(), want) {
			t.Errorf("error missing %q:\n%v", want, err)
		}
	})

	t.Run("prose without long words keeps original error format", func(t *testing.T) {
		text := "Verdict\n\n" + testProse(170)
		err := CheckShape(text, limits)
		if !errors.Is(err, ErrShape) {
			t.Fatalf("CheckShape() = %v, want ErrShape", err)
		}
		want := "line 3 is 170 characters (limit 160)"
		if !strings.Contains(err.Error(), want) {
			t.Errorf("error missing %q:\n%v", want, err)
		}
		if strings.Contains(err.Error(), "after excluding") {
			t.Errorf("unexpected exclusion note:\n%v", err)
		}
	})

	t.Run("39-char and 40-char tokens count normally while 41-char token counts as 0", func(t *testing.T) {
		prefix := testProse(70)
		tok39 := strings.Repeat("a", 39)
		tok40 := strings.Repeat("b", 40)
		tok41 := strings.Repeat("c", 41)

		// 70 + 1 + 39 = 110 > 100 -> fails without exclusion.
		err39 := CheckShape(prefix+" "+tok39, limits)
		if !errors.Is(err39, ErrShape) {
			t.Fatalf("39-char token error = %v, want ErrShape", err39)
		}
		if !strings.Contains(err39.Error(), "first line is 110 characters (limit 100)") {
			t.Errorf("39-char token error = %v", err39)
		}

		// 70 + 1 + 40 = 111 > 100 -> fails without exclusion (40 is not longer than 40).
		err40 := CheckShape(prefix+" "+tok40, limits)
		if !errors.Is(err40, ErrShape) {
			t.Fatalf("40-char token error = %v, want ErrShape", err40)
		}
		if !strings.Contains(err40.Error(), "first line is 111 characters (limit 100)") {
			t.Errorf("40-char token error = %v", err40)
		}

		// 70 + 1 + 0 = 71 <= 100 -> passes.
		if err41 := CheckShape(prefix+" "+tok41, limits); err41 != nil {
			t.Fatalf("41-char token error = %v, want nil", err41)
		}
	})

	t.Run("unicode runes measured accurately at threshold", func(t *testing.T) {
		prefix := testProse(70)
		tok40 := strings.Repeat("한", 40)
		tok41 := strings.Repeat("한", 41)

		if err := CheckShape(prefix+" "+tok40, limits); !errors.Is(err, ErrShape) {
			t.Fatalf("40-rune token error = %v, want ErrShape", err)
		}
		if err := CheckShape(prefix+" "+tok41, limits); err != nil {
			t.Fatalf("41-rune token error = %v, want nil", err)
		}
	})

	t.Run("paragraph and total message limits unchanged", func(t *testing.T) {
		// 4-line paragraph still fails even with long tokens.
		para := "Verdict\n\n" + url300 + "\nline 2\nline 3\nline 4"
		if err := CheckShape(para, limits); !errors.Is(err, ErrShape) || !strings.Contains(err.Error(), "paragraph at line 3 is 4 lines (limit 3)") {
			t.Fatalf("paragraph err = %v", err)
		}
		// Total message chars limit still applies.
		huge := "Verdict\n\n" + strings.Repeat(url300+"\n\n", 5)
		if err := CheckShape(huge, limits); !errors.Is(err, ErrShape) || !strings.Contains(err.Error(), "limit 1200") {
			t.Fatalf("total size err = %v", err)
		}
	})
}
