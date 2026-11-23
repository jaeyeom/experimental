package migrationimmutability

import (
	"reflect"
	"testing"
)

func TestDefaultPatterns(t *testing.T) {
	tests := map[string]string{
		"app/migrations/0001_initial.py":                  "django",
		"migrations/0012_x.py":                            "django",
		"app/migrations/__init__.py":                      "",
		"app/migrations/helpers.py":                       "",
		"svc/migrations/versions/abc_init.py":             "alembic",
		"svc/migrations/env.py":                           "",
		"db/migrations/001_schema.sql":                    "sql",
		"db/migrations/nested/001.sql":                    "",
		"db/migration/001.sql":                            "",
		"web/prisma/migrations/2024_init/migration.sql":   "prisma",
		"web/prisma/migrations/migration_lock.toml":       "",
		"app/migrations/BUILD.bazel":                      "",
		"notmigrations/0001_x.py":                         "",
		"app/migrations/0001_initial.pyc":                 "",
		"svc/migrations/versions/__pycache__/abc.cpython": "",
	}
	pats := DefaultPatterns()
	for path, want := range tests {
		if got := matchPattern(pats, path); got != want {
			t.Errorf("matchPattern(%q) = %q, want %q", path, got, want)
		}
	}
}

func TestParsePattern(t *testing.T) {
	p, err := ParsePattern(`flyway=(^|/)db/migration/V[^/]+\.sql$`)
	if err != nil {
		t.Fatal(err)
	}
	if p.Name != "flyway" || !p.Re.MatchString("db/migration/V1__init.sql") {
		t.Errorf("ParsePattern = %+v", p)
	}
	for _, bad := range []string{"noequals", "=x", "name=", "name=("} {
		if _, err := ParsePattern(bad); err == nil {
			t.Errorf("ParsePattern(%q) succeeded", bad)
		}
	}
}

func TestSelectPatterns(t *testing.T) {
	extra, err := ParsePattern(`flyway=V\d+\.sql$`)
	if err != nil {
		t.Fatal(err)
	}
	got, err := SelectPatterns([]Pattern{extra}, []string{"django", "prisma"})
	if err != nil {
		t.Fatal(err)
	}
	var names []string
	for _, p := range got {
		names = append(names, p.Name)
	}
	if want := []string{"alembic", "sql", "flyway"}; !reflect.DeepEqual(names, want) {
		t.Errorf("names = %v, want %v", names, want)
	}
	if _, err := SelectPatterns(nil, []string{"nope"}); err == nil {
		t.Error("unknown disable name accepted")
	}
	if _, err := SelectPatterns([]Pattern{{Name: "sql", Re: extra.Re}}, nil); err == nil {
		t.Error("duplicate pattern name accepted")
	}
}
