package main

import (
	"embed"
	"io/fs"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

//go:embed skill.md
var skillText string

//go:embed references/*.md
var skillRefFS embed.FS

func decideSkillFiles() []toolcli.SkillFile {
	entries, err := fs.ReadDir(skillRefFS, "references")
	if err != nil {
		panic("read embedded skill references: " + err.Error())
	}
	files := make([]toolcli.SkillFile, 0, len(entries))
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		rel := "references/" + entry.Name()
		data, err := skillRefFS.ReadFile(rel)
		if err != nil {
			panic("read embedded skill reference " + rel + ": " + err.Error())
		}
		files = append(files, toolcli.SkillFile{
			Path:    rel,
			Content: string(data),
		})
	}
	return files
}
