# factory Conventions

These rules apply to `devtools/factory` and supplement the root and `devtools/` `AGENTS.md` files.

- Layout: `cmd/factory` (`main` only) and `internal/<pkg>`. Keep packages under `internal/`.
- Libraries: `github.com/spf13/cobra`, `github.com/pelletier/go-toml/v2`, `modernc.org/sqlite` (pure Go, no cgo), and `github.com/jaeyeom/go-cmdexec`.
- Inject every external command through `go-cmdexec`'s `Executor` and every HTTP call through an injected `*http.Client`.
- Read environment variables only in `internal/config`.
- On an unexpected input shape, return an error that says what was expected and what was found; never fall back to a quiet default.
- Tests must not touch the network or real `herdr`, `gh`, or `git`.
