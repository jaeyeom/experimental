# fork-keeper

Rebase configured personal forks on upstream and open review pull requests.

## Install

```sh
make install
# or:
go install ./devtools/fork-keeper/cmd/fork-keeper
```

## Usage

```sh
fork-keeper [<fork-name>...]
fork-keeper version [--json]
fork-keeper update [--check] [--ref <ref>] [--json]
fork-keeper doctor [--offline] [--json]
fork-keeper skill [install|uninstall|status ...]
```

## Requirements

- `git`, `jq`, and `gh` on `PATH`
- `review-range.sh` and `validate-verdict.sh` from `jaeyeom/claude-toolbox`

## Environment Variables

- `FORK_KEEPER_CONFIG`: path to `forks.json` (default `~/.config/fork-keeper/forks.json`)
- `FORK_KEEPER_CACHE`: scratch repos and agent output (default `~/.cache/fork-keeper`)
- `FORK_KEEPER_SCRIPTS`: directory holding `review-range.sh` and `validate-verdict.sh` (default `~/.cache/claude-toolbox/plugins/fork-keeper/skills/fork-keeper/scripts`)
- `FORK_KEEPER_DATE`: date used in branch and tag names (default UTC `YYYY-MM-DD`)
