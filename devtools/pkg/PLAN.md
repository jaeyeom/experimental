# GH Runner Library Improvement Plan

## Status: Phase 1-3 Complete ✓

## Goal

Make the gh runner infrastructure in this repo usable as a backbone for ghx-cli
(located at `/Users/jaehyun/go/src/github.com/roboco-io/ghx-cli/`).

## Current State (After Implementation)

```
devtools/
├── pkg/                         # Public packages (importable) ✓
│   ├── executor/                # Command execution framework ✓
│   │   ├── executor.go          # BasicExecutor, Executor interface
│   │   ├── types.go             # ToolConfig, error types
│   │   ├── result.go            # ExecutionResult
│   │   ├── helpers.go           # Output, OutputJSON, Run, CombinedOutput
│   │   ├── command_builder.go   # CommandBuilder, DirectCommandBuilder, ShellCommandBuilder
│   │   ├── concurrent.go        # ConcurrentExecutor
│   │   ├── mock.go              # MockExecutor for testing
│   │   └── *_test.go            # Comprehensive tests
│   │
│   ├── ghauth/                  # GitHub authentication ✓
│   │   ├── token.go             # TokenSource interface, ChainTokenSource
│   │   ├── ghcli.go             # GHCLITokenSource (gh auth token)
│   │   ├── env.go               # EnvTokenSource, MultiEnvTokenSource
│   │   └── *_test.go            # Tests
│   │
│   └── ghtest/                  # Test utilities ✓
│       ├── mock.go              # GHMockExecutor with gh-specific helpers
│       ├── skip.go              # SkipWithoutGH, RequireGHAuth
│       ├── fixtures.go          # SampleIssue, SamplePR, SampleRepo, etc.
│       └── *_test.go            # Tests
│
├── internal/
│   └── executor/                # Original package (for existing code)
```

## Implementation Phases

### Phase 1: Create pkg/executor (Public Executor Package) ✓
- [x] Create directory structure
- [x] Create executor.go with Executor interface and BasicExecutor
- [x] Create types.go with ToolConfig and error types
- [x] Create result.go with ExecutionResult
- [x] Create helpers.go with Output, OutputJSON, Run, CombinedOutput
- [x] Create command_builder.go with CommandBuilder interface
- [x] Create concurrent.go with ConcurrentExecutor
- [x] Create mock.go with MockExecutor
- [x] Add comprehensive tests (21 tests passing)
- [x] Create BUILD.bazel

### Phase 2: Create pkg/ghauth (Authentication Package) ✓
- [x] Define TokenSource interface
- [x] Implement GHCLITokenSource (gh auth token)
- [x] Implement EnvTokenSource
- [x] Implement MultiEnvTokenSource
- [x] Implement ChainTokenSource
- [x] Add DefaultTokenSource helper
- [x] Add IsGHCLIAvailable helper
- [x] Add tests (12 tests passing)
- [x] Create BUILD.bazel

### Phase 3: Create pkg/ghtest (Test Utilities) ✓
- [x] Create GHMockExecutor with gh-specific helpers
- [x] Add ExpectAuthToken, ExpectGHCommand, ExpectAPICall
- [x] Add SkipWithoutGH, SkipWithoutGHAuth helpers
- [x] Add RequireGH, RequireGHAuth helpers
- [x] Add SkipIfShort helper
- [x] Add sample fixtures (SampleIssue, SamplePR, SampleRepo, etc.)
- [x] Add tests (6 tests passing)
- [x] Create BUILD.bazel

### Phase 4: Migration Strategy (Ongoing)
- [x] Keep internal/executor for existing code (backward compatible)
- [ ] New code should import from pkg/executor
- [ ] Gradually migrate internal code as needed
- [ ] Eventually deprecate internal/executor

Note: We keep both packages separate rather than re-exporting because:
1. Re-export causes duplicate declaration errors
2. Allows gradual migration
3. Internal package has extra features (signal_executor, signal_handler) not in pkg

## Usage Examples

### Using pkg/executor

```go
import "github.com/jaeyeom/experimental/devtools/pkg/executor"

// Create executor
exec := executor.NewBasicExecutor()

// Run command and get output
output, err := executor.Output(ctx, exec, "gh", "repo", "view", "--json", "name")

// Parse JSON output directly
type Repo struct {
    Name string `json:"name"`
}
repo, err := executor.OutputJSON[Repo](ctx, exec, "gh", "repo", "view", "--json", "name")

// Use mock in tests
mock := executor.NewMockExecutor()
mock.ExpectCommand("gh").WillSucceed(`{"name":"test"}`, 0).Build()
```

### Using pkg/ghauth

```go
import "github.com/jaeyeom/experimental/devtools/pkg/ghauth"

// Get token from gh CLI
exec := executor.NewBasicExecutor()
source := ghauth.NewGHCLITokenSource(exec, "github.com")
token, err := source.Token(ctx)

// Chain multiple sources (tries in order)
source := ghauth.NewChainTokenSource(
    ghauth.NewGHCLITokenSource(exec, ""),
    ghauth.NewEnvTokenSource("GH_TOKEN"),
    ghauth.NewEnvTokenSource("GITHUB_TOKEN"),
)
```

### Using pkg/ghtest

```go
import "github.com/jaeyeom/experimental/devtools/pkg/ghtest"

func TestMyGHFeature(t *testing.T) {
    ghtest.SkipWithoutGH(t) // Skip if gh not installed

    mock := ghtest.NewGHMockExecutor()
    mock.ExpectAuthToken("ghp_test_token")
    mock.ExpectGHCommand("repo view").WillReturnJSON(ghtest.SampleRepo("owner", "repo"))

    // Use mock.Executor() with code under test
}
```

## Test Results

All 35 tests pass:
- pkg/executor: 21 tests
- pkg/ghauth: 8 tests
- pkg/ghtest: 6 tests

## Next Steps for ghx-cli Integration

1. Add this repo as a dependency in ghx-cli's go.mod:
   ```
   go get github.com/jaeyeom/experimental/devtools/pkg/executor
   go get github.com/jaeyeom/experimental/devtools/pkg/ghauth
   go get github.com/jaeyeom/experimental/devtools/pkg/ghtest
   ```

2. Use executor for any shell command execution needs

3. Use ghauth for token management (can replace current auth/manager.go)

4. Use ghtest for testing gh CLI interactions
