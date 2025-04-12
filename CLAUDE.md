# Build/Lint/Test Commands

- **Build/Format**: `make format` (runs `gofumpt -w .` and `ruff format`)
- **Test**: `make test` (runs `go test ./...`)
- **Test specific Go file**: `go test ./path/to/package -run TestName`
- **Test specific TS file**: `npm test -- -g "test description"` (in my-ts-project)
- **Lint**: `make lint` (runs `golangci-lint run ./...` and `ruff check`)
- **TS Lint**: `npm run lint` (in my-ts-project)

# Code Style Guidelines

- **Go**: Use gofumpt for formatting, follow golangci-lint rules (godot, gosec enabled)
- **Python**: Line length 79 chars, 4-space indentation, Google-style docstrings, double quotes
- **TypeScript**: ESLint with TypeScript-eslint recommended rules, strict type checking
- **Imports**: Group standard library, third-party, and local imports
- **Naming**: CamelCase for Go exports, snake_case for Python, camelCase for TypeScript
- **Error Handling**: Check errors explicitly, use appropriate error types
- **Types**: Strong typing everywhere, especially in TypeScript (strict mode enabled)
- **Comments**: Required for exported Go functions, Google-style Python docstrings