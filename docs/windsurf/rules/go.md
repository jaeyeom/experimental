---
trigger: glob
globs: *.go
---

1. Use the log/slog package in the standard library to add appropriate log messages with the proper log level.
2. Define an interface where it is used rather than where it is implemented. Do not introduce unused interfaces.
3. When conducting table-driven tests, clarify which case is failing in the test log.
4. Try running `golangci-lint run ./...` and fix those lint errors. After that you may try `golangci-lint run ./... --default all` and take those optional advices.
