.PHONY: build test clean run format lint

# Build the binary with Bazel
build:
	bazel build //devtools/repo-sync/cmd/repo-sync:repo-sync

# Run tests with Bazel
test:
	bazel test //devtools/repo-sync/...

# Clean build artifacts
clean:
	bazel clean
	rm -rf bin/

# Run the application
run:
	bazel run //devtools/repo-sync/cmd/repo-sync:repo-sync

# Run the application with init command
init:
	bazel run //devtools/repo-sync/cmd/repo-sync:repo-sync -- init

# Development build with verbose output
dev:
	bazel run //devtools/repo-sync/cmd/repo-sync:repo-sync -- --verbose

# Format code (run from root to format entire project)
format:
	cd ../.. && make format

# Run linter (run from root)
lint:
	cd ../.. && make lint

# Copy binary to local bin directory for convenience
install: build
	mkdir -p bin/
	cp bazel-bin/devtools/repo-sync/cmd/repo-sync/repo-sync_/repo-sync bin/repo-sync

# Help
help:
	bazel run //devtools/repo-sync/cmd/repo-sync:repo-sync -- --help