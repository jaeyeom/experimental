.PHONY: format test lint

all: format test lint

format:
	gofumpt -w .
	ruff format

test:
	go test ./...

lint:
	GOPACKAGESDRIVER= golangci-lint run ./...
	ruff check
