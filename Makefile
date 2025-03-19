.PHONY: format test lint

all: format test lint

format:
	gofumpt -w .

test:
	go test ./...

lint:
	golangci-lint run ./...
