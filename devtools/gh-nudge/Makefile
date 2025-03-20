.PHONY: format test lint

all: format test lint install

format:
	gofumpt -w .

test:
	go test -v ./...

lint:
	GOPACKAGESDRIVER= golangci-lint run ./...

install:
	go install ./cmd/...
