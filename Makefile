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

requirements.txt: requirements.in
	pip install pip-tools
	pip-compile --upgrade --output-file=requirements.txt requirements.in
