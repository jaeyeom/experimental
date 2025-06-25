.PHONY: check-format format format-gofumpt format-ruff test lint lint-golangci lint-ruff

all: requirements.txt format test lint

check-format:
	gofumpt -l .
	ruff check --fix --exit-zero

format: format-gofumpt

format-gofumpt:
	gofumpt -w .

format-ruff:
	ruff format

test:
	go test ./...

lint: lint-golangci lint-ruff

lint-golangci:
	GOPACKAGESDRIVER= golangci-lint run ./...

lint-ruff:
	ruff check

requirements.txt: requirements.in
	pip install pip-tools
	pip-compile --upgrade --output-file=requirements.txt requirements.in
