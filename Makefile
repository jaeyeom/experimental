.PHONY: check-format format format-gofumpt format-ruff test lint fix lint-golangci fix-golangci lint-ruff fix-ruff

all: requirements.txt format test fix

check-format:
	gofumpt -l .
	ruff check --fix --exit-zero

format: format-gofumpt format-ruff

format-gofumpt:
	goimports -w .
	gofumpt -w .

format-ruff:
	ruff format

test:
	go test ./...

lint: lint-golangci lint-ruff

fix: fix-golangci fix-ruff

lint-golangci:
	GOPACKAGESDRIVER= golangci-lint run ./...
	oserrorsgodernize ./...

fix-golangci:
	GOPACKAGESDRIVER= golangci-lint run --fix ./...
	oserrorsgodernize --fix ./...

lint-ruff:
	ruff check

fix-ruff:
	ruff check --fix

requirements.txt: requirements.in
	pip install pip-tools
	pip-compile --upgrade --output-file=requirements.txt requirements.in
