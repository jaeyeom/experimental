.PHONY: check-format format test lint fix lint-golangci fix-golangci lint-ruff fix-ruff

all: requirements.txt format test fix

check-format:
	goimports -l .
	bazel test //tools/format:format_test

format:
	goimports -w .
	bazel run //:format

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
