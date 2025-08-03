.PHONY: check-format format format-whitespace test lint fix lint-golangci fix-golangci lint-ruff fix-ruff generate-ansible

all: requirements.txt generate-ansible format test fix

generate-ansible:
	$(MAKE) -C devtools/setup-dev/ansible

check-format:
	goimports -l .
	bazel test //tools/format:format_test

format: format-whitespace
	goimports -w .
	bazel run //:format

format-whitespace:
	find . -name "*.md" -o -name "*.org" | xargs $(shell if [ "$$(uname)" = "Darwin" ]; then echo "sed -i ''"; else echo "sed -i"; fi) 's/[[:space:]]*$$//'

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
