.PHONY: check-format format format-whitespace test lint fix lint-golangci fix-golangci lint-ruff fix-ruff generate-ansible verify-golangci-config check-bazel-go-files coverage coverage-report coverage-html clean-coverage

all: requirements.txt generate-ansible format test fix check-bazel-go-files

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
	bazel test //...

lint: lint-golangci lint-ruff

fix: fix-golangci fix-ruff

lint-golangci: verify-golangci-config
	GOPACKAGESDRIVER= golangci-lint run ./...
	oserrorsgodernize ./...

fix-golangci: verify-golangci-config
	GOPACKAGESDRIVER= golangci-lint run --fix ./...
	oserrorsgodernize --fix ./...

lint-ruff:
	ruff check

fix-ruff:
	ruff check --fix

requirements.txt: requirements.in
	pip install pip-tools
	pip-compile --upgrade --output-file=requirements.txt requirements.in

verify-golangci-config:
	@CURRENT_HASH=$$(shasum -a 256 .golangci.yml | cut -d' ' -f1); \
	if [ ! -f .golangci.yml.hash ] || [ "$$(cat .golangci.yml.hash)" != "$$CURRENT_HASH" ]; then \
		echo "Verifying golangci-lint config..."; \
		golangci-lint config verify && echo "$$CURRENT_HASH" > .golangci.yml.hash; \
	fi

check-bazel-go-files:
	@./check-bazel-go-files.sh

coverage:
	@echo "Generating coverage report..."
	@mkdir -p coverage
	bazel coverage --combined_report=lcov //...
	@OUTPUT_PATH=$$(bazel info output_path); \
	COVERAGE_FILE="$$OUTPUT_PATH/_coverage/_coverage_report.dat"; \
	if [ -f "$$COVERAGE_FILE" ]; then \
		echo "Filtering test files and vendor code..."; \
		lcov --remove "$$COVERAGE_FILE" '*/*_test.go' '*/vendor/*' '*/external/*' -o coverage/lcov.info; \
		echo "Coverage report generated: coverage/lcov.info"; \
	else \
		echo "Error: Coverage file not found at $$COVERAGE_FILE"; \
		exit 1; \
	fi

coverage-report: coverage
	@echo "Coverage summary:"
	@lcov --list coverage/lcov.info

coverage-html: coverage
	@echo "Generating HTML coverage report..."
	@genhtml --branch-coverage --output-directory coverage/html coverage/lcov.info
	@echo "HTML coverage report generated in coverage/html/"
	@echo "Open coverage/html/index.html in your browser to view the report"

clean-coverage:
	@echo "Cleaning coverage files..."
	@rm -rf coverage
	@echo "Coverage files cleaned"
