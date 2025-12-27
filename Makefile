# Common targets
.PHONY: all format format-whitespace check-format test lint fix

all: requirements.txt generate-ansible generate-pkl format test fix check-bazel-go-files check-org-lint-tests

format: format-whitespace
	goimports -w .
	bazel run //:format

format-whitespace:
	find . -name "*.md" -o -name "*.org" | xargs $(shell if [ "$$(uname)" = "Darwin" ]; then echo "sed -i ''"; else echo "sed -i"; fi) 's/[[:space:]]*$$//'

check-format:
	goimports -l .
	bazel test //tools/format:format_test

test:
	bazel test //...

lint: lint-golangci lint-ruff lint-shellcheck check-spacemacs

fix: fix-golangci fix-ruff lint-shellcheck check-spacemacs

# Go targets
.PHONY: lint-golangci fix-golangci verify-golangci-config check-bazel-go-files

lint-golangci: verify-golangci-config
	GOPACKAGESDRIVER= golangci-lint run ./...
	oserrorsgodernize ./...

fix-golangci: verify-golangci-config
	GOPACKAGESDRIVER= golangci-lint run --fix ./...
	oserrorsgodernize --fix ./...

verify-golangci-config:
	@CURRENT_HASH=$$(shasum -a 256 .golangci.yml | cut -d' ' -f1); \
	if [ ! -f .golangci.yml.hash ] || [ "$$(cat .golangci.yml.hash)" != "$$CURRENT_HASH" ]; then \
		echo "Verifying golangci-lint config..."; \
		golangci-lint config verify && echo "$$CURRENT_HASH" > .golangci.yml.hash; \
	fi

check-bazel-go-files:
	@./check-bazel-go-files.sh

# Python targets
.PHONY: lint-ruff fix-ruff

lint-ruff:
	ruff check

fix-ruff:
	ruff check --fix

requirements.txt: requirements.in
	pip install pip-tools
	pip-compile --upgrade --output-file=requirements.txt requirements.in

# Shell targets
.PHONY: lint-shellcheck

lint-shellcheck:
	find . -name "*.sh" -not -path "./.git/*" -not -path "./bazel-*" -not -path "./.bazel-*" | xargs shellcheck

# Emacs/Spacemacs targets
.PHONY: check-spacemacs check-org-lint-tests

check-spacemacs:
	$(MAKE) -C spacemacs check

check-org-lint-tests:
	./tools/org-lint/check-org-lint-tests.sh

# Code generation targets
.PHONY: generate-ansible generate-pkl

generate-ansible:
	$(MAKE) -C devtools/setup-dev/ansible

generate-pkl:
	$(MAKE) -C devtools/gh-nudge generate-pkl

# Coverage targets
.PHONY: coverage coverage-report coverage-html clean-coverage

coverage:
	@echo "Generating coverage report..."
	@mkdir -p coverage
	bazel coverage --combined_report=lcov //...
	@OUTPUT_PATH=$$(bazel info output_path); \
	COVERAGE_FILE="$$OUTPUT_PATH/_coverage/_coverage_report.dat"; \
	if [ -f "$$COVERAGE_FILE" ]; then \
		echo "Filtering test files and vendor code..."; \
		lcov --ignore-errors unused --remove "$$COVERAGE_FILE" \
			'*_test.go' \
			'*/vendor/*' \
			'*/external/*' \
			'*/testdata/*' \
			-o lcov.info; \
		echo "Coverage report generated: lcov.info"; \
	else \
		echo "Error: Coverage file not found at $$COVERAGE_FILE"; \
		exit 1; \
	fi

coverage-report: coverage
	@echo "Coverage summary:"
	@lcov --list lcov.info

coverage-html: coverage
	@echo "Generating HTML coverage report..."
	@genhtml --branch-coverage --output-directory coverage/html lcov.info
	@echo "HTML coverage report generated in coverage/html/"
	@echo "Open coverage/html/index.html in your browser to view the report"

clean-coverage:
	@echo "Cleaning coverage files..."
	@rm -rf lcov.info coverage
	@echo "Coverage files cleaned"
