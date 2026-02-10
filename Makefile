# Common targets
.PHONY: all check format format-whitespace check-whitespace check-format test lint fix

# Cross-platform sed in-place edit
SED_INPLACE := $(shell if [ "$$(uname)" = "Darwin" ]; then echo "sed -i ''"; else echo "sed -i"; fi)

# Phased targets for make -j safe parallel execution.
# Each $(MAKE) line runs its targets in parallel; lines run sequentially.
all:
	$(MAKE) requirements.txt generate-ansible generate-pkl
	$(MAKE) format
	$(MAKE) fix
	$(MAKE) test check-semgrep check-bazel-go-files check-org-lint-tests

check:
	$(MAKE) requirements.txt check-generated
	$(MAKE) check-format test lint check-semgrep check-bazel-go-files check-org-lint-tests

format: format-whitespace
	goimports -w .
	bazel run //:format

format-whitespace:
	@for f in $$(rg -l '\s$$' -g '*.md' -g '*.org' 2>/dev/null || true); do \
		$(SED_INPLACE) 's/[[:space:]]*$$//' "$$f"; \
	done

check-whitespace:
	@FILES=$$(rg -l '\s$$' -g '*.md' -g '*.org' 2>/dev/null || true); \
	if [ -n "$$FILES" ]; then echo "Trailing whitespace found in:"; echo "$$FILES"; exit 1; fi

check-format: check-whitespace
	goimports -l .
	bazel test //tools/format:format_test

test:
	bazel test //...

lint: lint-golangci lint-ruff lint-shellcheck check-spacemacs

# Target fix is best-effort autofix for lint issues. If autofix is not
# available, it still runs lint checks.
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
	@./check-bazel-src-files.sh go

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
	shellcheck -x $$(git ls-files "*.sh")

# Emacs/Spacemacs targets
.PHONY: check-spacemacs check-org-lint-tests

check-spacemacs:
	$(MAKE) -C spacemacs check

check-org-lint-tests:
	./tools/org-lint/check-org-lint-tests.sh

# Code generation targets
.PHONY: generate-ansible generate-pkl check-generated

# Paths where generators produce output
GENERATED_PATHS := devtools/setup-dev/ansible devtools/gh-nudge/internal/config/pkl

generate-ansible:
	$(MAKE) -C devtools/setup-dev/ansible

generate-pkl:
	$(MAKE) -C devtools/gh-nudge generate-pkl

check-generated: generate-ansible generate-pkl
	@if ! git diff --quiet -- $(GENERATED_PATHS); then \
		echo "Error: Generated files differ from staged. Run 'make all' and stage the files."; \
		git diff --name-only -- $(GENERATED_PATHS); \
		exit 1; \
	fi

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

# Semgrep targets
.PHONY: check-semgrep
check-semgrep:
	@if command -v semgrep >/dev/null 2>&1; then \
		semgrep scan --error --config auto --config .semgrep; \
	else \
		echo "Skipping semgrep: not installed"; \
	fi
