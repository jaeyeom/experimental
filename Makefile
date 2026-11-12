# Common targets
.PHONY: all check format format-whitespace check-whitespace check-format test lint fix check-uv-lock install

# Cross-platform sed in-place edit
SED_INPLACE := $(shell if [ "$$(uname)" = "Darwin" ]; then echo "sed -i ''"; else echo "sed -i"; fi)

# Phased targets for make -j safe parallel execution.
# Each $(MAKE) line runs its targets in parallel; lines run sequentially.
all:
	$(MAKE) generate-ansible generate-pkl
	$(MAKE) format
	$(MAKE) fix
	$(MAKE) test check-semgrep check-gitleaks check-bazel-go-files check-org-lint-tests

check:
	$(MAKE) check-uv-lock check-generated
	$(MAKE) check-format test lint check-semgrep check-gitleaks check-bazel-go-files check-org-lint-tests

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
	bazel test --test_summary=terse //tools/format:format_test

test:
	bazel test --test_summary=terse //...

lint: lint-golangci lint-ruff lint-mypy lint-shellcheck check-spacemacs

# Target fix is best-effort autofix for lint issues. If autofix is not
# available, it still runs lint checks.
fix: fix-golangci fix-ruff lint-mypy lint-shellcheck check-spacemacs

INSTALL_CMDS := \
	./devtools/claude-usage/cmd/claude-usage \
	./devtools/devcheck/cmd/devcheck \
	./devtools/docsync/cmd/docsync \
	./devtools/gh-nudge/cmd/gh-approve \
	./devtools/gh-nudge/cmd/gh-codeowners \
	./devtools/gh-nudge/cmd/gh-merge \
	./devtools/gh-nudge/cmd/gh-nudge \
	./devtools/gh-nudge/cmd/gh-pr-review \
	./devtools/gh-nudge/cmd/gh-slack \
	./devtools/gh-nudge/cmd/gh-storage \
	./devtools/linters/cmd/org-lint \
	./devtools/linters/cmd/pkgvar \
	./devtools/linters/cmd/unnecessary-interface-assertion-linter \
	./devtools/prsync/cmd/prsync \
	./devtools/rename-jira-keys/cmd/rename-jira-keys \
	./devtools/reviewpush/cmd/review-and-push-loop

install:
	GOPACKAGESDRIVER= go install $(INSTALL_CMDS)

# Go targets
.PHONY: lint-golangci fix-golangci verify-golangci-config check-bazel-go-files
GOLANGCI_CONFIG_HASH_FILE := .tmp/golangci.yml.hash

# oserrorsgodernize is the module tool github.com/jaeyeom/godernize/oserrors/cmd/oserrorsgodernize.
# pkgvar is a separate vettool: go vet accepts one -vettool per invocation.
# An empty GOPACKAGESDRIVER keeps Bazel from taking over package loading (#416).
lint-golangci: verify-golangci-config
	GOPACKAGESDRIVER= golangci-lint run ./...
	mkdir -p .tmp
	GOPACKAGESDRIVER= go build -o .tmp/pkgvar ./devtools/linters/cmd/pkgvar
	GOPACKAGESDRIVER= go vet -vettool=$(CURDIR)/.tmp/pkgvar ./...
	GOPACKAGESDRIVER= go tool oserrorsgodernize ./...

fix-golangci: verify-golangci-config
	GOPACKAGESDRIVER= golangci-lint run --fix ./...
	GOPACKAGESDRIVER= go tool oserrorsgodernize --fix ./...

verify-golangci-config:
	@CURRENT_HASH=$$(shasum -a 256 .golangci.yml | cut -d' ' -f1); \
	mkdir -p .tmp; \
	if [ ! -f $(GOLANGCI_CONFIG_HASH_FILE) ] || [ "$$(cat $(GOLANGCI_CONFIG_HASH_FILE))" != "$$CURRENT_HASH" ]; then \
		echo "Verifying golangci-lint config..."; \
		golangci-lint config verify && echo "$$CURRENT_HASH" > $(GOLANGCI_CONFIG_HASH_FILE); \
	fi

check-bazel-go-files:
	@./check-bazel-src-files.sh go

# Python targets
.PHONY: lint-ruff fix-ruff lint-mypy check-uv-lock

lint-ruff:
	ruff check

fix-ruff:
	ruff check --fix

lint-mypy:
	mypy

check-uv-lock: pyproject.toml uv.lock
	@if ! command -v uv >/dev/null 2>&1; then \
		echo "Error: uv is required to check uv.lock"; \
		echo "Install with:"; \
		echo "  cd devtools/setup-dev/ansible && ./ensure.sh uv"; \
		exit 1; \
	fi
	uv lock --check

# Shell targets
.PHONY: lint-shellcheck

lint-shellcheck:
	@FILES=$$(git ls-files "*.sh"); \
	if [ -n "$$FILES" ]; then \
		shellcheck -x $$FILES; \
	fi

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

# Fail when a generator adds an untracked file under GENERATED_PATHS or
# changes a pre-existing untracked file there. An unchanged pre-existing
# untracked file is not a failure.
check-generated:
	@mkdir -p .tmp
	@git ls-files --others --exclude-standard -- $(GENERATED_PATHS) | sort | while IFS= read -r f; do \
		[ -n "$$f" ] || continue; \
		printf '%s\t%s\n' "$$(git hash-object "$$f")" "$$f"; \
	done > .tmp/check-generated-before
	@$(MAKE) generate-ansible generate-pkl
	@STATUS=0; \
	if ! git diff --quiet -- $(GENERATED_PATHS); then \
		STALE=$$(git diff --cached --name-only -- $(GENERATED_PATHS)); \
		if [ -n "$$STALE" ]; then \
			echo "Error: Staged generated files are out of date. Regenerated output differs from staged content."; \
			echo "Run 'make all' and re-stage the generated files:"; \
			git diff --name-only -- $(GENERATED_PATHS); \
			STATUS=1; \
		else \
			echo "Warning: Generated files differ from index but none are staged for this commit."; \
			echo "If your commit includes source changes that affect code generation, stage the generated files:"; \
			git diff --name-only -- $(GENERATED_PATHS); \
		fi; \
	fi; \
	git ls-files --others --exclude-standard -- $(GENERATED_PATHS) | sort | while IFS= read -r f; do \
		[ -n "$$f" ] || continue; \
		printf '%s\t%s\n' "$$(git hash-object "$$f")" "$$f"; \
	done > .tmp/check-generated-after; \
	CHANGED=$$(awk -F '\t' 'FILENAME == ARGV[1] { if ($$2 != "") old[$$2] = $$1; next } $$2 == "" { next } { if (!($$2 in old)) { print $$2; bad = 1 } else if (old[$$2] != $$1) { print $$2; bad = 1 } } END { exit bad ? 1 : 0 }' .tmp/check-generated-before .tmp/check-generated-after); \
	if [ $$? -ne 0 ]; then \
		echo "Error: Untracked generated files found. Stage or remove them:"; \
		echo "$$CHANGED"; \
		STATUS=1; \
	fi; \
	exit $$STATUS

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
	@if ! command -v semgrep >/dev/null 2>&1; then \
		echo "Error: semgrep is required for make check"; \
		echo "Install with:"; \
		echo "  cd devtools/setup-dev/ansible && ./ensure.sh semgrep"; \
		exit 1; \
	fi
	@semgrep scan --error --config auto --config .semgrep

# Gitleaks targets
.PHONY: check-gitleaks
check-gitleaks:
	@if ! command -v gitleaks >/dev/null 2>&1; then \
		echo "Error: gitleaks is required for make check"; \
		echo "Install with:"; \
		echo "  cd devtools/setup-dev/ansible && ./ensure.sh gitleaks"; \
		exit 1; \
	fi
	@gitleaks git --redact --no-banner .
