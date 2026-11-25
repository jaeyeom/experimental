# Common targets
.PHONY: all check check-ci format format-whitespace check-format test test-rbe lint fix check-uv-lock install

# Stable PATH for Bazel tests under --incompatible_strict_action_env.
# Built from host tool locations in a canonical order so caller PATH
# reordering or extra entries do not invalidate Bazel's test cache.
BAZEL_TEST_PATH := $(shell \
	extra=$$(for t in ansible-playbook emacs go lsof node pkl python3; do \
		p=$$(command -v "$$t" 2>/dev/null || true); \
		[ -n "$$p" ] && dirname "$$p"; \
	done | grep -vxE '/usr/local/bin|/usr/bin|/bin|/usr/sbin|/sbin' | LC_ALL=C sort -u | paste -sd: -); \
	if [ -n "$$extra" ]; then \
		printf '%s:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin' "$$extra"; \
	else \
		printf '/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'; \
	fi)
BAZEL_TEST_ENV := --test_env=PATH=$(BAZEL_TEST_PATH)

# Phased targets for make -j safe parallel execution.
# Each $(MAKE) line runs its targets in parallel; lines run sequentially.
all:
	$(MAKE) generate-ansible generate-pkl
	$(MAKE) format
	$(MAKE) fix
	$(MAKE) test check-semgrep check-gitleaks

CHECK_CI_CHECKS := check-semgrep check-gitleaks

check-ci:
	$(MAKE) check-uv-lock
	$(MAKE) $(CHECK_CI_CHECKS)

check:
	$(MAKE) check-uv-lock
	$(MAKE) $(CHECK_CI_CHECKS) test check-spacemacs

format: format-whitespace
	bazel run //:format

format-whitespace:
	bazel run //tools/format/whitespace -- -w

check-format:
	bazel test --test_summary=terse $(BAZEL_TEST_ENV) //tools/format/...

test:
	bazel test --test_summary=terse $(BAZEL_TEST_ENV) $(BAZEL_FLAGS) //...

# Run remote-eligible tests on BuildBuddy Linux RBE workers without leaking
# host PATH (BAZEL_TEST_ENV) into remote cache keys, then run local/no-remote
# targets on the host platform (required for macOS-to-Linux cross-RBE).
test-rbe:
	bazel test --config=rbe --test_summary=terse --build_tag_filters=-local,-no-remote --test_tag_filters=-local,-no-remote $(BAZEL_FLAGS) //...
	bazel test --test_summary=terse $(BAZEL_TEST_ENV) --build_tag_filters=local,no-remote --test_tag_filters=local,no-remote $(BAZEL_FLAGS) //...

lint: check-spacemacs

# Target fix is best-effort autofix for lint issues. If autofix is not
# available, it still runs lint checks.
fix: fix-go fix-ruff check-spacemacs

INSTALL_CMDS := \
	./devtools/agent-pool/cmd/agent-pool \
	./devtools/agent-pool/cmd/agent-poold \
	./devtools/claude-usage/cmd/claude-usage \
	./devtools/devcheck/cmd/devcheck \
	./devtools/docsync/cmd/docsync \
	./devtools/fork-keeper/cmd/fork-keeper \
	./devtools/genfiles/cmd/genfiles \
	./devtools/gh-ask/cmd/gh-ask \
	./devtools/gh-outbox/cmd/gh-outbox \
	./devtools/gh-nudge/cmd/gh-approve \
	./devtools/gh-nudge/cmd/gh-codeowners \
	./devtools/gh-nudge/cmd/gh-merge \
	./devtools/gh-nudge/cmd/gh-nudge \
	./devtools/gh-nudge/cmd/gh-pr-review \
	./devtools/gh-nudge/cmd/gh-slack \
	./devtools/gh-nudge/cmd/gh-storage \
	./devtools/gh-nudge/cmd/gh-throttle \
	./devtools/linters/cmd/agents-md-size \
	./devtools/linters/cmd/migration-immutability \
	./devtools/linters/cmd/org-lint \
	./devtools/linters/cmd/pkgvar \
	./devtools/linters/cmd/unnecessary-interface-assertion-linter \
	./devtools/memstore/cmd/memstore \
	./devtools/prsync/cmd/prsync \
	./devtools/rename-jira-keys/cmd/rename-jira-keys \
	./devtools/reviewpush/cmd/review-and-push-loop \
	./devtools/slackwatch/cmd/slackwatch

VERSION ?= $(shell git describe --tags --abbrev=0 2>/dev/null || echo "dev")
VERSION_LDFLAGS := -ldflags "-X github.com/jaeyeom/experimental/devtools/pkg/version.Version=$(VERSION)"

install:
	GOPACKAGESDRIVER= go install $(VERSION_LDFLAGS) $(INSTALL_CMDS)

.PHONY: tag
tag:
	@if [ -z "$(TAG)" ]; then echo "Usage: make tag TAG=vX.Y.Z"; exit 1; fi
	@echo "$(TAG)" | grep -Eq '^v[0-9]+\.[0-9]+\.[0-9]+$$' || { echo "Error: TAG must match vMAJOR.MINOR.PATCH"; exit 1; }
	git tag -a "$(TAG)" -m "Release $(TAG)"
	@echo "Created tag $(TAG). Push with: git push origin $(TAG)"

# Go targets
.PHONY: fix-go

# pkgvar and oserrors run in nogo (//tools/lint:platform_nogo). Only the
# oserrorsgodernize --fix autofixer (a module tool) remains.
# An empty GOPACKAGESDRIVER keeps Bazel from taking over package loading (#416).
fix-go:
	GOPACKAGESDRIVER= go tool oserrorsgodernize --fix ./...

# Python targets
.PHONY: fix-ruff check-uv-lock

fix-ruff:
	ruff check --fix

check-uv-lock: pyproject.toml uv.lock
	@if ! command -v uv >/dev/null 2>&1; then \
		echo "Error: uv is required to check uv.lock"; \
		echo "Install with:"; \
		echo "  cd devtools/setup-dev/ansible && ./ensure.sh uv"; \
		exit 1; \
	fi
	uv lock --check

# Emacs/Spacemacs targets
.PHONY: check-spacemacs

check-spacemacs:
	$(MAKE) -C spacemacs check

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
	bazel coverage --combined_report=lcov $(BAZEL_TEST_ENV) //...
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

# Manual only: needs Emacs and is never part of check or test.
# Usage: make org-lint-parity [GO_LINT=cmd]; make org-lint-parity-update
.PHONY: org-lint-parity org-lint-parity-update
org-lint-parity org-lint-parity-update:
	@mkdir -p .tmp
	go build -o .tmp/org-lint ./devtools/linters/cmd/org-lint
	go run ./devtools/linters/cmd/org-lint-parity -emacs-lint .tmp/org-lint \
		$(if $(GO_LINT),-go-lint "$(GO_LINT)") \
		$(if $(filter org-lint-parity-update,$(MAKECMDGOALS)),-update)
