.PHONY: all
all: generate

.PHONY: generate-ansible
generate-ansible: generate_packages.go install_methods.go packages_data.go templates.go types.go
	go run .

.PHONY: generate
generate: generate-ansible

.PHONY: check-ansible-syntax
check-ansible-syntax:
	@echo "Checking Ansible playbook syntax..."
	@for yml in *.yml; do \
		ansible-playbook --syntax-check "$$yml" >/dev/null 2>&1 || { \
			echo "ERROR: Syntax check failed for $$yml"; \
			ansible-playbook --syntax-check "$$yml" 2>&1 | grep -E "ERROR|error"; \
			exit 1; \
		}; \
	done
	@echo "✓ All playbooks passed syntax check"

.PHONY: check
check: check-ansible-syntax
