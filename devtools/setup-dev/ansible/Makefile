.PHONY: all
all: generate

.PHONY: generate-ansible
generate-ansible: generate_packages.go install_methods.go packages_data.go templates.go types.go
	go run .

.PHONY: generate
generate: generate-ansible
