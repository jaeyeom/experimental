.PHONY: all
all: generate

.PHONY: generate
generate: generate_packages.go install_methods.go packages_data.go templates.go types.go
	go run .
