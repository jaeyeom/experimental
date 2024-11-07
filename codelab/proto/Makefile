# Makefile using buf to generate Python code from contacts.proto

dep-update:
	buf dep update

generate:
	buf generate --include-imports

build: dep-update generate
	buf build

run-server: build
	PYTHONPATH=./gen python3 -m server

run-client: build
	PYTHONPATH=./gen python3 -m client
