all: format test lint

format:
	gofumpt -w .

test:
	go test -v ./...

lint:
	golangci-lint run ./...
