.PHONY: format test lint install generate-pkl

# Phased for make -j: format first, then test/lint/install in parallel.
all:
	$(MAKE) format
	$(MAKE) test lint install

format:
	gofumpt -w .

test:
	go test -v ./...

lint:
	GOPACKAGESDRIVER= golangci-lint run ./...

install:
	go install ./cmd/...

PKL_GEN_FILES := internal/config/pkl/Config.pkl.go

generate-pkl: $(PKL_GEN_FILES)

$(PKL_GEN_FILES): pkl/Config.pkl
	cd pkl && pkl-gen-go \
		--base-path github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl \
		--output-path ../internal/config/pkl/ \
		Config.pkl
