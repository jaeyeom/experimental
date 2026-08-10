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

# pkl-go dropped the pkl-gen-go binary in v0.13.0; code generation is now a
# pkl:Command tool. The @pkl-go dependency notation resolves through the
# repo-root PklProject, so the generator version is pinned there.
$(PKL_GEN_FILES): pkl/Config.pkl ../../PklProject.deps.json
	cd pkl && pkl run @pkl-go/gen.pkl \
		--project-dir ../../.. \
		--base-path github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl \
		--output-path ../internal/config/pkl/ \
		Config.pkl
