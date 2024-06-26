default_gopath := $(HOME)/go

# Check if 'go' command exists and set GOPATH accordingly
ifeq (, $(shell command -v go 2>/dev/null))
    GOPATH := $(default_gopath)
else
    GOPATH := $(shell go env GOPATH)
endif

SSH_KEY = $(HOME)/.ssh/id_ed25519
EMACS_CONFIG_DIR = $(HOME)/.config/emacs
PASSWORD_STORE_DIR = $(HOME)/.password-store

.PHONY: all

all:
	@echo Done

# Function to check if a command exists
command_exists = $(shell command -v $(1) >/dev/null 2>&1 && echo yes || echo no)

# Include different Makefiles based on the availability of commands
ifeq ($(call command_exists, pkg),yes)
    include termux.mk
else ifeq ($(call command_exists, apt),yes)
    include debian.mk
endif

.PHONY: gitconfig spacemacs ssh-key-file emacs-w3m


gitconfig: git
	if [ -z "$(shell git config --global user.name)" ]; then \
		echo -n "Your name: " ; \
		read REPLY ; \
		git config --global user.name "$${REPLY}" ; \
	fi

	if [ -z "$(shell git config --global user.email)" ]; then \
		echo -n "Your email: " ; \
		read REPLY ; \
		git config --global user.email "$${REPLY}" ; \
	fi


spacemacs: emacs git $(EMACS_CONFIG_DIR)
$(EMACS_CONFIG_DIR):
	if [ -d ~/.emacs.d ] && [ ! -d ~/.config/emacs ]; then \
		mkdir -p ~/.config ; \
		mv ~/.emacs.d ~/.config/emacs ; \
	fi
	[ -d ~/.config/emacs ] || { \
		git clone https://github.com/syl20bnr/spacemacs ~/.config/emacs ; \
	}


emacs-w3m: spacemacs w3m git
	[ -d ~/.config/emacs/private/w3m ] || { \
		git clone https://github.com/venmos/w3m-layer.git ~/.config/emacs/private/w3m ; \
	}


ssh-key-file: openssh gitconfig $(SSH_KEY)
$(SSH_KEY):
	[ -f $@ ] || { \
		mkdir -p ~/.ssh ; \
		ssh-keygen -t ed25519 -C "$$(git config --global user.email)" -f $@ ; \
		eval "$$(ssh-agent -s)" ; \
		ssh-add ~/.ssh/id_ed25519 ; \
		cat ~/.ssh/id_ed25519.pub ; \
		echo -n "Paste the public key at https://github.com/settings/keys and press enter: " ; \
		read REPLY ; \
		grep -q 'ssh-agent' ~/.bashrc || { \
			echo -n "\neval \$$(ssh-agent -s)\nssh-add ~/.ssh/id_ed25519" >> ~/.bashrc ; \
		} ; \
	}


password-store: gpg pass $(PASSWORD_STORE_DIR)
$(PASSWORD_STORE_DIR):
	gpg --list-keys | grep -q 'uid' || gpg --full-generate-key
	grep -q 'pinentry' ~/.gnupg/gpg-agent.conf || { \
		echo "allow-emacs-pinentry" >> ~/.gnupg/gpg-agent.conf ; \
		echo "allow-loopback-pinentry" >> ~/.gnupg/gpg-agent.conf ; \
	}
	pass init "$$(gpg --list-keys | grep 'uid' | head -n 1 | sed 's/.*<\(.*\)>.*/\1/')"


spacemacs-go: spacemacs go golangci-lint godoc goimports gorename guru gotests fillstruct gomodifytags godoctor gopkgs impl godef


golangci-lint: go curl $(GOPATH)/bin/golangci-lint
$(GOPATH)/bin/golangci-lint:
	command -v golangci-lint || curl -sfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $$(go env GOPATH)/bin v1.59.0


godoc: go $(GOPATH)/bin/godoc
$(GOPATH)/bin/godoc:
	go install golang.org/x/tools/cmd/godoc@latest || \
	go install golang.org/x/tools/cmd/godoc@v0.18.0


goimports: go $(GOPATH)/bin/goimports
$(GOPATH)/bin/goimports:
	go install golang.org/x/tools/cmd/goimports@latest


gorename: go $(GOPATH)/bin/gorename
$(GOPATH)/bin/gorename:
	go install golang.org/x/tools/cmd/gorename@latest


guru: go $(GOPATH)/bin/guru
$(GOPATH)/bin/guru:
	go install golang.org/x/tools/cmd/guru@latest


gotests: go $(GOPATH)/bin/gotests
$(GOPATH)/bin/gotests:
	go install github.com/cweill/gotests/gotests@latest


fillstruct: go $(GOPATH)/bin/fillstruct
$(GOPATH)/bin/fillstruct:
	go install github.com/davidrjenni/reftools/cmd/fillstruct@latest


gomodifytags: go $(GOPATH)/bin/gomodifytags
$(GOPATH)/bin/gomodifytags:
	go install github.com/fatih/gomodifytags@latest


godoctor: go $(GOPATH)/bin/godoctor
$(GOPATH)/bin/godoctor:
	go install github.com/godoctor/godoctor@latest


gopkgs: go $(GOPATH)/bin/gopkgs
$(GOPATH)/bin/gopkgs:
	go install github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest


impl: go $(GOPATH)/bin/impl
$(GOPATH)/bin/impl:
	go install github.com/josharian/impl@latest


godef: go $(GOPATH)/bin/godef
$(GOPATH)/bin/godef:
	go install github.com/rogpeppe/godef@latest


image2ascii: go $(GOPATH)/bin/image2ascii
$(GOPATH)/bin/image2ascii:
	go install github.com/qeesung/image2ascii@latest

protoc-gen-go: protoc go $(GOPATH)/bin/protoc-gen-go
$(GOPATH)/bin/protoc-gen-go:
	go install google.golang.org/protobuf/cmd/protoc-gen-go@latest

protoc-gen-go-grpc: protoc go $(GOPATH)/bin/protoc-gen-go-grpc
$(GOPATH)/bin/protoc-gen-go-grpc:
	go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
