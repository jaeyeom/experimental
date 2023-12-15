#!/usr/bin/env bash

# TODO: Add a way to install `gh` script.
# TODO: Use XDG_* environment variables for directory paths.

# This script is used to set up development environment.

declare -A apt_m=( [ag]=silversearcher-ag [rg]=ripgrep [go]=golang-go [emacs]=emacs [ssh]=openssh [ssh-keygen]=openssh [ssh-add]=openssh [locate]=mlocate )

declare -A pkg_m=( [ag]=silversearcher-ag [rg]=ripgrep [go]=golang [emacs]=emacs [ssh]=dropbear [dropbearkey]=dropbear [locate]=mlocate )

# Install the binary using apt-get command.
get_apt () {
    local pkg_name="$1"
    if [ ${apt_m[$pkg_name]+_} ]; then
        pkg_name=apt_m[$pkg_name]
    fi
    sudo apt-get install -y "$pkg_name"
}

# Install the binary using pkg command for TermUX.
get_pkg () {
    local pkg_name="$1"
    if [ ${pkg_m[$pkg_name]+_} ]; then
        pkg_name=${pkg_m[$pkg_name]}
    fi
    pkg install -y "$pkg_name"
}

# Install the binary if it's not installed yet.
get () {
    command -v "$1" && return
    if command -v "pkg"; then
	      get_pkg "$@"
    elif command -v "apt-get"; then
        get_apt "$@"
    fi
}

# Runs the command
cmd () {
    get "$1"
    command "$@"
}

generate_ssh_key() {
    get ssh
    if command -v "dropbearkey"; then
	      mkdir -p ~/.ssh
	      dropbearkey -t ed25519 -f ~/.ssh/id_dropbear
	      dropbearkey -f ~/.ssh/id_dropbear -y
    else
	      mkdir -p ~/.ssh
	      cmd ssh-keygen -t ed25519 -C "$(git config --global user.email)"
	      eval "$(ssh-agent -s)"
	      cmd ssh-add ~/.ssh/id_ed25519
	      cat ~/.ssh/id_ed25519.pub
    fi
    echo -n "Paste the public key at https://github.com/settings/keys and press enter: "
    read
}

get git
if [ -z "$(git config --global user.name)" ]; then
    echo -n "Your name: "
    read
    cmd git config --global user.name "$REPLY"
fi

if [ -z "$(git config --global user.email)" ]; then
    echo -n "Your email: "
    read
    cmd git config --global user.email "$REPLY"
fi
if [ -d ~/.emacs.d ] && [ ! -d ~/.config/emacs ]; then
    mkdir -p ~/.config
    mv ~/.emacs.d ~/.config/emacs
fi
if [ ! -d ~/.config/emacs ]; then
    cmd git clone https://github.com/syl20bnr/spacemacs ~/.config/emacs
fi
if [ ! -d ~/.config/emacs/private/w3m ]; then
    cmd git clone https://github.com/venmos/w3m-layer.git ~/.config/emacs/private/w3m
fi

if [ ! -f ~/.ssh/id_rsa ] && [ ! -f ~/.ssh/id_ed25519 ] && [ ! -f ~/.ssh/id_dropbear ]; then
    generate_ssh_key
fi

if [ ! -f ~/go/src/github.com/jaeyeom/experimental/spacemacs/.spacemacs ]; then
    echo "NOTE: Go to https://github.com/settings/tokens to generate a personal access token."
    mkdir -p ~/go/src/github.com/jaeyeom
    pushd ~/go/src/github.com/jaeyeom

    cmd gh repo clone jaeyeom/experimental
    cmd gh repo clone jaeyeom/gogo
    cmd gh repo clone jaeyeom/sugo
    cmd gh repo clone jaeyeom/gofiletable
    cmd gh repo clone jaeyeom/gomemocache
    popd
fi

if [ ! -f ~/.spacemacs ]; then
    cp ~/go/src/github.com/jaeyeom/experimental/spacemacs/.spacemacs ~/
fi

# rg currently has an issue with Emacs helm integration
get ag
get emacs
get htop
get man
get w3m
get wget
get curl
get sed
get grep

get locate
if [ command -v "sudo" ]; then
    sudo updatedb
else
    updatedb
fi

command -v golangci-lint || curl -sfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh| sh -s -- -b $(go env GOPATH)/bin v1.55.2
GO111MODULE=on cmd go get golang.org/x/tools/gopls@latest
GO111MODULE=on cmd go get golang.org/x/tools/cmd/godoc@latest
GO111MODULE=on cmd go get golang.org/x/tools/cmd/goimports@latest
GO111MODULE=on cmd go get golang.org/x/tools/cmd/gorename@latest
GO111MODULE=on cmd go get golang.org/x/tools/cmd/guru@latest
GO111MODULE=on cmd go get github.com/cweill/gotests/...@latest
GO111MODULE=on cmd go get github.com/davidrjenni/reftools/cmd/fillstruct@latest
GO111MODULE=on cmd go get github.com/fatih/gomodifytags@latest
GO111MODULE=on cmd go get github.com/godoctor/godoctor@latest
GO111MODULE=on cmd go get github.com/haya14busa/gopkgs/cmd/gopkgs@latest
GO111MODULE=on cmd go get github.com/josharian/impl@latest
GO111MODULE=on cmd go get github.com/rogpeppe/godef@latest

grep 'PATH for go binaries' ~/.profile || cat <<EOF >> ~/.profile

# set PATH for go binaries
if [ -d "\$(go env GOPATH)/bin" ] ; then
    PATH="\$(go env GOPATH)/bin:\$PATH"
fi
EOF

if [ ! -d $HOME/.local/bin ]; then
    mkdir -p $HOME/.local/bin
fi
if [ ! -f $HOME/.local/bin/z.sh ]; then
    cmd curl -o ~/.local/bin/z.sh https://raw.githubusercontent.com/rupa/z/master/z.sh
    chmod +x ~/.local/bin/z.sh
fi

grep 'Enable z script' ~/.bashrc || cat <<EOF >> ~/.bashrc

# Enable z script
if [ -f ~/.local/bin/z.sh ]; then
    . ~/.local/bin/z.sh
fi
EOF

cmd emacs
