#!/usr/bin/env bash

# This script is used to set up development environment.

declare -A apt_m=( [ag]=silversearcher-ag [go]=golang-go [emacs]=emacs-nox [ssh]=openssh [ssh-keygen]=openssh [ssh-add]=openssh )

# Install the binary using apt command.
get_apt () {
    local pkg_name="$1"
    if [ ${apt_m[$pkg_name]+_} ]; then
        pkg_name=apt_m[$pkg_name]
    fi
    sudo apt install -y "$pkg_name"
}

# Install the binary if it's not installed yet.
get () {
    command -v "$1" || get_apt "$@"
}

# Runs the command
cmd () {
    get "$1"
    command "$@"
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

if [ ! -d ~/.emacs.d ]; then
    cmd git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi
if [ ! -d ~/.emacs.d/private/w3m ]; then
    cmd git clone https://github.com/venmos/w3m-layer.git ~/.emacs.d/private/w3m
fi

if [ ! -f ~/.ssh/id_rsa.pub && ! -f ~/.ssh/id_ed25519.pub ]; then
    get ssh
    cmd ssh-keygen -t rsa -b 4096 -C "$(git config --global user.email)"
    eval "$(ssh-agent -s)"
    cmd ssh-add ~/.ssh/id_rsa
    cat ~/.ssh/id_rsa.pub
    echo -n "Paste the public key at https://github.com/settings/keys and press enter: "
    read
fi

if [ ! -f ~/go/src/github.com/jaeyeom/experimental/spacemacs/.spacemacs ]; then
    mkdir -p ~/go/src/github.com/jaeyeom
    pushd ~/go/src/github.com/jaeyeom

    cmd hub clone jaeyeom/experimental
    cmd hub clone jaeyeom/gogo
    cmd hub clone jaeyeom/sugo
    cmd hub clone jaeyeom/gofiletable
    cmd hub clone jaeyeom/gomemocache
    popd
fi

if [ ! -f ~/.spacemacs ]; then
    cp ~/go/src/github.com/jaeyeom/experimental/spacemacs/.spacemacs ~/
fi

get ag
get emacs
get htop
get man
get w3m
get ruby
get wget
get curl
get sed
get grep
get mosh
get clang
get clang-format

cmd go get -u -v github.com/nsf/gocode
cmd go get -u -v github.com/rogpeppe/godef
cmd go get -u -v golang.org/x/tools/cmd/guru
cmd go get -u -v golang.org/x/tools/cmd/gorename
cmd go get -u -v golang.org/x/tools/cmd/goimports

command -v golangci-lint || curl -sfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh| sh -s -- -b $(go env GOPATH)/bin v1.21.0

grep 'PATH for go binaries' ~/.profile || cat <<EOF >> ~/.profile

# set PATH for go binaries
if [ -d "\$(go env GOPATH)/bin" ] ; then
    PATH="\$(go env GOPATH)/bin:\$PATH"
fi
EOF

if [ ! -d $HOME/bin ]; then
    mkdir -p $HOME/bin
fi
if [ ! -f $HOME/bin/z.sh ]; then
    cmd curl -o ~/bin/z.sh https://raw.githubusercontent.com/rupa/z/master/z.sh
fi

grep 'Enable z script' ~/.bashrc || cat <<EOF >> ~/.bashrc

# Enable z script
if [ -f ~/bin/z.sh ]; then
    . ~/bin/z.sh
fi
EOF

cmd emacs
