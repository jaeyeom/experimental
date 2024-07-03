.PHONY: ag rg go gpg emacs locate openssh git w3m pass curl python python3 pip3 nodejs protoc pandoc mpv ox-clip xclip

ag:
	command -v ag || sudo apt-get install -y silversearcher-ag

rg:
	command -v rg || sudo apt-get install -y ripgrep

go:
	command -v go || sudo apt-get install -y golang-go

gpg:
	command -v gpg || sudo apt-get install -y gnupg

emacs:
	command -v emacs || sudo apt-get install -y emacs

locate:
	command -v locate || sudo apt-get install -y mlocate

openssh:
	command -v ssh || sudo apt-get install -y openssh-client

git:
	command -v git || sudo apt-get install -y git

w3m:
	command -v w3m || sudo apt-get install -y w3m

pass:
	command -v pass || sudo apt-get install -y pass

curl:
	command -v curl || sudo apt-get install -y curl

python:
	command -v python || sudo apt-get install -y python-is-python3

python3:
	command -v python3 || sudo apt-get install -y python3

pip3:
	command -v pip3 || sudo apt-get install -y python3-pip

nodejs:
	command -v node || sudo apt-get install -y nodejs

protoc:
	command -v protoc || sudo apt-get install -y protobuf-compiler

pandoc:
	command -v pandoc || sudo apt-get install -y pandoc

mpv:
	command -v mpv || sudo apt-get install -y mpv

ox-clip: emacs xclip

xclip:
	command -v xclip || sudo apt-get install -y xclip
