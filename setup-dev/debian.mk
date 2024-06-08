.PHONY: ag rg go gpg emacs locate openssh git w3m pass curl python python3 pip3


ag:
	command -v ag || sudo apt install -y silversearcher-ag

rg:
	command -v rg || sudo apt install -y ripgrep

go:
	command -v go || sudo apt install -y golang-go

gpg:
	command -v gpg || sudo apt install -y gnupg

emacs:
	command -v emacs || sudo apt install -y emacs

locate:
	command -v locate || sudo apt install -y mlocate

openssh:
	command -v ssh || sudo apt install -y openssh-client

git:
	command -v git || sudo apt install -y git

w3m:
	command -v w3m || sudo apt install -y w3m

pass:
	command -v pass || sudo apt install -y pass

curl:
	command -v curl || sudo apt install -y curl

python:
	command -v python || sudo apt install -y python-is-python3

python3:
	command -v python3 || sudo apt install -y python3

pip3:
	command -v pip3 || sudo apt install -y python3-pip

nodejs:
	command -v node || sudo apt install -y nodejs
