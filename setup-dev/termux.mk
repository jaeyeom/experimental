.PHONY: ag rg go gpg emacs locate openssh git w3m pass curl python python3 pip3
ag:
	command -v ag || pkg install -y silversearcher-ag

rg:
	command -v rg || pkg install -y ripgrep

go:
	command -v go || pkg install -y golang

gpg:
	command -v gpg || pkg install -y gnupg

emacs:
	command -v emacs || pkg install -y emacs

locate:
	command -v locate || pkg install -y mlocate

openssh:
	command -v ssh || pkg install -y openssh

git:
	command -v git || pkg install -y git

w3m:
	command -v w3m || pkg install -y w3m

pass:
	command -v pass || pkg install -y pass

curl:
	command -v curl || pkg install -y curl

python:
	command -v python || pkg install -y python

python3: python

pip3:
	command -v pip3 || pkg install -y python-pip
