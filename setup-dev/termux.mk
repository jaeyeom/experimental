.PHONY: ag rg go gpg emacs locate openssh git w3m pass curl python python3 pip3 nodejs protoc pandoc

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

nodejs:
	command -v node || pkg install -y nodejs

protoc:
	command -v protoc || pkg install -y protobuf

pandoc:
	command -v pandoc || pkg install -y pandoc

MPV_CONFIG = $(HOME)/.config/mpv/mpv.conf
mpv: $(MPV_CONFIG)
	command -v mpv || pkg install -y mpv

$(MPV_CONFIG):
	mkdir -p $(shell dirname $(MPV_CONFIG))
	@echo "vo=tct" > $(MPV_CONFIG)
	@echo "ao=opensles" >> $(MPV_CONFIG)
	@echo "hwdec=auto" >> $(MPV_CONFIG)
	@echo "profile=sw-fast" >> $(MPV_CONFIG)
	@echo "vid=auto" >> $(MPV_CONFIG)
	@echo "really-quiet" >> $(MPV_CONFIG)
