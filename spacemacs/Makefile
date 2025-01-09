EMACS ?= emacs

.PHONY: build clean

build:
	$(EMACS) -Q --batch -L . -f batch-byte-compile .spacemacs

clean:
	rm -f *.elc test/*.elc
