EMACS ?= emacs

.PHONY: check clean

check: .spacemacs.elc

.spacemacs.elc: .spacemacs
	$(EMACS) -Q --batch -L . -f batch-byte-compile .spacemacs

clean:
	rm -f *.elc test/*.elc
