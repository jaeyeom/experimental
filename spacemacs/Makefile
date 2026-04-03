EMACS ?= emacs

PRIVATE_EL := $(wildcard private/local/*.el)
PRIVATE_ELC := $(PRIVATE_EL:.el=.elc)

.PHONY: check clean

check: .spacemacs.elc $(PRIVATE_ELC)

.spacemacs.elc: .spacemacs
	$(EMACS) -Q --batch -L . -f batch-byte-compile .spacemacs

private/local/%.elc: private/local/%.el
	$(EMACS) -Q --batch -L private/local -f batch-byte-compile $<

clean:
	rm -f .spacemacs.elc *.elc test/*.elc private/local/*.elc
