EMACS := emacs

all:
	$(EMACS) --batch --quick \
		--directory . \
		--load org-epa-gpg-tests.el \
		--funcall ert-run-tests-batch
