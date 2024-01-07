EMACS := emacs

.PHONY: all
all: test

.PHONY: clean
clean:
	@-rm org-epa-gpg*.elc 2>/dev/null
	@-rm org-epa-gpg*.ok 2>/dev/null

%.elc: %.el
	@-rm "$@" 2>/dev/null
	@$(EMACS) --batch --quick \
		--directory . \
		--load compile-setup \
		--eval '(byte-compile-file "$(subst .elc,.el,$@)")' \
	&& test -f "$@"

.PHONY: byte-compile
byte-compile: \
	org-epa-gpg.elc

.PHONY: test
test: lint-makefile byte-compile main-tests

org-epa-gpg-tests.ok: org-epa-gpg.elc org-epa-gpg-tests.elc
	$(EMACS) --batch --quick \
		--directory . \
		--load org-epa-gpg-tests.el \
		--funcall ert-run-tests-batch-and-exit \
	&& touch org-epa-gpg-tests.ok
main-tests: org-epa-gpg-tests.ok

Makefile.ok: Makefile
	@make -n all
	@docker run \
		--network=none \
		--volume "$(PWD)"/Makefile:/Makefile \
		backplane/checkmake /Makefile
lint-makefile: Makefile.ok

.PHONY: tag
tag:
	$(MAKE) all
	git add -f . && git stash
	@grep ";; Version:" org-epa-gpg.el | tee /dev/stderr | grep "$(TAG)"
	@git tag "$(TAG)" --sign
