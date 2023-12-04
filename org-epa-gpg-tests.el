;;; org-epa-gpg-tests.el -- tests for org-epa-gpg

;;; Code:

(require 'ert)
(require 'org-epa-gpg)

(ert-deftest org-epa-gpg-does-not-break-rendering ()
  (let ((old-func-called nil))
    (advice-add
     'create-image :override
     (lambda (&rest r) (setq-local old-func-called t)))
    (org-epa-gpg-patch-org-up)
    (create-image "dummy")
    (should old-func-called)))

(provide 'org-epa-gpg-tests)

;;; org-epa-gpg-tests.el ends here
