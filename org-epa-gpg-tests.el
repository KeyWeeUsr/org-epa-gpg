;;; org-epa-gpg-tests.el -- tests for org-epa-gpg

;;; Code:

(require 'ert)
(require 'org-epa-gpg)

(ert-deftest org-epa-gpg-does-not-break-rendering ()
  (let ((old-func-called nil))
    (advice-add
     'create-image :override
     (lambda (&rest r) (setq-local old-func-called t)))
    (org-epa-gpg--patch-org-up)
    (create-image "dummy")
    (should old-func-called)
    (org-epa-gpg--patch-org-down)))

(ert-deftest org-epa-gpg-encrypt-decrypt ()
  (let* ((epg-pinentry-mode 'loopback)
         (path "example.base64.png")
         (enc-path (format "%s.gpg" path))
         (new-path (format "%s.new" path))
         (password "password"))
    (advice-add 'read-passwd
                :override
                (lambda (prompt &optional confirm default) password))
    (advice-add 'message :override (lambda (&rest _)))
    (epa-encrypt-file path nil)
    (epa-decrypt-file enc-path new-path)
    (advice-remove 'message (lambda (&rest _)))
    (advice-remove 'read-passwd
                   (lambda (prompt &optional confirm default) password))
    (should (string= (with-temp-buffer
                       (insert-file-contents path) (buffer-string))
                     (with-temp-buffer
                       (insert-file-contents new-path) (buffer-string))))
    (delete-file enc-path)
    (delete-file new-path)))

(ert-deftest org-epa-gpg-unpatch-on-nonorg ()
  (let (patched unpatched)
    (with-temp-buffer
      (fundamental-mode)
      (advice-add 'org-epa-gpg--patch-org-up
                  :override
                  (lambda (&rest) (setq patched t)))
      (advice-add 'org-epa-gpg--patch-org-down
                  :override
                  (lambda (&rest) (setq unpatched t)))
      (org-epa-gpg--patch-org)
      (advice-remove 'org-epa-gpg--patch-org-up
                     (lambda (&rest) (setq patched t)))
      (advice-remove 'org-epa-gpg--patch-org-down
                     (lambda (&rest) (setq unpatched t)))
      (should-not patched)
      (should unpatched))))

(provide 'org-epa-gpg-tests)

;;; org-epa-gpg-tests.el ends here
