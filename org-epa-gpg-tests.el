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

(ert-deftest org-epa-gpg-unpatch-silently ()
  (let (patched purged)
    (with-temp-buffer
      (fundamental-mode)
      (advice-add 'org-epa-gpg--patch-org-up
                  :override
                  (lambda (&rest) (setq patched t)))
      (advice-add 'org-epa-gpg-purge
                  :override
                  (lambda (&rest) (setq purged t)))
      (org-epa-gpg--patch-org)
      (advice-remove 'org-epa-gpg-purge
                     (lambda (&rest) (setq purged t)))
      (advice-remove 'org-epa-gpg--patch-org-up
                     (lambda (&rest) (setq patched t)))
      (should-not patched)
      (should purged))))

(ert-deftest org-epa-gpg-local-advices-other-buffer ()
  (let (stored exts-patched image-patched inline-patched)
    (with-temp-buffer
      (push (current-buffer) stored)
      ;; exts
      (advice-add #'org-epa-gpg--dedup-exts
                  :override
                  (lambda (&rest _) (setq exts-patched t)))

      ;; image
      (advice-add #'org-epa-gpg--get-orig-ext :override (lambda (&rest _)))
      (advice-add #'org-epa-gpg-p :override (lambda (&rest _) t))
      (advice-add #'org-epa-gpg--mktmp :override (lambda (&rest _)))
      (advice-add #'org-epa-gpg--log-file :override (lambda (&rest _)))
      (advice-add #'epa-decrypt-file
                  :override
                  (lambda (&rest _) (setq image-patched t)))

      ;; inline
      (advice-add #'run-hooks
                  :around
                  (lambda (old-func hook &rest args)
                    (if (eq 'org-epa-gpg-purge-hook hook)
                        (setq inline-patched t)
                      (apply old-func hook args))))

      ;; set up local advices
      (org-epa-gpg--patch-org-up)

      ;; within same buffer, should work
      ;; exts
      (org-epa-gpg--patch-image-exts (lambda ()))
      (should exts-patched)
      (setq exts-patched nil)

      ;; image
      (org-epa-gpg--patch (lambda (&rest _)) "file")
      (should image-patched)
      (setq image-patched nil)

      ;; inline
      (org-remove-inline-images)
      (should inline-patched)
      (setq inline-patched nil)

      ;; different buffer, should not work
      (with-temp-buffer
        ;; exts
        (org-epa-gpg--patch-image-exts (lambda ()))
        (should-not exts-patched)

        ;; image
        (org-epa-gpg--patch (lambda (&rest _)) "file")
        (should-not image-patched)

        ;; inline
        (org-remove-inline-images)
        (should-not inline-patched)

        (should (string= (format "%s" `((org-inline ,@stored)
                                        (create-image ,@stored)
                                        (image-exts ,@stored)))
                         (format "%s" org-epa-gpg--advices))))

      ;; switch back to previous buffer, should work
      ;; exts
      (org-epa-gpg--patch-image-exts (lambda ()))
      (should exts-patched)

      ;; image
      (org-epa-gpg--patch (lambda (&rest _)) "file")
      (should image-patched)

      ;; inline
      (org-remove-inline-images)
      (should inline-patched)

      ;; unset local advices
      (should (string= (format "%s" `((org-inline ,@stored)
                                      (create-image ,@stored)
                                      (image-exts ,@stored)))
                       (format "%s" org-epa-gpg--advices)))
      (org-epa-gpg--patch-org-down)
      (should (string= (format "%s" '((org-inline)
                                      (create-image)
                                      (image-exts)))
                       (format "%s" org-epa-gpg--advices))))

    ;; exts
    (advice-remove #'org-epa-gpg--dedup-exts
                   (lambda (&rest _) (setq exts-patched t)))

    ;; image
    (advice-remove #'org-epa-gpg--get-orig-ext (lambda (&rest _)))
    (advice-remove #'org-epa-gpg-p (lambda (&rest _) t))
    (advice-remove #'org-epa-gpg--mktmp (lambda (&rest _)))
    (advice-remove #'org-epa-gpg--log-file (lambda (&rest _)))
    (advice-remove #'epa-decrypt-file
                   (lambda (&rest _) (setq image-patched t)))

    ;; inline
    (advice-remove #'run-hooks
                   (lambda (old-func hook &rest args)
                     (if (eq 'org-epa-gpg-purge-hook hook)
                         (setq inline-patched t)
                       (apply old-func hook args))))))

(ert-deftest org-epa-gpg-double-patch ()
  (org-epa-gpg--patch-org-up)
  (org-epa-gpg--patch-org-up)
  (org-epa-gpg--patch-org-down)
  (should (string= (format "%s" '((org-inline)
                                  (create-image)
                                  (image-exts)))
                   (format "%s" org-epa-gpg--advices)))
  (should-not
   (advice-member-p #'org-epa-gpg--patch-image-exts
                    #'image-file-name-regexp))
  (should-not
   (advice-member-p #'org-epa-gpg--patch #'create-image))
  (should-not
   (advice-member-p #'org-epa-gpg--inject-purge
                    #'org-remove-inline-images)))

(ert-deftest org-epa-gpg-double-unpatch ()
  (org-epa-gpg--patch-org-up)
  (org-epa-gpg--patch-org-down)
  (org-epa-gpg--patch-org-down)
  (should (string= (format "%s" '((org-inline)
                                  (create-image)
                                  (image-exts)))
                   (format "%s" org-epa-gpg--advices)))
  (should-not
   (advice-member-p #'org-epa-gpg--patch-image-exts
                    #'image-file-name-regexp))
  (should-not
   (advice-member-p #'org-epa-gpg--patch #'create-image))
  (should-not
   (advice-member-p #'org-epa-gpg--inject-purge
                    #'org-remove-inline-images)))

(ert-deftest org-epa-gpg-autoenable ()
  (advice-add 'add-hook
              :around
              (lambda (old-func hook &rest args)
                (unless (eq 'after-init-hook hook)
                  (apply old-func hook args))))
  (advice-add 'remove-hook
              :around
              (lambda (old-func hook &rest args)
                (unless (eq 'after-init-hook hook)
                  (apply old-func hook args))))
  (should (string= (format "%s" nil)
                   (format "%s" org-epa-gpg--advices)))
  (add-hook 'org-mode-hook #'org-epa-gpg-mode)
  (should (string= (format "%s" nil)
                   (format "%s" org-epa-gpg--advices)))
  (should-not (and (symbolp org-epa-gpg-mode)
                   (symbol-value org-epa-gpg-mode)))
  (with-temp-buffer
    (should-not (and (symbolp org-epa-gpg-mode)
                     (symbol-value org-epa-gpg-mode)))
    (org-mode)
    (should (and (symbolp org-epa-gpg-mode)
                 (symbol-value org-epa-gpg-mode)))
    (kill-buffer (current-buffer))
    (should (string= (format "%s" `((org-inline)
                                    (create-image)
                                    (image-exts)))
                     (format "%s" org-epa-gpg--advices))))
  (remove-hook 'org-mode-hook #'org-epa-gpg-mode)
  (advice-remove 'add-hook
                 (lambda (old-func hook &rest args)
                   (unless (eq 'after-init-hook hook)
                     (apply old-func hook args))))
  (advice-remove 'remove-hook
                 (lambda (old-func hook &rest args)
                   (unless (eq 'after-init-hook hook)
                     (apply old-func hook args))))
  (should (string= (format "%s" `((org-inline)
                                  (create-image)
                                  (image-exts)))
                   (format "%s" org-epa-gpg--advices)))

  (should-not
   (advice-member-p #'org-epa-gpg--patch-image-exts
                    #'image-file-name-regexp))
  (should-not
   (advice-member-p #'org-epa-gpg--patch #'create-image))
  (should-not
   (advice-member-p #'org-epa-gpg--inject-purge
                    #'org-remove-inline-images)))

;; pull to single func, find why non-zero exit when used as a func template
(ert-deftest org-epa-gpg-purge-on-after-init-hook ()
  (let* ((path "example.base64.png")
         (enc-path (format "%s.gpg" path))
         (password "password")
         (text (format "[[./%s]]" enc-path))
         patched temp-decrypted)
    (advice-add 'read-passwd
                :override
                (lambda (prompt &optional confirm default) password))
    (advice-add 'message :override (lambda (&rest _)))
    (let ((epg-pinentry-mode 'loopback))
      (epa-encrypt-file path nil))
    (advice-remove 'message (lambda (&rest _)))
    (advice-remove 'read-passwd
                   (lambda (prompt &optional confirm default) password))

    (with-temp-buffer
      (should (eq (point) 1))
      (insert text)
      (should (eq (point) (1+ (length text))))
      (should (eq (point) (point-max)))

      (goto-char (point-min))
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      (org-mode)

      ;; force drawing overlays by
      ;; - skipping org.el (when (display-graphic-p))
      (advice-add 'display-graphic-p :override (lambda (&rest _) t))
      ;; - preventing C calls in src/frame.c check_window_system()
      (advice-add 'clear-image-cache :override (lambda (&rest _)))
      ;; - skipping org.el (when image)
      (advice-add 'org--create-inline-image :override (lambda (&rest _) t))
      ;; - preventing C calls in src/frame.c check_window_system()
      (advice-add 'image-flush :override (lambda (&rest _)))

      ;; no overlay present yet
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      ;; draw overlay, but ignore on paths
      (org-display-inline-images nil nil)

      ;; overlay still not present, missing exts
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      ;; enable handling encrypted images
      (org-epa-gpg-mode 1)

      ;; check if proper funcs were called
      (advice-add 'image-file-name-regexp
                  :after
                  (lambda (&rest _) (setq patched t)))
      (should-not patched)

      ;; noise off
      (advice-add 'message
                  :around
                  (lambda (old-func fmt &rest args)
                    (unless (or (member enc-path args)
                                (string-prefix-p "%s..." fmt))
                      (apply old-func fmt args))))
      (advice-add 'org-epa-gpg--log-file :override (lambda (&rest _)))

      ;; draw overlay with patched paths
      (org-display-inline-images nil nil)
      (advice-remove 'org-epa-gpg--log-file (lambda (&rest _)))
      (advice-remove 'message
                     (lambda (old-func fmt &rest args)
                       (unless (or (member enc-path args)
                                   (string-prefix-p "%s..." fmt))
                         (apply old-func fmt args))))
      (should patched)
      (advice-remove 'image-file-name-regexp
                     (lambda (&rest _) (setq patched t)))
      (delete-file enc-path)

      ;; overlay present
      (should (get-char-property (point-min) 'org-image-overlay))
      (should (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 1 (length temp-decrypted)))
      (should (file-exists-p (car temp-decrypted)))
      (run-hooks 'after-init-hook)
      (should-not (file-exists-p (car temp-decrypted))))

    (advice-remove 'display-graphic-p (lambda (&rest _) t))
    (advice-remove 'clear-image-cache (lambda (&rest _)))
    (advice-remove 'org--create-inline-image (lambda (&rest _) t))
    (advice-remove 'image-flush (lambda (&rest _)))
    (should (string= (format "%s" `((org-inline)
                                    (create-image)
                                    (image-exts)))
                     (format "%s" org-epa-gpg--advices)))))

(ert-deftest org-epa-gpg-purge-on-after-save-hook ()
  (let* ((path "example.base64.png")
         (enc-path (format "%s.gpg" path))
         (password "password")
         (text (format "[[./%s]]" enc-path))
         patched temp-decrypted)
    (advice-add 'read-passwd
                :override
                (lambda (prompt &optional confirm default) password))
    (advice-add 'message :override (lambda (&rest _)))
    (let ((epg-pinentry-mode 'loopback))
      (epa-encrypt-file path nil))
    (advice-remove 'message (lambda (&rest _)))
    (advice-remove 'read-passwd
                   (lambda (prompt &optional confirm default) password))

    (with-temp-buffer
      (should (eq (point) 1))
      (insert text)
      (should (eq (point) (1+ (length text))))
      (should (eq (point) (point-max)))

      (goto-char (point-min))
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      (org-mode)

      ;; force drawing overlays by
      ;; - skipping org.el (when (display-graphic-p))
      (advice-add 'display-graphic-p :override (lambda (&rest _) t))
      ;; - preventing C calls in src/frame.c check_window_system()
      (advice-add 'clear-image-cache :override (lambda (&rest _)))
      ;; - skipping org.el (when image)
      (advice-add 'org--create-inline-image :override (lambda (&rest _) t))
      ;; - preventing C calls in src/frame.c check_window_system()
      (advice-add 'image-flush :override (lambda (&rest _)))

      ;; no overlay present yet
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      ;; draw overlay, but ignore on paths
      (org-display-inline-images nil nil)

      ;; overlay still not present, missing exts
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      ;; enable handling encrypted images
      (org-epa-gpg-mode 1)

      ;; check if proper funcs were called
      (advice-add 'image-file-name-regexp
                  :after
                  (lambda (&rest _) (setq patched t)))
      (should-not patched)

      ;; noise off
      (advice-add 'message
                  :around
                  (lambda (old-func fmt &rest args)
                    (unless (or (member enc-path args)
                                (string-prefix-p "%s..." fmt))
                      (apply old-func fmt args))))
      (advice-add 'org-epa-gpg--log-file :override (lambda (&rest _)))

      ;; draw overlay with patched paths
      (org-display-inline-images nil nil)
      (advice-remove 'org-epa-gpg--log-file (lambda (&rest _)))
      (advice-remove 'message
                     (lambda (old-func fmt &rest args)
                       (unless (or (member enc-path args)
                                   (string-prefix-p "%s..." fmt))
                         (apply old-func fmt args))))
      (should patched)
      (advice-remove 'image-file-name-regexp
                     (lambda (&rest _) (setq patched t)))
      (delete-file enc-path)

      ;; overlay present
      (should (get-char-property (point-min) 'org-image-overlay))
      (should (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 1 (length temp-decrypted)))
      (should (file-exists-p (car temp-decrypted)))
      (run-hooks 'after-save-hook)
      (should-not (file-exists-p (car temp-decrypted))))

    (advice-remove 'display-graphic-p (lambda (&rest _) t))
    (advice-remove 'clear-image-cache (lambda (&rest _)))
    (advice-remove 'org--create-inline-image (lambda (&rest _) t))
    (advice-remove 'image-flush (lambda (&rest _)))
    (should (string= (format "%s" `((org-inline)
                                    (create-image)
                                    (image-exts)))
                     (format "%s" org-epa-gpg--advices)))))

(ert-deftest org-epa-gpg-purge-on-auto-save-hook ()
  (let* ((path "example.base64.png")
         (enc-path (format "%s.gpg" path))
         (password "password")
         (text (format "[[./%s]]" enc-path))
         patched temp-decrypted)
    (advice-add 'read-passwd
                :override
                (lambda (prompt &optional confirm default) password))
    (advice-add 'message :override (lambda (&rest _)))
    (let ((epg-pinentry-mode 'loopback))
      (epa-encrypt-file path nil))
    (advice-remove 'message (lambda (&rest _)))
    (advice-remove 'read-passwd
                   (lambda (prompt &optional confirm default) password))

    (with-temp-buffer
      (should (eq (point) 1))
      (insert text)
      (should (eq (point) (1+ (length text))))
      (should (eq (point) (point-max)))

      (goto-char (point-min))
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      (org-mode)

      ;; force drawing overlays by
      ;; - skipping org.el (when (display-graphic-p))
      (advice-add 'display-graphic-p :override (lambda (&rest _) t))
      ;; - preventing C calls in src/frame.c check_window_system()
      (advice-add 'clear-image-cache :override (lambda (&rest _)))
      ;; - skipping org.el (when image)
      (advice-add 'org--create-inline-image :override (lambda (&rest _) t))
      ;; - preventing C calls in src/frame.c check_window_system()
      (advice-add 'image-flush :override (lambda (&rest _)))

      ;; no overlay present yet
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      ;; draw overlay, but ignore on paths
      (org-display-inline-images nil nil)

      ;; overlay still not present, missing exts
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      ;; enable handling encrypted images
      (org-epa-gpg-mode 1)

      ;; check if proper funcs were called
      (advice-add 'image-file-name-regexp
                  :after
                  (lambda (&rest _) (setq patched t)))
      (should-not patched)

      ;; noise off
      (advice-add 'message
                  :around
                  (lambda (old-func fmt &rest args)
                    (unless (or (member enc-path args)
                                (string-prefix-p "%s..." fmt))
                      (apply old-func fmt args))))
      (advice-add 'org-epa-gpg--log-file :override (lambda (&rest _)))

      ;; draw overlay with patched paths
      (org-display-inline-images nil nil)
      (advice-remove 'org-epa-gpg--log-file (lambda (&rest _)))
      (advice-remove 'message
                     (lambda (old-func fmt &rest args)
                       (unless (or (member enc-path args)
                                   (string-prefix-p "%s..." fmt))
                         (apply old-func fmt args))))
      (should patched)
      (advice-remove 'image-file-name-regexp
                     (lambda (&rest _) (setq patched t)))
      (delete-file enc-path)

      ;; overlay present
      (should (get-char-property (point-min) 'org-image-overlay))
      (should (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 1 (length temp-decrypted)))
      (should (file-exists-p (car temp-decrypted)))
      (run-hooks 'auto-save-hook)
      (should-not (file-exists-p (car temp-decrypted))))

    (advice-remove 'display-graphic-p (lambda (&rest _) t))
    (advice-remove 'clear-image-cache (lambda (&rest _)))
    (advice-remove 'org--create-inline-image (lambda (&rest _) t))
    (advice-remove 'image-flush (lambda (&rest _)))
    (should (string= (format "%s" `((org-inline)
                                    (create-image)
                                    (image-exts)))
                     (format "%s" org-epa-gpg--advices)))))

(ert-deftest org-epa-gpg-purge-on-desktop-save-hook ()
  (let* ((path "example.base64.png")
         (enc-path (format "%s.gpg" path))
         (password "password")
         (text (format "[[./%s]]" enc-path))
         patched temp-decrypted)
    (advice-add 'read-passwd
                :override
                (lambda (prompt &optional confirm default) password))
    (advice-add 'message :override (lambda (&rest _)))
    (let ((epg-pinentry-mode 'loopback))
      (epa-encrypt-file path nil))
    (advice-remove 'message (lambda (&rest _)))
    (advice-remove 'read-passwd
                   (lambda (prompt &optional confirm default) password))

    (with-temp-buffer
      (should (eq (point) 1))
      (insert text)
      (should (eq (point) (1+ (length text))))
      (should (eq (point) (point-max)))

      (goto-char (point-min))
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      (org-mode)

      ;; force drawing overlays by
      ;; - skipping org.el (when (display-graphic-p))
      (advice-add 'display-graphic-p :override (lambda (&rest _) t))
      ;; - preventing C calls in src/frame.c check_window_system()
      (advice-add 'clear-image-cache :override (lambda (&rest _)))
      ;; - skipping org.el (when image)
      (advice-add 'org--create-inline-image :override (lambda (&rest _) t))
      ;; - preventing C calls in src/frame.c check_window_system()
      (advice-add 'image-flush :override (lambda (&rest _)))

      ;; no overlay present yet
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      ;; draw overlay, but ignore on paths
      (org-display-inline-images nil nil)

      ;; overlay still not present, missing exts
      (should-not (get-char-property (point-min) 'org-image-overlay))
      (should-not (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 0 (length temp-decrypted)))

      ;; enable handling encrypted images
      (org-epa-gpg-mode 1)

      ;; check if proper funcs were called
      (advice-add 'image-file-name-regexp
                  :after
                  (lambda (&rest _) (setq patched t)))
      (should-not patched)

      ;; noise off
      (advice-add 'message
                  :around
                  (lambda (old-func fmt &rest args)
                    (unless (or (member enc-path args)
                                (string-prefix-p "%s..." fmt))
                      (apply old-func fmt args))))
      (advice-add 'org-epa-gpg--log-file :override (lambda (&rest _)))

      ;; draw overlay with patched paths
      (org-display-inline-images nil nil)
      (advice-remove 'org-epa-gpg--log-file (lambda (&rest _)))
      (advice-remove 'message
                     (lambda (old-func fmt &rest args)
                       (unless (or (member enc-path args)
                                   (string-prefix-p "%s..." fmt))
                         (apply old-func fmt args))))
      (should patched)
      (advice-remove 'image-file-name-regexp
                     (lambda (&rest _) (setq patched t)))
      (delete-file enc-path)

      ;; overlay present
      (should (get-char-property (point-min) 'org-image-overlay))
      (should (get-char-property (1- (point-max)) 'org-image-overlay))
      (setq temp-decrypted
            (file-expand-wildcards (concat (temporary-file-directory)
                                           org-epa-gpg-temp-prefix "*")))
      (should (eq 1 (length temp-decrypted)))
      (should (file-exists-p (car temp-decrypted)))
      (run-hooks 'desktop-save-hook)
      (should-not (file-exists-p (car temp-decrypted))))

    (advice-remove 'display-graphic-p (lambda (&rest _) t))
    (advice-remove 'clear-image-cache (lambda (&rest _)))
    (advice-remove 'org--create-inline-image (lambda (&rest _) t))
    (advice-remove 'image-flush (lambda (&rest _)))
    (should (string= (format "%s" `((org-inline)
                                    (create-image)
                                    (image-exts)))
                     (format "%s" org-epa-gpg--advices)))))

(provide 'org-epa-gpg-tests)

;;; org-epa-gpg-tests.el ends here
