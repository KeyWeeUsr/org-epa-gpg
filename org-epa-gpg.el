;;; org-epa-gpg.el --- Patch to enable EasyPG .gpg images in Org mode inline

;; Copyright (C) 2022 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: lisp, org, gpg, pgp, epa, encryption, image, inline, patch
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/KeyWeeUsr/org-epa-gpg

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a patch that attempts to fix image inlining (C-c C-x C-v) in
;; the org-mode for images that are encrypted AND end with ".gpg" extension.
;;
;; To start, add this mandatory line to your config file:
;; (require 'org-epa-gpg)
;; (org-epa-gpg-enable)
;;
;; The cleaning of the decrypted content (in /tmp via (make-temp-file) )
;; is currently done via patching (org-remove-inline-images) with a custom
;; hook (so that it's purged on (C-c C-x C-v) untoggling and via attaching
;; to multiple standard hooks.
;;
;; What I attempted to make it less insane:
;; * return the paths from functions
;;   -> Lisp/ELisp doesn't return values as a normal programming language,
;;   such as store first, then do stuff, then return... that's why the manual
;;   file lookup
;; * purge the files right after decrypting and loading
;;   -> this causes an empty rectangle to be shown in the buffer
;; * hook to standard hooks such as:
;;   * buffer-list-update-hook
;;     -> basically kills Emacs by making it frozen due to too many updates
;;     and the initial startup with few hundreds of buffers opened is
;;     a time to go for a coffee
;;   * change-major-mode / after-change-major-mode-hook / org-mode-hook
;;     -> is unreliable from security perspective due to being too slow
;;
;; So I added the purging to these hooks so that an action of "going away"
;; from the org buffer or from Emacs alone, which would potentially endanger
;; the unencrypted data, would also purge them.  It's also trying to minimize
;; their overall presence too:
;; * org-mode-hook
;; * after-init-hook
;; * after-save-hook
;; * auto-save-hook
;; * desktop-save-hook
;; * dired-load-hook
;; * emacs-startup-hook
;; * find-file-hook
;; * org-epa-gpg-purge-hook
;; * quit-window-hook
;; * suspend-hook
;; * suspend-resume-hook

;;; Code:

(require 'org)

(defconst org-epa-gpg-temp-prefix
  "org-epa-gpg-" "Prefix for temporary decrypted files.")
(defconst org-epa-gpg-ext
  "gpg" "Default extension for epa-enabled file.")

(defcustom org-epa-gpg-purge-hook()
  "Hook for purging temporary decrypted content."
  :type 'hook
  :group 'org-epa-gpg)

(defun org-epa-gpg-get-purge-path()
  "Return path for purging of the decrypted files."
  (concat (temporary-file-directory) org-epa-gpg-temp-prefix "*"))

(defun org-epa-gpg-purge()
  "Purge temporary files created when decrypting images for `org-mode'."
  (interactive)
  (dolist (elem (file-expand-wildcards (org-epa-gpg-get-purge-path)))
    (delete-file elem)))

(defun org-epa-gpg-replace-epa-ext(file)
  "Clear .gpg extension from FILE path."
  (replace-regexp-in-string (concat "." org-epa-gpg-ext) "" file))

(defun org-epa-gpg-get-ext(file)
  "Pull image extension from FILE path ending with .gpg."
  (car (last (split-string file "\\."))))

(defun org-epa-gpg-get-orig-ext(file)
  "Pull original extension from FILE path endinf with <ext>.gpg."
  (org-epa-gpg-get-ext (org-epa-gpg-replace-epa-ext file)))

(defun org-epa-gpg-is-epa(file)
  "Check if FILE is epa-enabled encrypted file by extension."
  (string-equal (org-epa-gpg-get-ext file) org-epa-gpg-ext))

(defun org-epa-gpg-mktmp(ext)
  "Create a temp file with a custom extension EXT."
  (make-temp-file org-epa-gpg-temp-prefix nil (concat "." ext)))

(defun org-epa-gpg-log-file(file)
  "Log warning about unencrypted content in FILE."
  (message
   "Created temp file for encrypted content '%s', purge with %s"
   file 'org-epa-gpg-purge))

(defun org-epa-gpg-patch(old-func file &rest args)
  "Patch for image.el/create-image() function.
Argument OLD-FUNC Original function to call.
Argument FILE path for temporary file.
Optional argument ARGS Args to forward to the original func."
  (let ((temp-image-ext (org-epa-gpg-get-orig-ext file)))
    (when (org-epa-gpg-is-epa file)
      (let ((temp-file (org-epa-gpg-mktmp temp-image-ext)))
        (org-epa-gpg-log-file temp-file)
        (epa-decrypt-file file temp-file)
        (apply old-func temp-file args)))))

(defun org-epa-gpg-dedup-exts()
  "Return unique list of extensions with epa-enabled extension."
  (delete-dups
   (append image-file-name-extensions
           (mapcar (lambda (el) (format "%s.%s" el org-epa-gpg-ext))
                   image-file-name-extensions))))

(defun org-epa-gpg-update-exts()
  "Update image extensions by their .gpg pairs."
  (set (make-local-variable 'image-file-name-extensions)
       (org-epa-gpg-dedup-exts)))

(defun org-epa-gpg-inject-purge(&rest _)
  "Inject a purge hook after (org-toggle-inline-images)."
  (run-hooks 'org-epa-gpg-purge-hook))

(defun org-epa-gpg-patch-org-up()
  "Set up patches."
  (interactive)
  (org-epa-gpg-update-exts)
  (advice-add #'create-image :around #'org-epa-gpg-patch)
  (advice-add #'org-remove-inline-images :after #'org-epa-gpg-inject-purge))

(defun org-epa-gpg-patch-org-down()
  "Tear down patches."
  (interactive)
  (advice-remove #'create-image #'org-epa-gpg-patch)
  (org-epa-gpg-purge))

(defun org-epa-gpg-patch-org()
  "Patch `image-file-name-extensions', `create-image', `org-remove-inline-images'."
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-epa-gpg-patch-org-up) (org-epa-gpg-patch-org-down)))

(defun org-epa-gpg-enable()
  "Enable inlining encrypted .gpg images via `org-mode-hook'."
  (interactive)
  (add-hook 'org-mode-hook #'org-epa-gpg-patch-org)
  (add-hook 'after-init-hook #'org-epa-gpg-purge)
  (add-hook 'after-save-hook #'org-epa-gpg-purge)
  (add-hook 'auto-save-hook #'org-epa-gpg-purge)
  (add-hook 'desktop-save-hook #'org-epa-gpg-purge)
  (add-hook 'dired-load-hook #'org-epa-gpg-purge)
  (add-hook 'emacs-startup-hook #'org-epa-gpg-purge)
  (add-hook 'find-file-hook #'org-epa-gpg-purge)
  (add-hook 'org-epa-gpg-purge-hook #'org-epa-gpg-purge)
  (add-hook 'quit-window-hook #'org-epa-gpg-purge)
  (add-hook 'suspend-hook #'org-epa-gpg-purge)
  (add-hook 'suspend-resume-hook #'org-epa-gpg-purge))

(defun org-epa-gpg-disable()
  "Disable inlining encrypted .gpg images via `org-mode-hook'."
  (interactive)
  (remove-hook 'org-mode-hook #'org-epa-gpg-patch-org)
  (remove-hook 'after-init-hook #'org-epa-gpg-purge)
  (remove-hook 'after-save-hook #'org-epa-gpg-purge)
  (remove-hook 'auto-save-hook #'org-epa-gpg-purge)
  (remove-hook 'desktop-save-hook #'org-epa-gpg-purge)
  (remove-hook 'dired-load-hook #'org-epa-gpg-purge)
  (remove-hook 'emacs-startup-hook #'org-epa-gpg-purge)
  (remove-hook 'find-file-hook #'org-epa-gpg-purge)
  (remove-hook 'org-epa-gpg-purge-hook #'org-epa-gpg-purge)
  (remove-hook 'quit-window-hook #'org-epa-gpg-purge)
  (remove-hook 'suspend-hook #'org-epa-gpg-purge)
  (remove-hook 'suspend-resume-hook #'org-epa-gpg-purge)
  (org-epa-gpg-patch-org-down))

(provide 'org-epa-gpg)
;;; org-epa-gpg.el ends here
