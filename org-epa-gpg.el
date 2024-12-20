;;; org-epa-gpg.el --- Patch to enable EasyPG .gpg images in Org mode inline

;; Copyright (C) 2022-2024 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: lisp, org, gpg, pgp, epa, encryption, image, inline, patch
;; Version: 3.0.0
;; Package-Requires: ((emacs "27.1"))
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
;; To start, add this use-package to your config file to enable it globally:
;;
;; (use-package org-epa-gpg
;;   :ensure t
;;   :after epa-file
;;   :config (org-epa-gpg-enable))
;;
;; Or use `org-epa-gpg-mode' as buffer-local alternative.
;;
;; The cleaning of the decrypted content (in /tmp via (make-temp-file) )
;; is currently done via patching (org-remove-inline-images) with a custom
;; hook (so that it's purged on (C-c C-x C-v) untoggling and via attaching
;; to multiple standard hooks.
;;
;; What I attempted to make it less insane:
;; * purge the files right after decrypting and loading
;;   -> this causes an empty rectangle in the buffer with hot-reloading
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
;; their overall presence too `org-epa-gpg-purging-hooks'
;;
;; A timer could possibly be added with a very short interval, but it doesn't
;; seem feasible if the amount of images is more than a few (large Org
;; documents) due to 1:1 (image:purger) relationship in `list-timers' possibly
;; hogging Emacs down readability-/resource-wise.  Or at least, if turned on by
;; default.

;;; Code:

(require 'org)

(defgroup org-epa-gpg
  nil
  "Customization group for `org-epa-gpg-mode' and friends."
  :group 'editing
  :group 'files
  :group 'data
  :group 'convenience
  :group 'org)

(defcustom org-epa-gpg-purge-hook nil
  "Temp data purge hook, run after `org-toggle-inline-images'."
  :type 'hook
  :group 'org-epa-gpg)

;; constants
(defconst org-epa-gpg-temp-prefix
  "org-epa-gpg-"
  "Prefix for temporary decrypted files.")

(defconst org-epa-gpg-ext
  "gpg"
  "Default extension for epa-enabled file.")

(defconst org-epa-gpg-purging-hooks
  '('after-init-hook
    'after-save-hook
    'auto-save-hook
    'desktop-save-hook
    'dired-load-hook
    'emacs-startup-hook
    'find-file-hook
    'org-epa-gpg-purge-hook
    'quit-window-hook
    'suspend-hook
    'suspend-resume-hook)
  "List of hooks which trigger temp data purging function.")

(defvar org-epa-gpg--advices nil
  "Tracking for buffer-local advices.")

;; private/helper funcs
(defun org-epa-gpg--get-purge-path ()
  "Return path for purging of the decrypted files."
  (concat (temporary-file-directory) org-epa-gpg-temp-prefix "*"))

(defun org-epa-gpg--replace-epa-ext (file)
  "Clear .gpg extension from FILE path."
  (replace-regexp-in-string (concat "." org-epa-gpg-ext) "" file))

(defun org-epa-gpg--get-ext (file)
  "Pull image extension from FILE path ending with .gpg."
  (car (last (split-string file "\\."))))

(defun org-epa-gpg--get-orig-ext (file)
  "Pull original extension from FILE path endinf with <ext>.gpg."
  (org-epa-gpg--get-ext (org-epa-gpg--replace-epa-ext file)))

(defun org-epa-gpg--mktmp (ext)
  "Create a temp file with a custom extension EXT."
  (make-temp-file org-epa-gpg-temp-prefix nil (concat "." ext)))

(defun org-epa-gpg--log-file (file)
  "Log warning about unencrypted content in FILE."
  (message
   "org-epa-gpg: Created temp file for encrypted content '%s', purge with %s"
   file 'org-epa-gpg-purge))

(defun org-epa-gpg--patch (old-func file &rest args)
  "Patch for image.el/create-image() function.
Argument OLD-FUNC Original function to call.
Argument FILE path for temporary file.
Optional argument ARGS Args to forward to the original func."
  (if (member (current-buffer) (alist-get 'create-image org-epa-gpg--advices))
      (let ((temp-image-ext (org-epa-gpg--get-orig-ext file)))
        (if (org-epa-gpg-p file)
            (let ((temp-file (org-epa-gpg--mktmp temp-image-ext)))
              (org-epa-gpg--log-file temp-file)
              (epa-decrypt-file file temp-file)
              (apply old-func temp-file args))
          (apply old-func file args)))
    (apply old-func file args)))

(defun org-epa-gpg--dedup-exts ()
  "Return unique list of extensions with epa-enabled extension."
  (delete-dups
   (append image-file-name-extensions
           (mapcar (lambda (el) (format "%s.%s" el org-epa-gpg-ext))
                   image-file-name-extensions))))

(defun org-epa-gpg--inject-purge (&rest _)
  "Inject a purge hook after (org-toggle-inline-images)."
  (when (member (current-buffer) (alist-get 'org-inline org-epa-gpg--advices))
    (run-hooks 'org-epa-gpg-purge-hook)))

(defun org-epa-gpg--patch-image-exts (old &optional _)
  "Suffix all `image-file-name-regexp' with `org-epa-gpg-ext'.
Argument OLD Original `image-file-name-regexp' ref."
  (if (member (current-buffer) (alist-get 'image-exts org-epa-gpg--advices))
      (let ((image-file-name-extensions
             (org-epa-gpg--dedup-exts)))
        (funcall old))
    (funcall old)))

(defun org-epa-gpg--patch-org-up ()
  "Set up patches."
  (advice-add #'image-file-name-regexp
              :around
              #'org-epa-gpg--patch-image-exts)
  (push (current-buffer) (alist-get 'image-exts org-epa-gpg--advices))

  (advice-add #'create-image
              :around #'org-epa-gpg--patch)
  (push (current-buffer) (alist-get 'create-image org-epa-gpg--advices))

  (advice-add #'org-remove-inline-images
              :after #'org-epa-gpg--inject-purge)
  (push (current-buffer) (alist-get 'org-inline org-epa-gpg--advices)))

(defun org-epa-gpg--patch-org-down ()
  "Tear down patches."
  (advice-remove #'image-file-name-regexp
                 #'org-epa-gpg--patch-image-exts)
  (setf (alist-get 'image-exts org-epa-gpg--advices)
        (remove (current-buffer)
                (alist-get 'image-exts org-epa-gpg--advices)))

  (advice-remove #'create-image #'org-epa-gpg--patch)
  (setf (alist-get 'create-image org-epa-gpg--advices)
        (remove (current-buffer)
                (alist-get 'create-image org-epa-gpg--advices)))

  (org-epa-gpg-purge)

  (advice-remove #'org-remove-inline-images
                 #'org-epa-gpg--inject-purge)
  (setf (alist-get 'org-inline org-epa-gpg--advices)
        (remove (current-buffer)
                (alist-get 'org-inline org-epa-gpg--advices))))

(defun org-epa-gpg--patch-org ()
  "Patch Org mode functions and its upstream call.
- `image-file-name-extensions'
- `create-image'
- `org-remove-inline-images'"
  (if (eq major-mode 'org-mode)
      (org-epa-gpg--patch-org-up) (org-epa-gpg--patch-org-down)))

;; public funcs
(defun org-epa-gpg-purge ()
  "Purge temporary files created when decrypting images for `org-mode'."
  (interactive)
  (dolist (elem (file-expand-wildcards (org-epa-gpg--get-purge-path)))
    (delete-file elem)))

(defun org-epa-gpg-p (file)
  "Check if FILE is epa-enabled encrypted file by extension."
  (string-equal (org-epa-gpg--get-ext file) org-epa-gpg-ext))

(defun org-epa-gpg-enable ()
  "Enable inlining encrypted .gpg images."
  (interactive)
  ;; explicitly disable, there's no mode to check
  (add-hook 'kill-buffer-hook #'org-epa-gpg-disable 0 t)

  ;; purge hooks
  (dolist (hook org-epa-gpg-purging-hooks)
    (add-hook (cadr hook) #'org-epa-gpg-purge 0 t))

  ;; hooks up, patch org
  (org-epa-gpg--patch-org))

(defun org-epa-gpg-disable ()
  "Disable inlining encrypted .gpg images."
  (interactive)
  (remove-hook 'kill-buffer-hook #'org-epa-gpg-disable t)

  ;; purge hooks
  (dolist (hook org-epa-gpg-purging-hooks)
    (remove-hook (cadr hook) #'org-epa-gpg-purge t))

  ;; hooks down, unpatch org
  (org-epa-gpg--patch-org-down))

;;;###autoload
(define-minor-mode org-epa-gpg-mode
  "Inline encrypted .gpg images with `epa'."
  :group 'org
  :group 'org-epa
  :group 'org-epa-gpg
  :lighter " org-epa-gpg"
  (if org-epa-gpg-mode
      (org-epa-gpg-enable)
    (org-epa-gpg-disable)))

(provide 'org-epa-gpg)
;;; org-epa-gpg.el ends here
