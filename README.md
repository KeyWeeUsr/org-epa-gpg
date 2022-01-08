# org-epa-gpg

This is a patch that attempts to fix image inlining (C-c C-x C-v) in
the org-mode for images that are encrypted AND end with ".gpg" extension.

## Install and run

For installation simply issue this command, it'll download the lisp module
from this repo directly under the latest stable tag (or replace the tag
version with `master` for the latest code).

```emacs-lisp
(let ((file (make-temp-file "" nil ".el")))
(url-copy-file
 "https://raw.githubusercontent.com/KeyWeeUsr/org-epa-gpg/1.0.0/org-epa-gpg.el"
 file t)
(package-install-file file))
```

To start, add these mandatory lines to your config file:

```emacs-lisp
(require 'org-epa-gpg)
(org-epa-gpg-enable)
```

## Notes on purging

The cleaning of the decrypted content (in /tmp via (make-temp-file) )
is currently done via patching (org-remove-inline-images) with a custom
hook (so that it's purged on (C-c C-x C-v) untoggling and via attaching
to multiple standard hooks.

### Attempted

What I attempted to make it less insane:
* return the paths from functions
  -> Lisp/ELisp doesn't return values as a normal programming language,
  such as store first, then do stuff, then return... that's why the manual
  file lookup
* purge the files right after decrypting and loading
  -> this causes an empty rectangle to be shown in the buffer
* hook to standard hooks such as:
  * buffer-list-update-hook
    -> basically kills Emacs by making it frozen due to too many updates
    and the initial startup with few hundreds of buffers opened is
    a time to go for a coffee
  * change-major-mode / after-change-major-mode-hook / org-mode-hook
    -> is unreliable from security perspective due to being too slow

### Current behavior

So I added the purging to these hooks so that an action of "going away"
from the org buffer or from Emacs alone, which would potentially endanger
the unencrypted data, would also purge them. It's also trying to minimize
their overall presence too:

* org-mode-hook
* after-init-hook
* after-save-hook
* auto-save-hook
* desktop-save-hook
* dired-load-hook
* emacs-startup-hook
* find-file-hook
* org-epa-gpg-purge-hook
* quit-window-hook
* suspend-hook
* suspend-resume-hook
