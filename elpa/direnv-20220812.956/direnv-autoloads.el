;;; direnv-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "direnv" "direnv.el" (0 0 0 0))
;;; Generated autoloads from direnv.el

(autoload 'direnv-update-environment "direnv" "\
Update the environment for FILE-NAME.

See `direnv-update-directory-environment' for FORCE-SUMMARY.

\(fn &optional FILE-NAME FORCE-SUMMARY)" t nil)

(autoload 'direnv-update-directory-environment "direnv" "\
Update the environment for DIRECTORY.

When FORCE-SUMMARY is non-nil or when called interactively, show
a summary message.

\(fn &optional DIRECTORY FORCE-SUMMARY)" t nil)

(autoload 'direnv-allow "direnv" "\
Run ‘direnv allow’ and update the environment afterwards." t nil)

(defvar direnv-mode nil "\
Non-nil if Direnv mode is enabled.
See the `direnv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `direnv-mode'.")

(custom-autoload 'direnv-mode "direnv" nil)

(autoload 'direnv-mode "direnv" "\
Global minor mode to automatically update the environment using direnv.

This is a minor mode.  If called interactively, toggle the
`Direnv mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='direnv-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When this mode is active, the environment inside Emacs will be
continuously updated to match the direnv environment for the currently
visited (local) file.

\(fn &optional ARG)" t nil)

(autoload 'direnv-envrc-mode "direnv" "\
Major mode for .envrc files as used by direnv.

Since .envrc files are shell scripts, this mode inherits from ‘sh-mode’.
\\{direnv-envrc-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.envrc\\'" . direnv-envrc-mode))

(register-definition-prefixes "direnv" '("direnv-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; direnv-autoloads.el ends here
