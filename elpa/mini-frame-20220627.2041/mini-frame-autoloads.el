;;; mini-frame-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mini-frame" "mini-frame.el" (0 0 0 0))
;;; Generated autoloads from mini-frame.el

(defvar mini-frame-mode nil "\
Non-nil if Mini-Frame mode is enabled.
See the `mini-frame-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mini-frame-mode'.")

(custom-autoload 'mini-frame-mode "mini-frame" nil)

(autoload 'mini-frame-mode "mini-frame" "\
Show minibuffer in child frame on read-from-minibuffer.

This is a minor mode.  If called interactively, toggle the
`Mini-Frame mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='mini-frame-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "mini-frame" '("mini-frame-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mini-frame-autoloads.el ends here
