;;; golden-ratio-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "golden-ratio" "golden-ratio.el" (0 0 0 0))
;;; Generated autoloads from golden-ratio.el

(autoload 'golden-ratio "golden-ratio" "\
Resizes current window to the golden-ratio's size specs.

\(fn &optional ARG)" t nil)

(defvar golden-ratio-mode nil "\
Non-nil if Golden-Ratio mode is enabled.
See the `golden-ratio-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `golden-ratio-mode'.")

(custom-autoload 'golden-ratio-mode "golden-ratio" nil)

(autoload 'golden-ratio-mode "golden-ratio" "\
Enable automatic window resizing with golden ratio.

This is a minor mode.  If called interactively, toggle the
`Golden-Ratio mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='golden-ratio-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "golden-ratio" '("golden-ratio-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; golden-ratio-autoloads.el ends here
