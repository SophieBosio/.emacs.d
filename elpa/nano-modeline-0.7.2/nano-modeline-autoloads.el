;;; nano-modeline-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nano-modeline" "nano-modeline.el" (0 0 0 0))
;;; Generated autoloads from nano-modeline.el

(defvar nano-modeline-mode nil "\
Non-nil if Nano-Modeline mode is enabled.
See the `nano-modeline-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nano-modeline-mode'.")

(custom-autoload 'nano-modeline-mode "nano-modeline" nil)

(autoload 'nano-modeline-mode "nano-modeline" "\
Toggle nano-modeline minor mode

This is a minor mode.  If called interactively, toggle the
`Nano-Modeline mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='nano-modeline-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "nano-modeline" '("nano-modeline"))

;;;***

;;;### (autoloads nil nil ("nano-modeline-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nano-modeline-autoloads.el ends here
