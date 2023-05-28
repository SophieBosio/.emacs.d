;;; cycle-themes-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cycle-themes" "cycle-themes.el" (0 0 0 0))
;;; Generated autoloads from cycle-themes.el

(defvar cycle-themes-mode nil "\
Non-nil if Cycle-Themes mode is enabled.
See the `cycle-themes-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `cycle-themes-mode'.")

(custom-autoload 'cycle-themes-mode "cycle-themes" nil)

(autoload 'cycle-themes-mode "cycle-themes" "\
Minor mode for cycling between themes.

This is a minor mode.  If called interactively, toggle the
`Cycle-Themes mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='cycle-themes-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "cycle-themes" '("cycle-themes"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cycle-themes-autoloads.el ends here
