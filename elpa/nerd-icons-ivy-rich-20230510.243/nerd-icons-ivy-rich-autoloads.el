;;; nerd-icons-ivy-rich-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nerd-icons-ivy-rich" "nerd-icons-ivy-rich.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from nerd-icons-ivy-rich.el

(defvar nerd-icons-ivy-rich-mode nil "\
Non-nil if Nerd-Icons-Ivy-Rich mode is enabled.
See the `nerd-icons-ivy-rich-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nerd-icons-ivy-rich-mode'.")

(custom-autoload 'nerd-icons-ivy-rich-mode "nerd-icons-ivy-rich" nil)

(autoload 'nerd-icons-ivy-rich-mode "nerd-icons-ivy-rich" "\
Better experience with icons for ivy.

This is a minor mode.  If called interactively, toggle the
`Nerd-Icons-Ivy-Rich mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='nerd-icons-ivy-rich-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'nerd-icons-ivy-rich-reload "nerd-icons-ivy-rich" "\
Reload `nerd-icons-ivy-rich'." t nil)

(register-definition-prefixes "nerd-icons-ivy-rich" '("nerd-icons-ivy-rich-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nerd-icons-ivy-rich-autoloads.el ends here
