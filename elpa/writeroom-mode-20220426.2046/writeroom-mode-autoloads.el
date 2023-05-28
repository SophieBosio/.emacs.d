;;; writeroom-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "writeroom-mode" "writeroom-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from writeroom-mode.el

(autoload 'writeroom-mode "writeroom-mode" "\
Minor mode for distraction-free writing.

This is a minor mode.  If called interactively, toggle the
`Writeroom mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `writeroom-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-writeroom-mode 'globalized-minor-mode t)

(defvar global-writeroom-mode nil "\
Non-nil if Global Writeroom mode is enabled.
See the `global-writeroom-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-writeroom-mode'.")

(custom-autoload 'global-writeroom-mode "writeroom-mode" nil)

(autoload 'global-writeroom-mode "writeroom-mode" "\
Toggle Writeroom mode in all buffers.
With prefix ARG, enable Global Writeroom mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Writeroom mode is enabled in all buffers where
`turn-on-writeroom-mode' would do it.

See `writeroom-mode' for more information on Writeroom mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "writeroom-mode" '("alpha" "bottom-divider-width" "define-writeroom-global-effect" "fullscreen" "internal-border-width" "menu-bar-lines" "sticky" "tool-bar-lines" "turn-on-writeroom-mode" "vertical-scroll-bars" "writeroom-"))

;;;***

;;;### (autoloads nil nil ("writeroom-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; writeroom-mode-autoloads.el ends here
