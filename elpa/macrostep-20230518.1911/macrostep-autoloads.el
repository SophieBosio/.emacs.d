;;; macrostep-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "macrostep" "macrostep.el" (0 0 0 0))
;;; Generated autoloads from macrostep.el

(autoload 'macrostep-mode "macrostep" "\
Minor mode for inline expansion of macros in Emacs Lisp source buffers.

This is a minor mode.  If called interactively, toggle the
`Macrostep mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `macrostep-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\<macrostep-mode-map>Progressively expand macro forms with \\[macrostep-expand], collapse them with \\[macrostep-collapse],
and move back and forth with \\[macrostep-next-macro] and \\[macrostep-prev-macro].  Use \\[macrostep-collapse-all] or collapse all
visible expansions to quit and return to normal editing.

\\{macrostep-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'macrostep-expand "macrostep" "\
Expand the macro form following point by one step.

Enters `macrostep-mode' if it is not already active, making the
buffer temporarily read-only.  If `macrostep-mode' is active and the
form following point is not a macro form, search forward in the
buffer and expand the next macro form found, if any.

With a prefix argument, the expansion is displayed in a separate
buffer instead of inline in the current buffer.  Setting
`macrostep-expand-in-separate-buffer' to non-nil swaps these two
behaviors.

\(fn &optional TOGGLE-SEPARATE-BUFFER)" t nil)

(register-definition-prefixes "macrostep" '("macrostep-"))

;;;***

;;;### (autoloads nil "macrostep-c" "macrostep-c.el" (0 0 0 0))
;;; Generated autoloads from macrostep-c.el

(autoload 'macrostep-c-mode-hook "macrostep-c" nil nil nil)

(add-hook 'c-mode-hook #'macrostep-c-mode-hook)

(register-definition-prefixes "macrostep-c" '("macrostep-c-"))

;;;***

;;;### (autoloads nil nil ("macrostep-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; macrostep-autoloads.el ends here
