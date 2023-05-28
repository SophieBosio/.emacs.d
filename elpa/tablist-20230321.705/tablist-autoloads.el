;;; tablist-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tablist" "tablist.el" (0 0 0 0))
;;; Generated autoloads from tablist.el

(autoload 'tablist-minor-mode "tablist" "\
Toggle tablist minor mode.

This is a minor mode.  If called interactively, toggle the
`Tablist minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tablist-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'tablist-mode "tablist" "\


\(fn)" t nil)

(register-definition-prefixes "tablist" '("tablist-"))

;;;***

;;;### (autoloads nil "tablist-filter" "tablist-filter.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from tablist-filter.el

(register-definition-prefixes "tablist-filter" '("tablist-filter-"))

;;;***

;;;### (autoloads nil nil ("tablist-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tablist-autoloads.el ends here
