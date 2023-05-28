;;; lsp-treemacs-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-treemacs" "lsp-treemacs.el" (0 0 0 0))
;;; Generated autoloads from lsp-treemacs.el

(autoload 'lsp-treemacs-symbols "lsp-treemacs" "\
Show symbols view." t nil)

(autoload 'lsp-treemacs-java-deps-list "lsp-treemacs" "\
Display java dependencies." t nil)

(autoload 'lsp-treemacs-java-deps-follow "lsp-treemacs" nil t nil)

(defvar lsp-treemacs-sync-mode nil "\
Non-nil if Lsp-Treemacs-Sync mode is enabled.
See the `lsp-treemacs-sync-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-treemacs-sync-mode'.")

(custom-autoload 'lsp-treemacs-sync-mode "lsp-treemacs" nil)

(autoload 'lsp-treemacs-sync-mode "lsp-treemacs" "\
Global minor mode for synchronizing lsp-mode workspace folders and treemacs projects.

This is a minor mode.  If called interactively, toggle the
`Lsp-Treemacs-Sync mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='lsp-treemacs-sync-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'lsp-treemacs-references "lsp-treemacs" "\
Show the references for the symbol at point.
With a prefix argument, select the new window and expand the tree of references automatically.

\(fn ARG)" t nil)

(autoload 'lsp-treemacs-implementations "lsp-treemacs" "\
Show the implementations for the symbol at point.
With a prefix argument, select the new window expand the tree of implementations automatically.

\(fn ARG)" t nil)

(autoload 'lsp-treemacs-call-hierarchy "lsp-treemacs" "\
Show the incoming call hierarchy for the symbol at point.
With a prefix argument, show the outgoing call hierarchy.

\(fn OUTGOING)" t nil)

(autoload 'lsp-treemacs-type-hierarchy "lsp-treemacs" "\
Show the type hierarchy for the symbol at point.
With prefix 0 show sub-types.
With prefix 1 show super-types.
With prefix 2 show both.

\(fn DIRECTION)" t nil)

(autoload 'lsp-treemacs-errors-list "lsp-treemacs" nil t nil)

(register-definition-prefixes "lsp-treemacs" '("lsp-treemacs-"))

;;;***

;;;### (autoloads nil "lsp-treemacs-generic" "lsp-treemacs-generic.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-treemacs-generic.el

(register-definition-prefixes "lsp-treemacs-generic" '("lsp-treemacs-"))

;;;***

;;;### (autoloads nil "lsp-treemacs-themes" "lsp-treemacs-themes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-treemacs-themes.el

(register-definition-prefixes "lsp-treemacs-themes" '("lsp-treemacs-theme"))

;;;***

;;;### (autoloads nil nil ("lsp-treemacs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-treemacs-autoloads.el ends here
