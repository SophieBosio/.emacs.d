;;; proof-general-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "proof-general" "proof-general.el" (0 0 0 0))
;;; Generated autoloads from proof-general.el

(eval-and-compile (defvar pg-init--script-full-path (or (bound-and-true-p byte-compile-current-file) (and load-in-progress load-file-name) (buffer-file-name))) (defvar pg-init--pg-root (file-name-directory pg-init--script-full-path)))

(unless (bound-and-true-p byte-compile-current-file) (require 'proof-site (expand-file-name "generic/proof-site" pg-init--pg-root)))

;;;***

;;;### (autoloads nil nil ("proof-general-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; proof-general-autoloads.el ends here
