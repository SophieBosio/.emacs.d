;;; define-word-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "define-word" "define-word.el" (0 0 0 0))
;;; Generated autoloads from define-word.el

(autoload 'define-word "define-word" "\
Define WORD using various services.

By default uses `define-word-default-service', but a prefix arg
lets the user choose service.

\(fn WORD SERVICE &optional CHOOSE-SERVICE)" t nil)

(autoload 'define-word-at-point "define-word" "\
Use `define-word' to define word at point.
When the region is active, define the marked phrase.
Prefix ARG lets you choose service.

In a non-interactive call SERVICE can be passed.

\(fn ARG &optional SERVICE)" t nil)

(register-definition-prefixes "define-word" '("define-word-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; define-word-autoloads.el ends here
