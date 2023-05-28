;;; helpful-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helpful" "helpful.el" (0 0 0 0))
;;; Generated autoloads from helpful.el

(autoload 'helpful-function "helpful" "\
Show help for function named SYMBOL.

See also `helpful-macro', `helpful-command' and `helpful-callable'.

\(fn SYMBOL)" t nil)

(autoload 'helpful-command "helpful" "\
Show help for interactive function named SYMBOL.

See also `helpful-function'.

\(fn SYMBOL)" t nil)

(autoload 'helpful-key "helpful" "\
Show help for interactive command bound to KEY-SEQUENCE.

\(fn KEY-SEQUENCE)" t nil)

(autoload 'helpful-macro "helpful" "\
Show help for macro named SYMBOL.

\(fn SYMBOL)" t nil)

(autoload 'helpful-callable "helpful" "\
Show help for function, macro or special form named SYMBOL.

See also `helpful-macro', `helpful-function' and `helpful-command'.

\(fn SYMBOL)" t nil)

(autoload 'helpful-symbol "helpful" "\
Show help for SYMBOL, a variable, function or macro.

See also `helpful-callable' and `helpful-variable'.

\(fn SYMBOL)" t nil)

(autoload 'helpful-variable "helpful" "\
Show help for variable named SYMBOL.

\(fn SYMBOL)" t nil)

(autoload 'helpful-at-point "helpful" "\
Show help for the symbol at point." t nil)

(register-definition-prefixes "helpful" '("helpful-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helpful-autoloads.el ends here
