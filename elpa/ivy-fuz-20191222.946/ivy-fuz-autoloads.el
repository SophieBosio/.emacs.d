;;; ivy-fuz-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-fuz" "ivy-fuz.el" (0 0 0 0))
;;; Generated autoloads from ivy-fuz.el

(defalias 'ivy-fuz-regex-fuzzy #'ivy--regex-fuzzy)

(autoload 'ivy-fuz-sort-fn "ivy-fuz" "\
Sort CANDS according to PATTERN.

\(fn PATTERN CANDS)" nil nil)

(autoload 'ivy-fuz-highlight-fn "ivy-fuz" "\
Put highlight face on matched positions of the STR.

\(fn STR)" nil nil)

(register-definition-prefixes "ivy-fuz" '("ivy-fuz-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-fuz-autoloads.el ends here
