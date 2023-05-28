;;; plhb.el - Setup AUC TeX for editing Polish text with plhb.sty  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `plhb.sty' use `"' to make next character Polish.
;; `plhb.sty' <C> J. S. Bie\'n, IIUW, jsbien@mimuw.edu.pl

;;; Code:

(require 'tex)
(require 'latex)

(defvar LaTeX-plhb-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `plhb.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-plhb-mode-syntax-table)

(TeX-add-style-hook
 "plhb"
 (lambda ()
   (set-syntax-table LaTeX-plhb-mode-syntax-table)
   (make-local-variable 'TeX-open-quote)
   (make-local-variable 'TeX-close-quote)
   (make-local-variable 'TeX-command-default)
   (make-local-variable 'TeX-quote-after-quote)
   (setq TeX-open-quote "\"<")
   (setq TeX-close-quote "\">")
   (setq TeX-quote-after-quote t)
   (setq TeX-command-default "plLaTeX")
   (run-hooks 'TeX-language-pl-hook))
 TeX-dialect)

;;; plhb.el ends here
