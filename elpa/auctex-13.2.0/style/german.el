;;; german.el --- Setup AUCTeX for editing German text.  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Cater for some specialities of `(n)german.sty', e.g. special quote
;; and hyphen strings or that `"' makes the following letter an
;; umlaut.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-quotes
                  "font-latex"
                  (quotes))

(declare-function font-latex-add-to-syntax-alist
                  "font-latex"
                  (list))

(defvar LaTeX-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `german.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-german-mode-syntax-table)

(TeX-add-style-hook
 "german"
 (lambda ()
   (set-syntax-table LaTeX-german-mode-syntax-table)
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language '("german" "\"`" "\"'" t)))
   (setq LaTeX-babel-hyphen-language "german")
   ;; Fontification of quotation marks.
   (when (and (eq TeX-install-font-lock 'font-latex-setup)
              (featurep 'font-latex))
     (font-latex-add-quotes '("\"`" "\"'"))
     (font-latex-add-quotes '("\">" "\"<" german))
     ;; Prevent "| from leading to color bleed.
     (font-latex-add-to-syntax-alist (list (cons ?\" "\\"))))
   (run-hooks 'TeX-language-de-hook))
 TeX-dialect)

;;; german.el ends here
