;;; english.el --- Setup AUCTeX for editing English text.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)

(TeX-add-style-hook
 "english"
 (lambda ()
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language nil))
   (run-hooks 'TeX-language-en-hook))
 TeX-dialect)

;;; english.el ends here
