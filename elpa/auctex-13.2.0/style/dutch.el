;;; dutch.el - Setup AUCTeX for editing Dutch text.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)

(TeX-add-style-hook
 "dutch"
 (lambda ()
   (run-hooks 'TeX-language-nl-hook))
 TeX-dialect)

;;; dutch.el ends here
