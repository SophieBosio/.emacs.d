;;; dk.el - Setup AUCTeX for editing Danish text.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)

(TeX-add-style-hook
 "dk"
 (lambda ()
   (run-hooks 'TeX-language-dk-hook))
 TeX-dialect)

;;; dk.el ends here
