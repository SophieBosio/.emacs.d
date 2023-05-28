;;; amsbook.el --- Style hook for the AMS-LaTeX book document class.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "amsbook"
 (lambda ()
   (TeX-run-style-hooks "amsmath" "amsthm")
   (LaTeX-add-environments "abstract"))
 TeX-dialect)

;;; amsbook.el ends here.
