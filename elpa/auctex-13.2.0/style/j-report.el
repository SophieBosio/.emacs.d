;;; j-report.el - Special code for j-report style.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "j-report"
 (lambda ()
   (LaTeX-largest-level-set "chapter"))
 TeX-dialect)

;;; j-report.el ends here
