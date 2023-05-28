;;; j-article.el - Special code for j-article style.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "j-article"
 (lambda ()
   (LaTeX-largest-level-set "section"))
 TeX-dialect)

;;; j-article.el ends here
