;;; graphics.el --- Handle graphical commands in LaTeX 2e.  -*- lexical-binding: t; -*-

;;; Code:

;; Load "graphicx" explicitly to access `LaTeX-graphicx-package-options'
;; before running style hook "graphics".  This is necessary to have
;; support for completion of package options of "usepackage".

(require 'tex)

(TeX-load-style "graphicx")
(defvar LaTeX-graphics-package-options LaTeX-graphicx-package-options)

(TeX-add-style-hook
 "graphics"
 (lambda ()
   (TeX-run-style-hooks "graphicx"))
 TeX-dialect)

;;; graphics.el ends here.
