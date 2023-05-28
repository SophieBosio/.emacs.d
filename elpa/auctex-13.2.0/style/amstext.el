;;; amstext.el --- Style hook for the AMS-LaTeX amstext package.  -*- lexical-binding: t; -*-
;;;
;;; AUTHOR: Carsten Dominik <dominik@strw.leidenuniv.nl>

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "amstext"
 (lambda ()
   (TeX-add-symbols
    '("text" t)))
 TeX-dialect)

(defvar LaTeX-amstext-package-options nil
  "Package options for the amstext package.")

;;; amstext.el ends here.
