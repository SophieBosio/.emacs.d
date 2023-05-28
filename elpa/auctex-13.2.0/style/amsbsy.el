;;; amsbsy.el --- Style hook for the AMS-LaTeX amsbsy package.  -*- lexical-binding: t; -*-
;;;
;;; AUTHOR: Carsten Dominik <dominik@strw.leidenuniv.nl>

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "amsbsy"
 (lambda ()
   (TeX-add-symbols
    '("boldsymbol" "Symbol")
    '("pmb"        "Symbol")))
 TeX-dialect)

(defvar LaTeX-amsbsy-package-options nil
  "Package options for the amsbsy package.")

;;; amsbsy.el ends here.
