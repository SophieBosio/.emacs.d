;;; floatpag.el --- AUCTeX style for `floatpag.sty' (v2.0)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-12-11
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `floatpag.sty' (v2.0) from 2021/10/04.
;; `floatpag.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "floatpag"
 (lambda ()

   ;; Macros always available:
   (TeX-add-symbols
    '("floatpagestyle" (TeX-arg-pagestyle "Float pagestyle"))
    '("thisfloatpagestyle" (TeX-arg-pagestyle "Float pagestyle")))

   ;; The next one makes only sense if 'rotating' package is loaded:
   (when (member "rotating" (TeX-style-list))
     (TeX-add-symbols
      '("rotfloatpagestyle" (TeX-arg-pagestyle "Float pagestyle"))))

   ;; Load 'array.el' when the package option is provided:
   (when (LaTeX-provided-package-options-member "floatpag" "array")
     (TeX-run-style-hooks "array"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("floatpagestyle"     "{")
                                ("thisfloatpagestyle" "{"))
                              'function)
     (when (member "rotating" (TeX-style-list))
       (font-latex-add-keywords '(("rotfloatpagestyle" "{"))
                                'function))))

 TeX-dialect)

(defvar LaTeX-floatpag-package-options '("array")
  "Package options for the floatpag package.")

;;; floatpag.el ends here
