;;; ifluatex.el --- AUCTeX style for `ifluatex.sty' version 1.5.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014--2022 Free Software Foundation, Inc.

;; Author: Davide G. M. Salvetti <salve@debian.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-11-15
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

;; This file adds support for `ifluatex.sty' 1.5.  The package is now
;; part of 'iftex' bundle and therefore, the code is now in
;; 'iftex.el'.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "ifluatex"
 (lambda ()

   ;; Run the style hook for 'iftex' which does the work:
   (TeX-run-style-hooks "iftex")

   (TeX-add-symbols
    '("luatexversion" 0)
    '("luatexrevision" 0))

   (TeX-declare-expert-macros
    "ifluatex"
    "luatexversion" "luatexrevision")

   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("luatexversion")
                                ("luatexrevision"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-ifluatex-package-options nil
  "Package options for the ifluatex package.")

;;; ifluatex.el ends here
