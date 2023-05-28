;;; bigstrut.el --- AUCTeX style for `bigstrut.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2014--2022 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
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

;; This file adds support for `bigstrut.sty', v2.6 from 2021/01/02.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "bigstrut"
 (lambda ()
   (TeX-add-symbols
    '("bigstrut"
      [TeX-arg-completing-read ("t" "b")
                               "Strut to top (t) or bottom (b)"]))

   (LaTeX-add-lengths "bigstrutjot")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("bigstrut" "["))
                              'function)))
 TeX-dialect)

(defvar LaTeX-bigstrut-package-options nil
  "Package options for the bigstrut package.")

;;; bigstrut.el ends here
