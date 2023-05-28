;;; currvita.el --- AUCTeX style for `currvita.sty' (v0.9i)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015--2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2015-01-05
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

;; This file adds support for `currvita.sty' (v0.9i) from 1999/09/13.
;; `currvita.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "currvita"
 (lambda ()

   ;; env's defined by currvita.sty
   (LaTeX-add-environments
    '("cv"     "Heading of CV")
    '("cvlist" LaTeX-env-item-args "Heading of list"))

   ;; Add "cvlist" to the list of environments which have an optional
   ;; argument for each item
   (add-to-list 'LaTeX-item-list '("cvlist" . LaTeX-item-argument))

   ;; General commands: "\date" is already provided by AUCTeX
   (TeX-add-symbols
    '("cvplace" t)
    "cvheadingfont"
    "cvlistheadingfont"
    "cvlabelfont"
    "cvbibname")

   ;; Add new lengths defined by currvita.sty
   (LaTeX-add-lengths "cvlabelwidth" "cvlabelskip" "cvlabelsep")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("cvplace" "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-currvita-package-options
  '("LabelsAligned" "TextAligned" "openbib" "ManyBibs" "NoDate")
  "Package options for the currvita package.")

;;; currvita.el ends here
