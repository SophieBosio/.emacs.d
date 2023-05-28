;;; multicol.el --- AUCTeX style for `multicol.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2011, 2020, 2021 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2011-01-24
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

;; This file adds support for `multicol.sty' v1.9b from 2021/10/28.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "multicol"
 (lambda ()
   (LaTeX-add-environments
    '("multicols" "Number of columns" [ "Text across columns" ]
      [ "Local value for \\premulticols" ])
    '("multicols*" "Number of columns" [ "Text across columns" ]
      [ "Local value for \\premulticols" ]))

   (TeX-add-symbols
    '("multicoltolerance"    (TeX-arg-literal " = "))
    '("multicolpretolerance" (TeX-arg-literal " = "))
    "columnseprulecolor"
    '("raggedcolumns" 0)
    '("flushcolumns"  0)
    ;; 2.3 Manually breaking columns
    '("newcolumn"     0)
    '("columnbreak"   [ "How much [0 - 4]" ])

    ;; Preface to version 1.7
    "RLmulticolcolumns"
    "LRmulticolcolumns")

   ;; Preface to version 1.8
   (when (LaTeX-provided-package-options-member "multicol" "colaction")
     (TeX-add-symbols '("docolaction" 3)))

   (LaTeX-add-lengths "premulticols"
                      "postmulticols"
                      "multicolsep"
                      "multicolbaselineskip"
                      "multicolovershoot"
                      "multicolundershoot")

   (LaTeX-add-counters "collectmore")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("columnbreak"       "")
                                ("newcolumn"         "")
                                ("LRmulticolcolumns" "")
                                ("RLmulticolcolumns" ""))
                              'warning)))
 TeX-dialect)

(defvar LaTeX-multicol-package-options
  '("errorshow" "infoshow" "balancingshow" "markshow" "debugshow"
    "grid" "colaction")
  "Package options for the multicol package.")

;;; multicol.el ends here
