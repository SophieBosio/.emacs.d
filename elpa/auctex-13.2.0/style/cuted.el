;;; cuted.el --- AUCTeX style for `cuted.sty' (v2.0)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

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

;; This file adds support for `cuted.sty' (v2.0) from 2021/10/04.
;; `cuted.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "cuted"
 (lambda ()

   ;; Add the only environment provided by the package:
   (LaTeX-add-environments "strip")

   ;; This is a glue, in LaTeX set with \setlength:
   (LaTeX-add-lengths "stripsep")

   ;; New symbols
   (TeX-add-symbols
    '("preCutedStrip"  t)
    '("postCutedStrip" t)
    '("oldcolsbreak"   t))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("preCutedStrip"  "{")
                                ("postCutedStrip" "{")
                                ("oldcolsbreak"   "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-cuted-package-options
  '("spread"  "nospread"  "shrink"  "noshrink"
    "lspread" "nolspread" "lshrink" "nolshrink"
    "rspread" "norspread" "rshrink" "norshrink"
    "debug"   "nodebug")
  "Package options for the cuted package.")

;;; cuted.el ends here
