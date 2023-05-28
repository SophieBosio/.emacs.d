;;; iftex.el --- AUCTeX style for `iftex.sty' version 1.0f  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-04-17
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

;; This file adds support for `iftex.sty' v1.0f from 2022/02/03.
;; `iftex.sty' is part of TeXlive.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defun LaTeX-iftex-set-TeX-exit-mark (_optional)
  "Ignore OPTIONAL and set `TeX-exit-mark' to current point."
  (set-marker TeX-exit-mark (point)))

(TeX-add-style-hook
 "iftex"
 (lambda ()

   (TeX-add-symbols
    '("ifetex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifeTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifpdftex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifPDFTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifxetex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifXeTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifluatex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifLuaTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifluahbtex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifLuaHBTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifptex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifpTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifuptex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifupTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifptexng"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifpTeXng"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifvtex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifVTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifalephtex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifAlephTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("iftutex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifTUTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("iftexpadtex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifTexpadTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("iftex"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifTeX"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    '("ifhint"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))
    '("ifHINT"
      LaTeX-iftex-set-TeX-exit-mark
      (TeX-arg-literal "\\else\\fi"))

    "RequireeTeX"
    "RequirePDFTeX"
    "RequireXeTeX"
    "RequireLuaTeX"
    "RequireLuaHBTeX"
    "RequirepTeX"
    "RequireupTeX"
    "RequirepTeXng"
    "RequireVTeX"
    "RequireAlephTeX"
    "RequireTUTeX"
    "RequireTexpadTeX"
    "RequireHINT")

   ;; This package is used to make it possible to compile a document
   ;; with different TeX engines.  By setting `TeX-check-engine-list'
   ;; to nil we ignore engine restrictions posed by other packages.
   (TeX-check-engine-add-engines nil)

   ;; Fontification:
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("RequireeTeX"      "")
                                ("RequirePDFTeX"    "")
                                ("RequireXeTeX"     "")
                                ("RequireLuaTeX"    "")
                                ("RequireLuaHBTeX"  "")
                                ("RequirepTeX"      "")
                                ("RequireupTeX"     "")
                                ("RequirepTeXng"    "")
                                ("RequireVTeX"      "")
                                ("RequireAlephTeX"  "")
                                ("RequireTUTeX"     "")
                                ("RequireTexpadTeX" "")
                                ("RequireHINT"      ""))
                              'function)))
 TeX-dialect)

(defvar LaTeX-iftex-package-options nil
  "Package options for the iftex package.")

;;; iftex.el ends here
