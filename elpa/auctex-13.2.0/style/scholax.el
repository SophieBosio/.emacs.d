;;; scholax.el --- AUCTeX style for `scholax.sty' (v1.027)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2020-11-29
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

;; This file adds support for `scholax.sty' (v1.027) from 2020/11/30.
;; `scholax.sty' is part of TeXLive.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "scholax"
 (lambda ()

   ;; Run style hook for various packages loaded by scholax
   (TeX-run-style-hooks "textcomp" "fontaxes")

   ;; New symbols
   (TeX-add-symbols

    ;; Only preamble commands
    '("useosf"  0)
    '("useproportional" 0)
    '("thfamily" 0)

    ;; Text commands
    '("textsu"     t)   ; superior figures
    '("sustyle"   -1)
    '("textin"     t)   ; inferior figures
    '("instyle"   -1)

    '("textlf"     t)   ; lining figures
    '("lfstyle"   -1)

    '("texttlf"    t)   ; tabular lining figures
    '("tlfstyle"  -1)

    '("textosf"    t)   ; oldstyle figures
    '("osfstyle"  -1)

    '("texttosf"   t)   ; tabular oldstyle figures
    '("tosfstyle" -1)

    '("textfrac"  "Numerator" "Denominator"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("textsu"    "{")
                                ("textin"    "{")
                                ("textlf"    "{")
                                ("texttlf"   "{")
                                ("textosf"   "{")
                                ("texttosf"  "{")
                                ("textfrac"  "{{"))
                              'type-command)
     (font-latex-add-keywords '(("sustyle"   "")
                                ("instyle"   "")
                                ("lfstyle"   "")
                                ("tlfstyle"  "")
                                ("osfstyle"  "")
                                ("tosfstyle" ""))
                              'type-declaration)))
 TeX-dialect)

(defvar LaTeX-scholax-package-options
  '("scale"       "scaled"
    "spacing"     "stretch"    "shrink"
    "foresolidus" "aftsolidus" "raisefrac"
    "theoremfont"
    "scosf"    "sups" "lining"  "lf"
    "oldstyle" "osf"  "tabular" "p" "proportional"
    "looser"   "loosest")
  "Package options for the scholax package.")

;;; scholax.el ends here
