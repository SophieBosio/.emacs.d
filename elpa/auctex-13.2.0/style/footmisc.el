;;; footmisc.el --- AUCTeX style for `footmisc.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2011, 2018--2022 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Created: 2011-04-08
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

;; This file adds support for `footmisc.sty' (v6.0b) from 2022/02/14.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-footmisc-fnsymbols-set '("bringhurst" "chicago" "wiley"
                                       "lamport" "lamport*")
  "Sets of footnote symbols provided by the footmisc package.")

(TeX-add-style-hook
 "footmisc"
 (lambda ()
   (TeX-add-symbols

    ;; 1.4 Option ragged and \footnotelayout
    "footnotelayout"

    ;; 1.7 The \setfnsymbol and \DefineFNsymbols commands
    '("DefineFNsymbols"
      (TeX-arg-completing-read LaTeX-footmisc-fnsymbols-set "Name")
      [TeX-arg-completing-read ("text" "math") "Style"]
      1)
    '("DefineFNsymbols*"
      (TeX-arg-completing-read LaTeX-footmisc-fnsymbols-set "Name")
      [TeX-arg-completing-read ("text" "math") "Style"]
      1)

    ;; These two commands define both text and math variants of the
    ;; footnote symbols
    '("DefineFNsymbolsTM"
      (TeX-arg-completing-read LaTeX-footmisc-fnsymbols-set "Name")
      1)
    '("DefineFNsymbolsTM*"
      (TeX-arg-completing-read LaTeX-footmisc-fnsymbols-set "Name")
      1)
    '("setfnsymbol"
      (TeX-arg-completing-read LaTeX-footmisc-fnsymbols-set "Name"))

    ;; 1.11 Option hang
    "hangfootparskip"
    "hangfootparindent"

    ;; 1.15 The multiple option
    "multiplefootnotemarker"
    "multfootsep"

    ;; 1.16 User interface
    "mpfootnotemark")

   ;; 1.9 Option marginal
   (LaTeX-add-lengths "footnotemargin")

   ;; 1.13 Option splitrule
   (when (LaTeX-provided-package-options-member "footmisc" "splitrule")
     (TeX-add-symbols "mpfootnoterule"
                      "pagefootnoterule"
                      "splitfootnoterule"))

   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("DefineFNsymbols"   "*{[{")
                                ("DefineFNsymbolsTM" "*{{")
                                ("setfnsymbol"       "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-footmisc-package-options '("perpage" "side" "ragged"
                                         "para" "symbol" "symbol*"
                                         "marginal" "flushmargin" "hang"
                                         "norule" "splitrule" "stable"
                                         "multiple"
                                         "abovefloats" "belowfloats"
                                         "bottom" "bottomfloats")
  "Package options for the footmisc package.")

;;; footmisc.el ends here
