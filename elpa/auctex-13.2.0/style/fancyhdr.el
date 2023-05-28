;;; fancyhdr.el --- AUCTeX style for `fancyhdr.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2013, 2018-2022 Free Software Foundation, Inc.

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

;; This file adds support for `fancyhdr.sty', v4.0 from 2021/01/04.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-fancyhdr-fancypagestyle-regexp
  '("\\\\fancypagestyle{\\([^}]+\\)}"
    1 LaTeX-auto-pagestyle)
  "Regexp matching the first argument of \\fancypagestyle macro.")

(TeX-add-style-hook
 "fancyhdr"
 (lambda ()
   (TeX-add-symbols

    ;; 2 Using fancyhdr
    '("fancyhead" [TeX-arg-completing-read-multiple
                   ("L" "LO" "LE" "C" "CO" "CE" "R" "RE" "RO")
                   "Places"]
      t)
    '("fancyfoot" [TeX-arg-completing-read-multiple
                   ("L" "LO" "LE" "C" "CO" "CE" "R" "RE" "RO")
                   "Places"]
      t)
    '("fancyhf"   [TeX-arg-completing-read-multiple
                   ("L" "LO" "LE" "LOH" "LOF" "LEH" "LEF"
                    "C" "CO" "CE" "COH" "COF" "CEH" "CEF"
                    "R" "RO" "RE" "ROH" "ROF" "REH" "REF")
                   "Places"]
      t)

    '("fancyheadoffset"
      [TeX-arg-completing-read-multiple ("L" "LO" "LE" "R" "RO" "RE") "Places"]
      TeX-arg-length)
    '("fancyfootoffset"
      [TeX-arg-completing-read-multiple ("L" "LO" "LE" "R" "RO" "RE") "Places"]
      TeX-arg-length)
    '("fancyhfoffset"
      [TeX-arg-completing-read-multiple ("L" "LO" "LE" "LOH" "LOF" "LEH" "LEF"
                                         "R" "RO" "RE" "ROH" "ROF" "REH" "REF")
                                        "Places"]
      TeX-arg-length)

    "headrulewidth" "footrulewidth"
    "headruleskip"  "footruleskip"
    "headrule"      "footrule"
    "headwidth"

    '("fancyheadinit" t)
    '("fancyfootinit" t)
    '("fancyhfinit"   t)

    '("fancycenter"
      [TeX-arg-length "Distance"] [ "Stretch" ] 3)

    '("iftopfloat"  2)
    '("ifbotfloat"  2)
    '("iffloatpage" 2)
    '("iffootnote"  2)

    '("fancypagestyle"
      ;; Always add the chosen pagestyle to list of known pagestyles,
      ;; dupes are removed when retrieving with the function
      ;; `LaTeX-pagestyle-list':
      (TeX-arg-pagestyle nil t)
      [TeX-arg-pagestyle "Base pagestyle"]
      t)

    ;; 15 The scoop on LATEX’s marks
    '("nouppercase" t))

   ;; Don't increase indentation at various \if* macros:
   (let ((exceptions '("iftopfloat"
                       "ifbotfloat"
                       "iffloatpage"
                       "iffootnote")))
     (dolist (elt exceptions)
       (add-to-list 'LaTeX-indent-begin-exceptions-list elt t))
     (LaTeX-indent-commands-regexp-make))

   ;; 30 Deprecated commands
   ;; Don't offer deprecated commands in V4.0 for completion anymore.
   ;; '("lhead" t)
   ;; '("lfoot" t)
   ;; '("chead" t)
   ;; '("cfoot" t)
   ;; '("rhead" t)
   ;; '("rfoot" t)
   ;; "plainfootrulewidth"
   ;; "plainheadrulewidth"

   ;; `fancyhdr.sty' supplies these two pagestyles.  Pagestyle
   ;; `fancyplain' is now deprecated.
   (LaTeX-add-pagestyles "fancy" "fancydefault")

   ;; Add \fancypagestyle{pagestyle} to AUCTeX parser
   (TeX-auto-add-regexp LaTeX-fancyhdr-fancypagestyle-regexp)

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fancyhead" "[{")
                                ("fancyfoot" "[{")
                                ("fancyhf"   "[{")
                                ("fancyheadoffset" "[{")
                                ("fancyfootoffset" "[{")
                                ("fancyhfoffset"   "[{")
                                ("fancyheadinit"   "{")
                                ("fancyfootinit"   "{")
                                ("fancyhfinit"     "{")
                                ;; Fontify deprecated commands for
                                ;; older documents; to be removed
                                ;; sometimes ...
                                ("lhead" "[{")
                                ("lfoot" "[{")
                                ("chead" "[{")
                                ("cfoot" "[{")
                                ("rhead" "[{")
                                ("rfoot" "[{")
                                ;; Don't fontify the last argument;
                                ;; all macros used there should have
                                ;; their own fontification since they
                                ;; can also be used in a document
                                ;; top-level.
                                ("fancypagestyle"  "{["))
                              'function)))
 TeX-dialect)

(defvar LaTeX-fancyhdr-package-options
  '("nocheck" "compatV3" "headings" "myheadings")
  "Package options for fancyhdr package.")

;;; fancyhdr.el ends here
