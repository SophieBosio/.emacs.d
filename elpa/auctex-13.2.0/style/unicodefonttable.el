;;; unicodefonttable.el --- AUCTeX style for `unicodefonttable.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-10-23
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
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary:

;; This file adds support for `unicodefonttable.sty' v1.0f form
;; 2021-10-29.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(declare-function LaTeX-xcolor-definecolor-list "xcolor" ())

(defvar LaTeX-unicodefonttable-key-val-options
  '(("header"          ("true" "false"))
    ("noheader"        ("true" "false"))
    ("title-format")
    ("title-format-cont")
    ("display-block"   ("titles" "rules" "none"))
    ("hex-digits"      ("block" "foot" "head" "head+foot" "none"))
    ("hex-digits-font" ("\\rmfamily" "\\sffamily"   "\\ttfamily"
                        "\\mdseries" "\\bfseries"   "\\upshape"
                        "\\itshape"  "\\slshape"    "\\scshape"
                        "\\tiny"     "\\scriptsize" "\\footnotesize"
                        "\\small"    "\\normalsize" "\\large"
                        "\\Large"    "\\LARGE"  "\\huge" "\\Huge"
                        "\\normalfont"))
    ("color")
    ("statistics"      ("true" "false"))
    ("nostatistics"    ("true" "false"))
    ("statistics-font" ("\\rmfamily" "\\sffamily"   "\\ttfamily"
                        "\\mdseries" "\\bfseries"   "\\upshape"
                        "\\itshape"  "\\slshape"    "\\scshape"
                        "\\tiny"     "\\scriptsize" "\\footnotesize"
                        "\\small"    "\\normalsize" "\\large"
                        "\\Large"    "\\LARGE"  "\\huge" "\\Huge"
                        "\\normalfont"))
    ("statistics-format")
    ("glyph-width")
    ("missing-glyph")
    ("missing-glyph-font" ("\\rmfamily" "\\sffamily"   "\\ttfamily"
                           "\\mdseries" "\\bfseries"   "\\upshape"
                           "\\itshape"  "\\slshape"    "\\scshape"
                           "\\tiny"     "\\scriptsize" "\\footnotesize"
                           "\\small"    "\\normalsize" "\\large"
                           "\\Large"    "\\LARGE"  "\\huge" "\\Huge"
                           "\\normalfont"))
    ("missing-glyph-color")
    ("compare-with")
    ("compare-color")
    ("compare-bgcolor")
    ("statistics-compare-format")
    ("range-start")
    ("range-end"))
  "Key=value options for unicodefonttable macros.")

(defun LaTeX-unicodefonttable-key-val-options ()
  "Return an updated list of key=vals from unicodefonttable package."
  (append
   (let ((colors (mapcar #'car (LaTeX-xcolor-definecolor-list)))
         (keys '("color"
                 "missing-glyph-color"
                 "compare-color"
                 "compare-bgcolor"))
         result)
     (dolist (key keys result)
       (push (list key colors) result)))
   LaTeX-unicodefonttable-key-val-options))

(TeX-add-style-hook
 "unicodefonttable"
 (lambda ()

   (TeX-run-style-hooks "fontspec" "xcolor" "caption"
                        "longtable" "booktabs")

   (TeX-add-symbols
    '("displayfonttable"
      [TeX-arg-key-val (LaTeX-unicodefonttable-key-val-options)]
      LaTeX-fontspec-arg-font
      [TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"])

    '("displayfonttable*"
      [TeX-arg-key-val (LaTeX-unicodefonttable-key-val-options)]
      LaTeX-fontspec-arg-font
      [TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"])

    '("fonttablesetup"
      (TeX-arg-key-val (LaTeX-unicodefonttable-key-val-options)))

    "fonttableglyphcount")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("displayfonttable"     "[{[")
                                ("displayfonttable*"    "[{[")
                                ("fonttableglyphcount"   ""))
                              'function)))
 TeX-dialect)

(defvar LaTeX-unicodefonttable-package-options nil
  "Package options for the unicodefonttable package.")

;;; unicodefonttable.el ends here
