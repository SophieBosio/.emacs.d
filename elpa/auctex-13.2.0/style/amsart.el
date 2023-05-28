;;; amsart.el --- Style hook for the AMS-LaTeX article document class.  -*- lexical-binding: t; -*-

;; Copyright (C) 1994-2022 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: auctex-devel@gnu.org
;; Created: 1994-01-05
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

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(declare-function font-latex-set-syntactic-keywords
                  "font-latex")

(defvar LaTeX-amsart-class-options
  '("a4paper" "letterpaper" "landscape" "portrait"
    "twoside" "oneside" "draft" "final"
    "8pt" "9pt" "10pt" "11pt" "12pt"
    "titlepage" "notitlepage" "onecolumn" "twocolumn"
    "leqno" "reqno" "centertags" "tbtags" "fleqn"
    "nomath" "noamsfonts" "psamsfonts")
  "Class options for the amsart class.")

(TeX-add-style-hook
 "amsart"
 (lambda ()

   ;; Load amsmath.el if the option nomath isn't given:
   (unless (LaTeX-provided-class-options-member "amsart" "nomath")
     (TeX-run-style-hooks "amsmath"))
   ;; Same for amsfonts.el:
   (unless (LaTeX-provided-class-options-member "amsart" "noamsfonts")
     (TeX-run-style-hooks "amsfonts"))
   ;; amsthm is built-in:
   (TeX-run-style-hooks "amsthm")

   (TeX-add-symbols
    '("address" 1)
    '("author" ["Short author(s)"] (LaTeX-arg-author "Long author(s)"))
    '("curraddr" 1)
    '("dedicatory" 1)
    '("email" 1)
    '("keywords" 1)
    '("subjclass" ["Year"] "List of subjects")
    '("title" ["Short Title"] "Title")
    '("urladdr" 1))

   (LaTeX-add-environments "abstract")

   (LaTeX-largest-level-set "section")
   (LaTeX-add-counters "part" "section" "subsection" "subsubsection"
                       "paragraph" "subparagraph"
                       "figure" "table")
   (LaTeX-add-pagestyles "headings" "myheadings")

   ;; Tell AUCTeX about \specialsection:
   (LaTeX-section-list-add-locally '("specialsection" 2))
   (LaTeX-paragraph-commands-add-locally "specialsection")
   (add-to-list (make-local-variable 'LaTeX-section-label)
                '("specialsection" . "sec:")
                t)

   ;; Tell RefTeX about \specialsection and append the entry to
   ;; `reftex-section-levels':
   (when (boundp 'reftex-section-levels)
     (add-to-list (make-local-variable 'reftex-section-levels)
                  '("specialsection" . 2)
                  t))

   ;; These macros will contain links etc., so treat the argument
   ;; verbatim:
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "email")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "urladdr")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("author"      "[{")
                                ("contrib"     "[{")
                                ("curraddr"    "{")
                                ("dedicatory"  "{")
                                ("keywords"    "{")
                                ("subjclass"   "[{")
                                ("title"       "[{")
                                ("email"       "")
                                ("urladdr"     ""))
                              'textual)
     (font-latex-add-keywords '(("specialsection" "{"))
                              'sectioning-2)
     (font-latex-set-syntactic-keywords)))
 TeX-dialect)

;;; amsart.el ends here.
