;;; cancel.el --- AUCTeX style for `cancel.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-11-24
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

;; This file adds support for `cancel.sty' v2.2 form 2013-04-12.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "cancel"
 (lambda ()
   (TeX-add-symbols
    '("cancel"   "Expression")
    '("bcancel"  "Expression")
    '("xcancel"  "Expression")
    '("cancelto" "Value" "Expression")
    "CancelColor")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("cancel"   "|{\\")
                                ("bcancel"  "|{\\")
                                ("xcancel"  "|{\\"))
                              'textual)))
 TeX-dialect)

(defvar LaTeX-cancel-package-options
  '("thicklines" "samesize" "smaller" "Smaller")
  "Package options for the cancel package.")

;;; cancel.el ends here
