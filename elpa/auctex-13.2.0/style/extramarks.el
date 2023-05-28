;;; extramarks.el --- AUCTeX style for `extramarks.sty' (v4.0)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-01-12
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

;; This file adds support for `extramarks.sty' (v4.0) from 2021/01/04.
;; `extramarks.sty' is part of TeXLive.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "extramarks"
 (lambda ()

   ;; New symbols
   (TeX-add-symbols
    "firstleftmark"
    "lastrightmark"
    "firstrightmark"
    "lastleftmark"
    "firstleftxmark"
    "firstrightxmark"
    "topleftxmark"
    "toprightxmark"
    "lastleftxmark"
    "lastrightxmark"
    "firstxmark"
    "lastxmark"
    "topxmark"
    '("extramarks" 2))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("extramarks" "{{"))
                              'reference)))
 TeX-dialect)

(defvar LaTeX-extramarks-package-options nil
  "Package options for the extramarks package.")

;;; extramarks.el ends here
