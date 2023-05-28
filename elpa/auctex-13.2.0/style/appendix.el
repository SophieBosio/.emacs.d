;;; appendix.el --- AUCTeX style for `appendix.sty' (v1.2c)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2020-10-10
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

;; This file adds support for `appendix.sty' version 1.2c from
;; 2020/02/08.

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "appendix"
 (lambda ()

   (TeX-add-symbols
    "appendixpage"
    "addappheadtotoc"
    "noappendicestocpagenum"
    "appendicestocpagenum"
    "appendixname"
    "appendixtocname"
    "appendixpagename"

    "appendixtocon"
    "appendixtocoff"
    "appendixpageon"
    "appendixpageoff"
    "appendixtitleon"
    "appendixtitleoff"
    "appendixtitletocon"
    "appendixtitletocoff"
    "appendixheaderon"
    "appendixheaderoff"
    "restoreapp"

    "setthesection"
    "setthesubsection")

   (LaTeX-add-environments
    '("appendices")
    '("subappendices"))

   ;; Don't indent the content inside \(sub\)?appendices environments:
   (unless (string-match "appendices" LaTeX-document-regexp)
     (set (make-local-variable 'LaTeX-document-regexp)
          (concat LaTeX-document-regexp "\\|\\(?:sub\\)?appendices"))))
 TeX-dialect)

(defvar LaTeX-appendix-package-options '("toc" "page" "title"
                                         "titletoc" "header")
  "Package options for the appendix package.")

;;; appendix.el ends here
