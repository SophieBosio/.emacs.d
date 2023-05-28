;;; brazilian.el --- Setup AUCTeX for editing Brazilian text.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-05-02
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

;; Cater for some specialities of Brazilian and Portuguese languages
;; provided by babel package, e.g. special quote and hyphen strings or
;; `"' which is an active character.

;; Thanks to Gustavo Barros <gusbrs.2016@gmail.com> for requesting
;; this feature and reviewing the code.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-quotes
                  "font-latex"
                  (quotes))

(declare-function font-latex-add-to-syntax-alist
                  "font-latex"
                  (list))

(defvar LaTeX-brazilian-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `brazilian' language.")

(modify-syntax-entry ?\" "w" LaTeX-brazilian-mode-syntax-table)

(TeX-add-style-hook
 "brazilian"
 (lambda ()
   (set-syntax-table LaTeX-brazilian-mode-syntax-table)
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language nil))
   (setq LaTeX-babel-hyphen-language "brazilian")
   (TeX-add-symbols
    "ord"
    "ro"
    "orda"
    "ra")
   ;; Fontification
   (when (and (eq TeX-install-font-lock 'font-latex-setup)
              (featurep 'font-latex))
     (font-latex-add-quotes '("\"<" "\">" french))
     ;; Prevent "| from leading to color bleed.
     (font-latex-add-to-syntax-alist (list (cons ?\" "\\"))))
   (run-hooks 'TeX-language-pt-br-hook))
 TeX-dialect)

;;; brazilian.el ends here
