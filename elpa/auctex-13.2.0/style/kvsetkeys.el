;;; kvsetkeys.el --- AUCTeX style for `kvsetkeys.sty' version 1.18.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-05-29
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

;; This file adds support for `kvsetkeys.sty' 1.18 from 2019-12-15.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "kvsetkeys"
 (lambda ()
   (TeX-add-symbols
    '("kvsetkeys" "Family" t)
    '("kvsetknownkeys" "Family" 2))

   ;; Fontification: Don't fontify arguments which contain code
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("kvsetkeys"      "{{")
                                ("kvsetknownkeys" "{{{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-kvsetkeys-package-options nil
  "Package options for the kvsetkeys package.")

;;; kvsetkeys.el ends here
