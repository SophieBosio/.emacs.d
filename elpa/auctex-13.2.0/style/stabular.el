;;; stabular.el --- AUCTeX style for `stabular.sty' (v2.1)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-12-11
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

;; This file adds support for `stabular.sty' (v2.1) from 2021/10/04.
;; `stabular.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "stabular"
 (lambda ()

   ;; Load array.el if the package option is given:
   (when (LaTeX-provided-package-options-member "stabular" "array")
     (TeX-run-style-hooks "array"))

   ;; Add the environments provided by the package:
   (LaTeX-add-environments
    '("stabular" LaTeX-env-array)
    '("stabular*" LaTeX-env-tabular*))

   ;; Use the enhanced tabular indentation.  Append to
   ;; `LaTeX-indent-environment-list' in order not to override custom
   ;; settings.
   (make-local-variable 'LaTeX-indent-environment-list)
   (add-to-list 'LaTeX-indent-environment-list
                '("stabular" LaTeX-indent-tabular)
                t)
   (add-to-list 'LaTeX-indent-environment-list
                '("stabular*" LaTeX-indent-tabular)
                t)

   ;; Append the environments to `LaTeX-item-list':
   (add-to-list 'LaTeX-item-list
                '("stabular" . LaTeX-item-array)
                t)
   (add-to-list 'LaTeX-item-list
                '("stabular*" . LaTeX-item-tabular*)
                t))

 TeX-dialect)

(defvar LaTeX-stabular-package-options '("array")
  "Package options for the stabular package.")

;;; stabular.el ends here
