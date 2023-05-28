;;; midfloat.el --- AUCTeX style for `midfloat.sty' (v1.1)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

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

;; This file adds support for `midfloat.sty' (v1.1) from 2012/05/29.
;; `midfloat.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "midfloat"
 (lambda ()

   ;; Add the only environment provided by the package:
   (LaTeX-add-environments
    '("strip" ["Top/Bottom skip"]))

   ;; This is a glue, in LaTeX set with \setlength:
   (LaTeX-add-lengths "stripsep"))
 TeX-dialect)

(defvar LaTeX-midfloat-package-options nil
  "Package options for the midfloat package.")

;;; midfloat.el ends here
