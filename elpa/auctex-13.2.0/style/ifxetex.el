;;; ifxetex.el --- AUCTeX style for `ifxetex.sty' version 0.7.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-04-23
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

;; This file adds support for `ifxetex.sty' 0.7.  The package is now
;; part of 'iftex' bundle and therefore, the code is now in
;; 'iftex.el'.

;;; Code:

(require 'tex)

(TeX-add-style-hook
 "ifxetex"
 (lambda ()
   ;; Run the style hook for 'iftex' which does the work:
   (TeX-run-style-hooks "iftex"))
 TeX-dialect)

(defvar LaTeX-ifxetex-package-options nil
  "Package options for the ifxetex package.")

;;; ifxetex.el ends here
