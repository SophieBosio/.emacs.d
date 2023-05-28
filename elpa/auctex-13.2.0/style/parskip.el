;;; parskip.el --- AUCTeX style for `parskip.sty' (v2.0h)  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-10-07
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

;; This file adds support for `parskip.sty' (v2.0h) from 2021/03/14.
;; `parskip.sty' is part of TeXLive.  Note that the parskip package
;; doesn't offer any document user commands and just needs loading
;; with \usepackage.  Hence this style provides only one function for
;; key=val query of package options.

;;; Code:

(require 'tex)
(require 'latex)

(defun LaTeX-parskip-package-options ()
  "Prompt for package options for the parskip package."
  (let ((len (mapcar (lambda (x)
                       (concat TeX-esc (car x)))
                     (LaTeX-length-list))))
    (TeX-read-key-val t `(("skip"    ,len)
                          ("tocskip" ,len)
                          ("indent"  ,len)
                          ("parfill" ,len)))))

;;; parskip.el ends here
