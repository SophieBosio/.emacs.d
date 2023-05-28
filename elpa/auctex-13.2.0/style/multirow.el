;;; multirow.el --- AUCTeX style for `multirow.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2011, 2018--2022 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
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

;; This file adds support for `multirow.sty', v2.6 from 2021/01/02.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "multirow"
 (lambda ()
   (TeX-add-symbols
    ;; \multirow[<vpos>]{<nrows>}[<bigstruts>]{<width>}[<vmove>]{<text>}
    `("multirow"
      [TeX-arg-completing-read ("c" "b" "t") "Vertical position"]
      "Number of rows"
      [ "Big struts" ]
      (TeX-arg-completing-read
       ,(lambda ()
          (append '("*" "=")
                  (mapcar (lambda (x)
                            (concat TeX-esc (car x)))
                          (LaTeX-length-list))))
       "Width")
      [TeX-arg-length "Vertical fix-up"]
      t)
    "multirowsetup"
    "multirowdebugtrue"
    "multirowdebugfalse")

   ;; \bigstrutjot is a length defined both in multirow.sty and
   ;; bigstrut.sty.  It doesn't make a difference within AUCTeX since
   ;; dupes are removed by the function `LaTeX-length-list'.
   (LaTeX-add-lengths "bigstrutjot")

   ;; \STneed is only defined with package option `supertabular':
   (when (LaTeX-provided-package-options-member "multirow"
                                                "supertabular")
     (TeX-add-symbols
      '("STneed" TeX-arg-length)))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("multirow" "[{[{[{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-multirow-package-options '("debug"
                                         "longtable"
                                         "supertabular")
  "Package options for the multirow package.")

;;; multirow.el ends here
