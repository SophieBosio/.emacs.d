;;; fbox.el --- AUCTeX style for `fbox.sty' (v0.06)  -*- lexical-binding: t; -*-

;; Copyright (C) 2019--2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2019-11-08
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

;; This file adds support for `fbox.sty' (v0.06) from 2022/02/20.
;; `fbox.sty' is part of TeXLive.

;;; Code

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(declare-function LaTeX-xcolor-definecolor-list "xcolor" ())

(defun LaTeX-fbox-key-val-options ()
  "Return an updated list of key=vals from fbox package."
  (let ((colors (mapcar #'car (LaTeX-xcolor-definecolor-list)))
        (lenghts (mapcar (lambda (x)
                           (concat TeX-esc (car x)))
                         (LaTeX-length-list))))
    `(("boxrule" ,lenghts)
      ("boxsep"  ,lenghts)
      ("lcolor"  ,colors)
      ("rcolor"  ,colors)
      ("bcolor"  ,colors)
      ("tcolor"  ,colors)
      ("l")
      ("r")
      ("b")
      ("t"))))

(TeX-add-style-hook
 "fbox"
 (lambda ()
   (TeX-run-style-hooks "xcolor")
   (TeX-add-symbols
    '("fbox"     [TeX-arg-key-val (LaTeX-fbox-key-val-options)] t)
    '("fbox*"    [TeX-arg-key-val (LaTeX-fbox-key-val-options)] t)
    '("fparbox"  [TeX-arg-key-val (LaTeX-fbox-key-val-options)] t)
    '("fparbox*" [TeX-arg-key-val (LaTeX-fbox-key-val-options)] t))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fbox"    "*[{")
                                ("fparbox" "*[{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-fbox-package-options nil
  "Package options for the fbox package.")

;;; fbox.el ends here
