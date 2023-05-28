;;; bigdelim.el --- AUCTeX style for `bigdelim.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2011--2022 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2011-01-24
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

;; This file adds support for `bigdelim.sty', v2.6 from 2021/01/02.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defun TeX-arg-bigdelim-brace (optional side &optional prompt)
  "Prompt for a single brace, and do not insert the matching one.
If OPTIONAL is non-nil, include the argument only if not empty.
SIDE is one of the symbols `left' or `right'. PROMPT replaces the
standard one."
  (let* ((brace (completing-read
                 (TeX-argument-prompt optional prompt "Brace")
                 (if (eq side 'left)
                     '("(" "[" "{" "\\langle" "|" "\\|" "\\lceil" "\\lfloor")
                   '(")" "]" "}" "\\rangle" "|" "\\|" "\\rceil" "\\rfloor"))))
         (TeX-arg-opening-brace (if (member (substring brace 0 1)
                                            `("{" "}" ,TeX-esc))
                                    ""
                                  TeX-grop))
         (TeX-arg-closing-brace (if (string= TeX-arg-opening-brace TeX-grop)
                                    TeX-grcl
                                  "")))
    (TeX-argument-insert brace optional (when (member brace '("{" "}"))
                                          TeX-esc))))

(TeX-add-style-hook
 "bigdelim"
 (lambda ()

   (TeX-run-style-hooks "multirow")

   (TeX-add-symbols
    `("ldelim"
      (TeX-arg-bigdelim-brace left)
      "Number of rows for multirow"
      (TeX-arg-completing-read
       ,(lambda () (append '("*")
                           (mapcar (lambda (x)
                                     (concat TeX-esc (car x)))
                                   (LaTeX-length-list))))
       "Width in multirow")
      [ "Text in multirow" ])
    `("rdelim"
      (TeX-arg-bigdelim-brace right)
      "Number of rows for multirow"
      (TeX-arg-completing-read
       ,(lambda () (append '("*")
                           (mapcar (lambda (x)
                                     (concat TeX-esc (car x)))
                                   (LaTeX-length-list))))
       "Width in multirow")
      [ "Text in multirow" ]))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("ldelim" "|{\\{{[")
                                ("rdelim" "|{\\{{["))
                              'function)))
 TeX-dialect)

(defvar LaTeX-bigdelim-package-options nil
  "Package options for the bigdelim package.")

;;; bigdelim.el ends here
