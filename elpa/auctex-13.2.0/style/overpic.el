;;; overpic.el --- AUCTeX style for `overpic.sty' (v1.3)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020--2023 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2020-02-23
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

;; This file adds support for `overpic.sty' (v1.3) from 2020/02/22.
;; `overpic.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(declare-function LaTeX-graphicx-key-val-options
                  "graphicx" ())
(defvar LaTeX-graphicx-package-options)

(defvar LaTeX-overpic-key-val-options
  '(("abs"     ("true" "false"))
    ("percent" ("true" "false"))
    ("permil"  ("true" "false"))
    ("rel")
    ("grid"    ("true" "false"))
    ("tics")
    ("unit"))
  "Key=value options for overpic macro and environments.")

(TeX-add-style-hook
 "overpic"
 (lambda ()

   ;; overpic.sty loads graphicx.sty
   (TeX-run-style-hooks "graphicx")

   (TeX-add-symbols
    '("setOverpic" (TeX-arg-key-val LaTeX-overpic-key-val-options)))

   (LaTeX-add-environments
    `("overpic" LaTeX-env-args
      [TeX-arg-key-val ,(lambda ()
                          (append (LaTeX-graphicx-key-val-options)
                                  LaTeX-overpic-key-val-options))
                       nil nil ?\s]
      LaTeX-arg-includegraphics)

    `("Overpic" LaTeX-env-args
      [TeX-arg-key-val ,(lambda ()
                          (append (LaTeX-graphicx-key-val-options)
                                  LaTeX-overpic-key-val-options))
                       nil nil ?\s]
      (TeX-arg-literal "{" "}")
      ,(lambda (_optional)
         (set-marker TeX-exit-mark (1- (point))))))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("setOverpic" "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-overpic-package-options
  (progn
    (TeX-load-style "graphicx")
    (append
     LaTeX-graphicx-package-options
     '("abs"
       "percent"
       "permil")))
  "Package options for the overpic package.")

;;; overpic.el ends here
