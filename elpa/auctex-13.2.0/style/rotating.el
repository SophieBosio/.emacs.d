;;; rotating.el --- AUCTeX style for `rotating.sty' (v2.16d)  -*- lexical-binding: t; -*-

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

;; This file adds support for `rotating.sty' (v2.16d) from 2016/08/11.
;; `rotating.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(defvar LaTeX-graphicx-package-options)

(TeX-add-style-hook
 "rotating"
 (lambda ()

   ;; Environments provided by the package:
   (LaTeX-add-environments
    '("sidewaysfigure"  LaTeX-env-figure)
    '("sidewaysfigure*" LaTeX-env-figure)
    '("sidewaystable"   LaTeX-env-figure)
    '("sidewaystable*"  LaTeX-env-figure)
    "sideways"
    '("turn"   "Angle")
    '("rotate" "Angle"))

   ;; Add the float environments to `LaTeX-label-alist':
   (dolist (env '("sidewaysfigure" "sidewaysfigure*"))
     (add-to-list 'LaTeX-label-alist `(,env . LaTeX-figure-label) t))

   (dolist (env '("sidewaystable" "sidewaystable*"))
     (add-to-list 'LaTeX-label-alist `(,env . LaTeX-table-label) t))

   ;; The skips:
   (LaTeX-add-lengths "rotFPtop" "rotFPbot")

   ;; New symbols
   (TeX-add-symbols
    '("turnbox" ["Angle"] "Argument"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("turnbox"   "[{"))
                              'textual)))
 TeX-dialect)

(defvar LaTeX-rotating-package-options
  (progn
    (TeX-load-style "graphicx")
    (append LaTeX-graphicx-package-options
            '("clockwise"   "counterclockwise" "anticlockwise"
              "figuresleft" "figuresright"
              "quiet" "log" "chatter")))
  "Package options for the rotating package.")

;;; rotating.el ends here
