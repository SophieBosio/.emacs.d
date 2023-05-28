;;; wrapfig.el --- AUCTeX style for `wrapfig.sty' version v3.6  -*- lexical-binding: t; -*-

;; Copyright (C) 2014--2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-12-13
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

;; This file adds support for `wrapfig.sty' version v3.6 from
;; 2003/01/31.  `wrapfig.sty' is part of TeXLive.

;;; Code:

(declare-function LaTeX-newfloat-DeclareFloatingEnvironment-list
                  "newfloat" ())

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "wrapfig"
 (lambda ()
   (LaTeX-add-environments
    ;; \begin{wrapfigure}[No.lines]{Placement}[Overhang]{Width} ... \end{wrapfigure}
    '("wrapfigure" LaTeX-env-args
      ["Number of narrow lines"]
      (TeX-arg-completing-read ("r" "R" "l" "L" "i" "I" "o" "O")
                               "Placement")
      [TeX-arg-length "Overhang"]
      (TeX-arg-length "Width"))

    ;; \begin{wraptable}[No.lines]{Placement}[Overhang]{Width} ... \end{wraptable}
    '("wraptable" LaTeX-env-args
      ["Number of narrow lines"]
      (TeX-arg-completing-read ("r" "R" "l" "L" "i" "I" "o" "O")
                               "Placement")
      [TeX-arg-length "Overhang"]
      (TeX-arg-length "Width"))

    ;; \begin{wrapfloat}{<Type>}[No.lines]{Placement}[Overhang]{Width} ... \end{wrapfloat}
    ;;
    ;; <Type> can be a new floating environment defined with
    ;; "\DeclareFloatingEnvironment" from newfloat.el.  We check if
    ;; the function `LaTeX-newfloat-DeclareFloatingEnvironment-list'
    ;; is bound and returns non-nil before offering environment for
    ;; completion.  Otherwise, just ask user without completion.
    `("wrapfloat" LaTeX-env-args
      (TeX-arg-conditional (and (fboundp 'LaTeX-newfloat-DeclareFloatingEnvironment-list)
                                (LaTeX-newfloat-DeclareFloatingEnvironment-list))
          ((TeX-arg-completing-read
            ,(lambda ()
               (mapcar #'car (LaTeX-newfloat-DeclareFloatingEnvironment-list)))
            "Float type"))
        ("Float type"))
      ["Number of narrow lines"]
      (TeX-arg-completing-read ("r" "R" "l" "L" "i" "I" "o" "O")
                               "Placement")
      [TeX-arg-length "Overhang"]
      (TeX-arg-length "Width"))))
 TeX-dialect)

(defvar LaTeX-wrapfig-package-options '("verbose")
  "Package options for the wrapfig package.")

;;; wrapfig.el ends here
