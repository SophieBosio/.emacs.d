;;; algpseudocode.el --- AUCTeX style for the (LaTeX) algpseudocode package  -*- lexical-binding: t; -*-

;; Copyright (C) 2020--2022 Free Software Foundation, Inc.

;; Author: Uwe Brauer <oub@mat.ucm.es>
;; Created: 2020-01-26
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

;; This file adds support for the algpseudocode package.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-algpseudocode-package-options
  '("compatible" "nocompatible" "end" "noend")
  "Package options for the algpseudocode package.")

(TeX-add-style-hook
 "algpseudocode"
 (lambda ()
   (TeX-add-symbols
    ;; 2.3 Simple lines
    '("State"  (TeX-arg-literal " "))
    '("Statex" 0)

    ;; 2.4 Placing comments in sources
    '("Comment" 1)

    ;; 2.5 Labels and references
    '("algref" (TeX-arg-ref "Algorithm") (TeX-arg-ref "Line"))

    ;; 2.6 Breaking up long algorithms
    '("algstore" 1)
    '("algstore*" 1)
    '("algrestore" 1)
    '("algrestore*" 1)

    ;; 3.1.1 The for block
    '("For" 1)
    '("ForAll" 1)
    '("EndFor" 0)

    ;; 3.1.2 The while block
    '("While" 1)
    '("EndWhile" 0)

    ;; 3.1.3 The repeat block
    '("Repeat" 0)
    '("Until" 1)

    ;; 3.1.4 The if block
    '("If" 1)
    '("ElsIf" 1)
    '("Else" 0)
    '("EndIf" 0)

    ;; 3.1.5 The procedure block
    '("Procedure" 2)
    '("EndProcedure" 0)

    ;; 3.1.6 The function block
    '("Function" 2)
    '("EndFunction" 0)

    ;; 3.1.7 The loop block
    '("Loop" 0)
    '("EndLoop" 0)

    ;; 3.1.8 Other commands in this layout
    '("Require" (TeX-arg-literal " "))
    '("Ensure"  (TeX-arg-literal " "))
    '("Call" 2))

   (LaTeX-add-environments
    '("algorithmic" [ "Number" ]))

   ;; Indentation: Add the keywords above to the respective variables
   ;; and run `LaTeX-indent-commands-regexp-make'.
   (let ((beg '("For" "ForAll"
                "While"
                "Repeat"
                "If"
                "Procedure"
                "Function"
                "Loop"))
         (mid '("ElsIf" "Else"))
         (end '("EndFor"
                "EndWhile"
                "Until"
                "EndIf"
                "EndProcedure"
                "EndFunction"
                "EndLoop")))
     (dolist (elt beg)
       (add-to-list 'LaTeX-indent-begin-list elt t))
     (dolist (elt mid)
       (add-to-list 'LaTeX-indent-mid-list elt t))
     (dolist (elt end)
       (add-to-list 'LaTeX-indent-end-list elt t))
     (LaTeX-indent-commands-regexp-make))

   ;; Add the 'algorithmic' environment to a local version of
   ;; `LaTeX-indent-environment-list'.  This effectively kills filling
   ;; but indenting works as expected.  Hence, 'M-q' gives a better
   ;; experience.
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
                '("algorithmic")
                t)

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("algref" "{{"))
                              'reference)))
 TeX-dialect)

;;; algpseudocode.el ends here
