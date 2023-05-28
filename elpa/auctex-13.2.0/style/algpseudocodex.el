;;; algpseudocodex.el --- AUCTeX style for `algpseudocodex.sty' (v1.0.2)  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Created: 2022-10-10
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

;; This file adds support for the `algpseudocodex.sty' (v1.0.2) from
;; 2022-10-07.  `algpseudocodex.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defun LaTeX-arg-algpseudocodex-block (_optional block &optional num)
  "Insert the arguments of blocks from algpseudocodex package.
OPTIONAL is ignored.  BLOCK is the name of the block which also
determines the macro inserted for the block end.  NUM determines
the number of arguments."
  (cond ((string= block "Repeat")
         (indent-according-to-mode)
         (LaTeX-newline)
         (indent-according-to-mode)
         (LaTeX-newline)
         (insert TeX-esc "Until" TeX-grop)
         (set-marker TeX-exit-mark (point))
         (insert TeX-grcl)
         (indent-according-to-mode))
        ((integerp num)
         (indent-according-to-mode)
         (insert TeX-grop)
         (set-marker TeX-exit-mark (point))
         (insert TeX-grcl)
         (when (= num 2)
           (insert TeX-grop TeX-grcl))
         (LaTeX-newline)
         (indent-according-to-mode)
         (LaTeX-newline)
         (insert TeX-esc "End" block)
         (indent-according-to-mode))
        (t
         (indent-according-to-mode)
         (LaTeX-newline)
         (indent-according-to-mode)
         (set-marker TeX-exit-mark (point))
         (LaTeX-newline)
         (insert TeX-esc "End" block)
         (indent-according-to-mode))))

(TeX-add-style-hook
 "algpseudocodex"
 (lambda ()

   ;; 1.1 Algorithmic Block
   (LaTeX-add-environments
    '("algorithmic" [ "Number" ]))

   (TeX-add-symbols
    ;; 1.2 Simple Statements and Commands
    '("State"  (TeX-arg-literal " "))
    '("Statex" (TeX-arg-literal " "))
    '("Return" (TeX-arg-literal " "))
    '("Output" (TeX-arg-literal " "))
    '("Call"   2)

    ;; 1.3.1 While Loop
    '("While" (LaTeX-arg-algpseudocodex-block "While" 1))
    '("EndWhile" 0)

    ;; 1.3.2 For Loop
    '("For" (LaTeX-arg-algpseudocodex-block "For" 1))
    '("EndFor" 0)

    ;; 1.3.3 For-All Loop
    '("ForAll" (LaTeX-arg-algpseudocodex-block "For" 1))

    ;; 1.3.4 Loop
    '("Loop" (LaTeX-arg-algpseudocodex-block "Loop"))
    '("EndLoop" 0)

    ;; 1.3.5 Repeat-Until Loop
    '("Repeat" (LaTeX-arg-algpseudocodex-block "Repeat"))
    '("Until" 1)

    ;; 1.3.6 If Statement
    '("If" (LaTeX-arg-algpseudocodex-block "If" 1))
    '("ElsIf" 1)
    '("Else"  0)
    '("EndIf" 0)

    ;; 1.3.7 Procedure
    '("Procedure" (LaTeX-arg-algpseudocodex-block "Procedure" 2))
    '("EndProcedure" 0)

    ;; 1.3.8 Function
    '("Function" (LaTeX-arg-algpseudocodex-block "Function" 2))
    '("EndFunction" 0)

    ;; 1.4 Require and Ensure
    '("Require" (TeX-arg-literal " "))
    '("Ensure"  (TeX-arg-literal " "))

    ;; 1.5 Comments
    '("Comment"  1)
    '("LComment" 1)

    ;; 2.1 Boxes Around Multiple Lines of Code
    '("BeginBox" (LaTeX-arg-algpseudocodex-block "Box"))

    ;; 2.2 Boxes Inside Single Line
    '("BoxedString" ["options"] t)

    '("algrenewcommand"
      (TeX-arg-completing-read ("algorithmicend"
                                "algorithmicdo"
                                "algorithmicwhile"
                                "algorithmicfor"
                                "algorithmicforall"
                                "algorithmicloop"
                                "algorithmicrepeat"
                                "algorithmicuntil"
                                "algorithmicprocedure"
                                "algorithmicfunction"
                                "algorithmicif"
                                "algorithmicthen"
                                "algorithmicelse"
                                "algorithmicrequire"
                                "algorithmicensure"
                                "algorithmicreturn"
                                "algorithmicoutput")
                               "Macro (cr): \\" t "\\")
      t))

   ;; Indentation: Add the keywords above to the respective variables
   ;; and run `LaTeX-indent-commands-regexp-make'.
   (unless (member "BeginBox" LaTeX-indent-begin-list)
     (let ((beg '("For" "ForAll"
                  "While"
                  "Repeat"
                  "If"
                  "Procedure"
                  "Function"
                  "Loop"
                  "BeginBox"))
           (mid '("ElsIf" "Else"))
           (end '("EndFor"
                  "EndWhile"
                  "Until"
                  "EndIf"
                  "EndProcedure"
                  "EndFunction"
                  "EndLoop"
                  "EndBox")))
       (dolist (elt beg)
         (add-to-list 'LaTeX-indent-begin-list elt t))
       (dolist (elt mid)
         (add-to-list 'LaTeX-indent-mid-list elt t))
       (dolist (elt end)
         (add-to-list 'LaTeX-indent-end-list elt t))
       (LaTeX-indent-commands-regexp-make)))

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
     (font-latex-add-keywords '(("algrenewcommand" "|{\\{"))
                              'function)))
 TeX-dialect)

(defun LaTeX-algpseudocodex-package-options ()
  "Package options for the algpseudocodex package."
  (TeX-read-key-val t '(("noEnd" ("true" "false"))
                        ("indLines" ("true" "false"))
                        ("spaceRequire" ("true" "false"))
                        ("italicComments" ("true" "false"))
                        ("rightComments" ("true" "false"))
                        ("commentColor")
                        ("beginComment")
                        ("endComment")
                        ("beginLComment")
                        ("endLComment"))))

;;; algpseudocodex.el ends here
