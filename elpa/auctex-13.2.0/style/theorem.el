;;; theorem.el --- AUCTeX style for `theorem.sty' (v2.2c)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022  Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2015-10-31
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

;; This file adds support for `theorem.sty' (v2.2c) from 2014/10/28.
;; `theorem.sty' is a standard LaTeX package and part of TeXLive.

;; This style interacts with AUCTeX and RefTeX mechanisms for
;; inserting labels into new defined environments with "\newtheoreom".
;; AUCTeX users need to add the new environment to `LaTeX-label-alist'
;; via customize or in init-file like this:
;;
;;   (add-to-list 'LaTeX-label-alist '("lemma" . "lem:"))
;;
;; RefTeX users have to add the value to both `LaTeX-label-alist' and
;; `reftex-label-alist' like this:
;;
;;   (add-to-list 'LaTeX-label-alist '("lemma" . "lem:"))
;;   (add-to-list 'reftex-label-alist
;;                '("lemma" ?m "lem:" "~ref{%s}"
;;                  nil ("Lemma" "lemma") nil))

;;; Code:

(require 'crm)
(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-theorem-theoremstyle-list
  '("plain" "break" "margin" "change" "marginbreak" "changebreak")
  "List of theorem styles provided by `theorem.sty'.")

;; Setup parsing for \newtheorem
(TeX-auto-add-type "theorem-newtheorem" "LaTeX")

(defun LaTeX-theorem-auto-prepare ()
  "Clear `LaTeX-auto-theorem-newtheorem' before parsing."
  (setq LaTeX-auto-theorem-newtheorem nil))

(defun LaTeX-theorem-auto-cleanup ()
  "Move parsed results from `LaTeX-auto-theorem-newtheorem' and
make them available as new environments."
  (dolist (newthm (mapcar #'car (LaTeX-theorem-newtheorem-list)))
    (LaTeX-add-environments (list newthm #'LaTeX-env-label-args ["Heading"]))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-theorem-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-theorem-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "theorem"
 (lambda ()

   (TeX-auto-add-regexp
    `(,(concat "\\\\newtheorem{\\(" TeX-token-char "+\\)}")
      1 LaTeX-auto-theorem-newtheorem))

   (TeX-add-symbols
    ;; Overrule the defintion in `latex.el':
    `("newtheorem"
      ,(lambda (optional)
         (let ((nthm (TeX-read-string
                      (TeX-argument-prompt optional nil "Environment"))))
           (LaTeX-add-theorem-newtheorems nthm)
           (LaTeX-add-environments (list nthm #'LaTeX-env-label-args ["Heading"]))
           (TeX-argument-insert nthm optional)))
      [ TeX-arg-environment "Numbered like" ]
      t [ (TeX-arg-eval progn (if (eq (save-excursion
                                        (backward-char 2)
                                        (preceding-char)) ?\])
                                  ()
                                (TeX-arg-counter t "Within counter"))
                        "") ])

    '("theoremstyle"
      (TeX-arg-completing-read LaTeX-theorem-theoremstyle-list "Style"))

    `("theorembodyfont"
      (TeX-arg-completing-read-multiple
       ,(lambda () (append LaTeX-font-family
                           LaTeX-font-series
                           LaTeX-font-shape
                           LaTeX-font-size))
       "Body font" nil nil ,(regexp-quote TeX-esc) ,TeX-esc
       nil nil nil nil ,TeX-esc))

    `("theoremheaderfont"
      (TeX-arg-completing-read-multiple
       ,(lambda () (append LaTeX-font-family
                           LaTeX-font-series
                           LaTeX-font-shape
                           LaTeX-font-size))
       "Header font" nil nil ,(regexp-quote TeX-esc) ,TeX-esc
       nil nil nil nil ,TeX-esc))

    '("theorempreskipamount"
      (TeX-arg-length "Skip before theorem"))

    '("theorempostskipamount"
      (TeX-arg-length "Skip after theorem")))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("theoremstyle"          "{")
                                ("theorembodyfont"       "{")
                                ("theoremheaderfont"     "{")
                                ("theorempreskipamount"  "{")
                                ("theorempostskipamount" "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-theorem-package-options nil
  "Package options for the theorem package.")

;;; theorem.el ends here
