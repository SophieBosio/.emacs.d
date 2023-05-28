;;; amsthm.el --- Style hook for the AMS-LaTeX amsthm package.  -*- lexical-binding: t; -*-

;; Copyright (C) 1997--2022  Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@strw.leidenuniv.nl>
;; Maintainer: auctex-devel@gnu.org

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

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-amsthm-package-options nil
  "Package options for the amsthm package.")

;; Setup parsing for \newtheorem
(TeX-auto-add-type "amsthm-newtheorem" "LaTeX")

;; Setup parsing for \newtheoremstyle
(TeX-auto-add-type "amsthm-newtheoremstyle" "LaTeX")

(defun LaTeX-amsthm-auto-prepare ()
  "Clear `LaTeX-auto-amsthm-newtheorem' and
`LaTeX-auto-amsthm-newtheoremstyle' before parsing."
  (setq LaTeX-auto-amsthm-newtheorem nil)
  (setq LaTeX-auto-amsthm-newtheoremstyle nil))

(defun LaTeX-amsthm-auto-cleanup ()
  "Move parsed results from `LaTeX-auto-amsthm-newtheorem' and
make them available as new environments."
  (dolist (newthm (mapcar #'car (LaTeX-amsthm-newtheorem-list)))
    (LaTeX-add-environments (list newthm #'LaTeX-env-label-args ["Heading"]))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-amsthm-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-amsthm-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "amsthm"
 (lambda ()
   ;; Add the pre-defined styles:
   (LaTeX-add-amsthm-newtheoremstyles "definition"
                                      "plain"
                                      "remark")

   (LaTeX-add-environments
    '("proof" LaTeX-env-label-args ["Heading"]))

   (TeX-add-symbols
    ;; Overrule the defintion in `latex.el':
    `("newtheorem"
      ,(lambda (optional)
         (let ((nthm (TeX-read-string
                      (TeX-argument-prompt optional nil "Environment"))))
           (LaTeX-add-amsthm-newtheorems nthm)
           (LaTeX-add-environments (list nthm #'LaTeX-env-label-args ["Heading"]))
           (TeX-argument-insert nthm optional)))
      [ TeX-arg-environment "Numbered like" ]
      t [ (TeX-arg-eval progn (if (eq (save-excursion
                                        (backward-char 2)
                                        (preceding-char)) ?\])
                                  ()
                                (TeX-arg-counter t "Within counter"))
                        "") ])

    `("newtheorem*"
      ,(lambda (optional)
         (let ((nthm (TeX-read-string
                      (TeX-argument-prompt optional nil "Environment"))))
           (LaTeX-add-amsthm-newtheorems nthm)
           ;; NOTE: Using `LaTeX-env-label-args' on environments
           ;; defined with \newtheorem* is semi-accurate since these
           ;; environments are not numbered.  But numbering depends on
           ;; an entry in `LaTeX-label-alist' and we assume that users
           ;; will not add an entry to it.  Hence,
           ;; `LaTeX-env-label-args' will not insert a label and we
           ;; don't need to differentiate.
           (LaTeX-add-environments (list nthm #'LaTeX-env-label-args ["Heading"]))
           (TeX-argument-insert nthm optional)))
      "Heading")

    '("theoremstyle"
      (TeX-arg-completing-read (LaTeX-amsthm-newtheoremstyle-list) "Style"))
    "qedhere"
    "swapnumbers"

    `("newtheoremstyle"
      ,(lambda (optional)
         (let ((nthmstyle (TeX-read-string
                           (TeX-argument-prompt optional nil "Style name"))))
           (LaTeX-add-amsthm-newtheoremstyles nthmstyle)
           (TeX-argument-insert nthmstyle optional)))
      (TeX-arg-length "Space above")
      (TeX-arg-length "Space below")
      (TeX-arg-completing-read-multiple
       ,(lambda () (append LaTeX-font-family
                           LaTeX-font-series
                           LaTeX-font-shape
                           LaTeX-font-size))
       "Body font" nil nil ,(regexp-quote TeX-esc) ,TeX-esc
       nil nil nil nil ,TeX-esc)
      "Indent amount"
      (TeX-arg-completing-read-multiple
       ,(lambda () (append LaTeX-font-family
                           LaTeX-font-series
                           LaTeX-font-shape
                           LaTeX-font-size))
       "Theorem head font" nil nil ,(regexp-quote TeX-esc) ,TeX-esc
       nil nil nil nil ,TeX-esc)
      "Punctuation after head"
      (TeX-arg-length "Space after head")
      "Theorem head spec"))

   (TeX-auto-add-regexp
    `(,(concat "\\\\newtheorem\\*?{\\(" TeX-token-char "+\\)}")
      1 LaTeX-auto-amsthm-newtheorem))
   (TeX-auto-add-regexp
    `(,(concat "\\\\newtheoremstyle{\\(" TeX-token-char "+\\)}")
      1 LaTeX-auto-amsthm-newtheoremstyle))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newtheorem"      "*{[{[")
                                ("theoremstyle"    "{")
                                ("newtheoremstyle" "{{{{{{{{{"))
                              'function)))
 TeX-dialect)

;;; amsthm.el ends here
