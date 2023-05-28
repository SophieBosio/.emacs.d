;;; multibib.el --- AUCTeX style for `multibib.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-01-18
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
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary:

;; This file adds support for `multibib.sty' v1.4 form 2008-12-10.

;; Note for RefTeX users: multibib package allows multiple macro calls
;; to load bibliography database files.  RefTeX bundled with Emacs
;; 29.1 is adjusted to recognize this.  For prior Emacs versions, the
;; following code in the preamble should suffice to "fool" AUCTeX and
;; RefTeX for multiple bibliography calls:
;;
;;  \iffalse
;;  \usepackage{biblatex}
;;  \fi

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

;; Setup for \newcites: Note that the macro is called \newcites and we
;; choose the name newcite in order to get away with any un-natural
;; plural form:
(TeX-auto-add-type "multibib-newcite" "LaTeX")

(defvar LaTeX-multibib-newcites-regexp
  `(,(concat "\\\\newcites"
             "[ \t\n\r%]*"
             "{\\([^}]+\\)}")
    1 LaTeX-auto-multibib-newcite)
  "Matches the arguments of \\newcites from multibib package.")

(defun LaTeX-multibib-auto-prepare ()
  "Clear `LaTeX-auto-multibib-newcite' variable before parsing."
  (setq LaTeX-auto-multibib-newcite nil))

(defun LaTeX-multibib-auto-cleanup ()
  "Process the parsed elements from \\newcites macro."
  (when (LaTeX-multibib-newcite-list)
    (let (suffix)
      (dolist (elt (LaTeX-multibib-newcite-list))
        (push (replace-regexp-in-string "[ %\n\r\t]" "" (car elt)) suffix))
      (setq suffix (mapconcat #'identity suffix ","))
      (dolist (elt (split-string suffix "," t))
        (TeX-add-symbols
         `(,(concat "cite" elt)
           (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) ())
           TeX-arg-cite)
         `(,(concat "nocite" elt) TeX-arg-cite)
         `(,(concat "bibliographystyle" elt) TeX-arg-bibstyle)
         `(,(concat "bibliography" elt) TeX-arg-bibliography))
        ;; RefTeX: Check if `reftex-bibliography-commands' is bound
        ;; and append our newly defined entry to a local version of
        ;; it:
        (when (boundp 'reftex-bibliography-commands)
          (add-to-list (make-local-variable 'reftex-bibliography-commands)
                       (concat "bibliography" elt)
                       t))
        ;; Fontification
        (when (and (featurep 'font-latex)
                   (eq TeX-install-font-lock 'font-latex-setup))
          (font-latex-add-keywords `((,(concat "cite" elt)   "[{")
                                     (,(concat "nocite" elt) "{")
                                     (,(concat "bibliography" elt) "{"))
                                   'reference)
          (font-latex-add-keywords `((,(concat "bibliographystyle" elt) "{"))
                                   'function))))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-multibib-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-multibib-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "multibib"
 (lambda ()

   ;; Add 'multibib' to the parser:
   (TeX-auto-add-regexp LaTeX-multibib-newcites-regexp)

   (TeX-add-symbols
    `("newcites"
      ,(lambda (optional)
         (let ((suf (TeX-read-string
                     (TeX-argument-prompt optional nil "Suffix"))))
           (LaTeX-add-multibib-newcites suf)
           (LaTeX-multibib-auto-cleanup)
           (TeX-argument-insert suf optional)))
      "Heading")
    '("setbiblabelwidth" "Label"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newcites"         "{{")
                                ("setbiblabelwidth" "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-multibib-package-options '("labeled" "resetlabels")
  "Package options for the multibib package.")

;;; multibib.el ends here
