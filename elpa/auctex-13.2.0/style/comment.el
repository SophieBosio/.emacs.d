;;; comment.el --- AUCTeX style for `comment.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2007, 2018--2022 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2007-03-18
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

;; This file adds support for `comment.sty'.

;;; Code:

(require 'tex)
(require 'tex-style)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(declare-function font-latex-set-syntactic-keywords
                  "font-latex")
(defvar font-latex-syntactic-keywords-extra)

;; Prepare for parsing:
(TeX-auto-add-type "comment-incl-excl" "LaTeX")

(defvar LaTeX-comment-include-exclude-regexp
  '("\\\\\\(include\\|exclude\\|special\\)comment[ \t\n\r%]*{\\([^}]+\\)}"
    (2 1) LaTeX-auto-comment-incl-excl)
  "Matches the name of environments defined by comment macros.")

(defun LaTeX-comment-auto-prepare ()
  "Reset the value of `LaTeX-auto-comment-incl-excl'."
  (setq LaTeX-auto-comment-incl-excl nil))

(defun LaTeX-comment-auto-cleanup ()
  "Process parsed elements for comment package."
  (dolist (elt (LaTeX-comment-incl-excl-list))
    (let ((env (car elt))
          (type (cadr elt)))
      ;; Make the environment available for completion
      (LaTeX-add-environments env)
      ;; Fontification
      (when (and (boundp 'font-latex-syntactic-keywords-extra)
                 (eq TeX-install-font-lock 'font-latex-setup))
        ;; For syntactic fontification.
        (if (string= type "exclude")
            ;; Argument of \excludecomment:
            (progn
              (add-to-list 'font-latex-syntactic-keywords-extra
                           ;; \begin is supposed to start at the
                           ;; beginning of a line.
                           `(,(format "^\\\\begin *{%s}.*\\(\n\\)"
                                      env)
                             (1 "!" t)))
              (add-to-list 'font-latex-syntactic-keywords-extra
                           ;; \end is supposed to start at the
                           ;; beginning of a line.
                           `(,(format "^\\(\\\\\\)end *{%s}"
                                      env)
                             (1 "!" t))))
          ;; Delete the entry from
          ;; `font-latex-syntactic-keywords-extra' if argument of
          ;; \includecomment or \specialcomment:
          (setq font-latex-syntactic-keywords-extra
                (delete `(,(format "^\\\\begin *{%s}.*\\(\n\\)"
                                   env)
                          (1 "!" t))
                        font-latex-syntactic-keywords-extra))
          (setq font-latex-syntactic-keywords-extra
                (delete `(,(format "^\\(\\\\\\)end *{%s}"
                                   env)
                          (1 "!" t))
                        font-latex-syntactic-keywords-extra))))))
  ;; Recalculate the fontification rules once at the end:
  (when (and (LaTeX-comment-incl-excl-list)
             (fboundp 'font-latex-set-syntactic-keywords)
             (eq TeX-install-font-lock 'font-latex-setup))
    (font-latex-set-syntactic-keywords)))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-comment-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-comment-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "comment"
 (lambda ()

   ;; Add comment to the parser.
   (TeX-auto-add-regexp LaTeX-comment-include-exclude-regexp)

   ;; New symbols
   (TeX-add-symbols
    `("includecomment"
      ,(lambda (optional)
         (let ((env (TeX-read-string
                     (TeX-argument-prompt optional nil "Name"))))
           (LaTeX-add-comment-incl-excls `(,env "include"))
           (LaTeX-comment-auto-cleanup)
           (TeX-argument-insert env optional))))

    `("excludecomment"
      ,(lambda (optional)
         (let ((env (TeX-read-string
                     (TeX-argument-prompt optional nil "Name"))))
           (LaTeX-add-comment-incl-excls `(,env "exclude"))
           (LaTeX-comment-auto-cleanup)
           (TeX-argument-insert env optional))))

    `("specialcomment"
      ,(lambda (optional)
         (let ((env (TeX-read-string
                     (TeX-argument-prompt optional nil "Name"))))
           (LaTeX-add-comment-incl-excls `(,env "special"))
           (LaTeX-comment-auto-cleanup)
           (TeX-argument-insert env optional)))
      "Before commands" "After commands")

    '("processcomment" "Name" "Each-line commands"
      "Before commands" "After commands"))

   ;; New environments
   (mapc #'LaTeX-add-environments LaTeX-comment-env-list)

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
              (eq TeX-install-font-lock 'font-latex-setup))
     ;; For syntactic fontification.
     (add-to-list 'font-latex-syntactic-keywords-extra
                  ;; \begin is supposed to start at the beginning of a line.
                  `(,(format "^\\\\begin *{%s}.*\\(\n\\)"
                             (regexp-opt LaTeX-comment-env-list))
                    (1 "!" t)))
     (add-to-list 'font-latex-syntactic-keywords-extra
                  ;; \end is supposed to start at the beginning of a line.
                  `(,(format "^\\(\\\\\\)end *{%s}"
                             (regexp-opt LaTeX-comment-env-list))
                    (1 "!" t)))
     (font-latex-add-keywords '(("includecomment" "{")
                                ("excludecomment" "{")
                                ("specialcomment" "{{{")
                                ("processcomment" "{{{{"))
                              'variable)
     ;; Tell font-lock about the update.
     (font-latex-set-syntactic-keywords)))
 TeX-dialect)

(defvar LaTeX-comment-package-options nil
  "Package options for the comment package.")

;;; comment.el ends here
