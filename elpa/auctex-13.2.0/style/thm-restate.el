;;; thm-restate.el --- AUCTeX style for `thm-restate.sty' (v66)  -*- lexical-binding: t; -*-

;; Copyright (C) 2018--2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2018-07-07
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

;; This file adds support for `thm-restate.sty'.  `thm-restate.sty' is
;; part of `thmtools' package (v66) from 2014/04/21.  `thmtools.sty'
;; is part of TeXLive.

;;; Code:

;; Needed for auto-parsing.
(require 'tex)
(require 'latex)

;; Silence the parser:
(declare-function LaTeX-thmtools-declaretheorem-list
                  "thmtools" ())
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

;; Setup for macro names defined with
;; \begin{restatable*?}[<Heading>]{<env-name>}{<macro name>}:

(TeX-auto-add-type "thmrestate-restatable-macro" "LaTeX")

(defvar LaTeX-thmrestate-restatable-marco-regexp
  `(,(concat "\\\\begin{restatable\\*?}"
             "[ \t\n\r%]*"
             "\\(?:\\[[^]]*\\]\\)?"
             "[ \t\n\r%]*"
             "\\(?:{[^}]+}\\)"
             "[ \t\n\r%]*"
             "{\\([^}]+\\)}")
    1 LaTeX-auto-thmrestate-restatable-macro))

(defun LaTeX-thmrestate-auto-prepare ()
  "Clear `LaTeX-auto-thmrestate-restatable-macro' before parsing."
  (setq LaTeX-auto-thmrestate-restatable-macro nil))

(defun LaTeX-thmrestate-auto-cleanup ()
  "Process parsed elements from thm-restate package."
  (dolist (newmac (mapcar #'car (LaTeX-thmrestate-restatable-macro-list)))
    (TeX-add-symbols newmac (concat newmac "*"))
    (when (and (featurep 'font-latex)
               (eq TeX-install-font-lock 'font-latex-setup))
      (font-latex-add-keywords `((,newmac "*"))
                               'function))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-thmrestate-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-thmrestate-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-env-thmrestate-restatable (optional)
  "Insert last argument of restatable environment from thm-restate package."
  (let ((mac (TeX-read-string
              (TeX-argument-prompt optional nil "Macro"))))
    (when (and (featurep 'font-latex)
               (eq TeX-install-font-lock 'font-latex-setup))
      (font-latex-add-keywords `((,mac "*"))
                               'function))
    (TeX-add-symbols mac (concat mac "*"))
    (TeX-argument-insert mac optional)))

(TeX-add-style-hook
 "thm-restate"
 (lambda ()

   ;; Run the style hook `thmtools.el':
   (TeX-run-style-hooks "thmtools")

   ;; Add thm-restate to the parser
   (TeX-auto-add-regexp LaTeX-thmrestate-restatable-marco-regexp)

   ;; Provide restatable\\*? environment
   (LaTeX-add-environments
    `("restatable"  LaTeX-env-args
      ["Heading"]
      (TeX-arg-completing-read
       ;; Name of the environment we are referring to; this can be
       ;; defined via amsthm.sty, ntheorem.sty or thmtools.sty:
       ,(lambda ()
          (append
           ;; Cater for environments defined with amsthm's \newtheorem
           (when (and (fboundp 'LaTeX-amsthm-newtheorem-list)
                      (LaTeX-amsthm-newtheorem-list))
             (LaTeX-amsthm-newtheorem-list))
           ;; Cater for environments defined with ntheorem's \newtheorem
           (when (and (fboundp 'LaTeX-ntheorem-newtheorem-list)
                      (LaTeX-ntheorem-newtheorem-list))
             (LaTeX-ntheorem-newtheorem-list))
           ;; Environments defined with \declaretheorem
           (LaTeX-thmtools-declaretheorem-list)))
       "Environment")
      LaTeX-env-thmrestate-restatable)

    `("restatable*" LaTeX-env-args
      ["Heading"]
      (TeX-arg-completing-read
       ;; Name of the environment we are referring to; this can be
       ;; defined via amsthm.sty, ntheorem.sty or thmtools.sty:
       ,(lambda ()
          (append
           ;; Cater for environments defined with amsthm's \newtheorem
           (when (and (fboundp 'LaTeX-amsthm-newtheorem-list)
                      (LaTeX-amsthm-newtheorem-list))
             (LaTeX-amsthm-newtheorem-list))
           ;; Cater for environments defined with ntheorem's \newtheorem
           (when (and (fboundp 'LaTeX-ntheorem-newtheorem-list)
                      (LaTeX-ntheorem-newtheorem-list))
             (LaTeX-ntheorem-newtheorem-list))
           ;; Environments defined with \declaretheorem
           (LaTeX-thmtools-declaretheorem-list)))
       "Environment")
      LaTeX-env-thmrestate-restatable)))
 TeX-dialect)

(defvar LaTeX-thm-restate-package-options nil
  "Package options for the thm-restate package.")

;;; thm-restate.el ends here
