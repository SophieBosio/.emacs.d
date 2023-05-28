;;; backref.el --- AUCTeX style for `backref.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-02-06
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

;; This file adds support for `backref.sty' v1.41 form 2021-02-04.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "backref"
 (lambda ()

   (TeX-add-symbols
    '("backrefsetup"
      (TeX-arg-key-val (("verbose" ("true" "false"))
                        ("enable" ("true" "false"))
                        ("disable" ("true" "false")))))
    "backrefparscanfalse"
    "backrefparscantrue"
    "backrefprint"
    "backref"
    "backrefalt"
    "backrefsep"
    "backreftwosep"
    "backreflastsep"
    "backrefentrycount")

   ;; This is a hack: We want to have the 2 macros
   ;; \backrefparscanfalse and \backrefparscantrue indented like
   ;; \bibitem, hence we add them to a local version of
   ;; `LaTeX-item-regexp':
   (unless (string-match-p "backrefparscan" LaTeX-item-regexp)
     (setq-local LaTeX-item-regexp
                 (concat LaTeX-item-regexp
                         "\\|" "backrefparscan\\(false\\|true\\)\\b")))

   ;; Only add "backrefprint" here, "backrefparscan*" will be added
   ;; via `LaTeX-item-regexp' when `LaTeX-set-paragraph-start' is
   ;; called:
   (LaTeX-paragraph-commands-add-locally '("backrefprint"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("backrefsetup"        "{")
                                ("backrefparscanfalse" "")
                                ("backrefparscantrue"  "")
                                ("backrefprint"        ""))
                              'function)))
 TeX-dialect)

(defvar LaTeX-backref-package-options
  '("ref" "pageref" "hyperref" "hyperpageref"
    "enable" "disable" "verbose"
    "english" "american" "australian" "british" "canadian"
    "newzealand" "UKenglish" "USenglish"
    "german" "ngerman" "austrian" "naustrian"
    "french" "acadian" "canadien" "frenchb" "francais"
    "spanish" "brazil" "brazilian" "afrikaans")
  "Package options for the backref package.")

;;; backref.el ends here
