;;; kvoptions.el --- AUCTeX style for `kvoptions.sty' version 3.14.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-05-27
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

;; This file adds support for `kvoptions.sty' 3.14 from 2020-10-07.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "kvoptions"
 (lambda ()

   ;; kvoptions.sty loads kvsetkeys.sty:
   (TeX-run-style-hooks "kvsetkeys")

   (TeX-add-symbols
    ;; 2.1.1 \ProcessKeyvalOptions
    '("ProcessKeyvalOptions" "Family")
    '("ProcessKeyvalOptions*" 0)

    '("ProcessLocalKeyvalOptions" "Family")
    '("ProcessLocalKeyvalOptions*" 0)

    '("SetupKeyvalOptions"
      (TeX-arg-key-val (("family")
                        ("prefix")
                        ("setkeys" ("\\setkeys" "\\kvsetkeys")))))

    ;; 2.2.1 \DeclareStringOption
    '("DeclareStringOption" ["Initial value"] "Key" ["Default value"])

    ;; 2.2.2 \DeclareBoolOption
    '("DeclareBoolOption" ["Initial value"] "Key")

    ;; 2.2.3 \DeclareComplementaryOption
    '("DeclareComplementaryOption" "Key" "Parent")

    ;; 2.2.4 \DeclareVoidOption
    '("DeclareVoidOption" "Key" t)

    ;; 2.2.5 \DeclareDefaultOption
    '("DeclareDefaultOption" t)

    ;; 2.2.6 Local options
    '("DeclareLocalOption" "Option")
    '("DeclareLocalOptions" "Options")

    ;; 2.2.8 \DisableKeyvalOption
    '("DisableKeyvalOption"
      [TeX-arg-key-val (("action" ("undef" "warning" "error" "ignore"))
                        ("global")
                        ("local")
                        ("package")
                        ("class"))]
      "Family" "Key")

    ;; 2.2.9 \AddToKeyvalOption
    '("AddToKeyvalOption"  "Family" "Key" t)
    '("AddToKeyvalOption*" "Key" t))

   ;; Fontification: Don't fontify arguments which contain code
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("ProcessKeyvalOptions"       "*")
                                ("ProcessLocalKeyvalOptions"  "*")
                                ("SetupKeyvalOptions"         "{")
                                ("DeclareStringOption"        "[{[")
                                ("DeclareBoolOption"          "[{")
                                ("DeclareComplementaryOption" "{{")
                                ("DeclareVoidOption"          "{")
                                ("DeclareDefaultOption"       "")
                                ("DeclareLocalOption"         "{")
                                ("DeclareLocalOptions"        "{")
                                ("DisableKeyvalOption"        "[{{")
                                ("AddToKeyvalOption"          "*{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-kvoptions-package-options '("patch" "debugshow")
  "Package options for the kvoptions package.")

;;; kvoptions.el ends here
