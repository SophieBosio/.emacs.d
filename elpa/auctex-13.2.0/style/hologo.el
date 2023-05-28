;;; hologo.el --- AUCTeX style for `hologo.sty' (v1.10)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015--2022 Free Software Foundation, Inc.

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

;; This file adds support for `hologo.sty' (v1.10) from 2012/04/26.
;;  `hologo.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-hologo-logo-names
  '("(La)TeX"
    "AmSLaTeX"
    "AmSTeX"
    "biber"
    "BibTeX"
    "BibTeX8"
    "ConTeXt"
    "emTeX"
    "eTeX"
    "ExTeX"
    "HanTheThanh"
    "iniTeX"
    "KOMAScript"
    "La"
    "LaTeX"
    "LaTeX2e"
    "LaTeX3"
    "LaTeXe"
    "LaTeXML"
    "LaTeXTeX"
    "LuaLaTeX"
    "LuaTeX"
    "LyX"
    "METAFONT"
    "MetaFun"
    "METAPOST"
    "MetaPost"
    "MiKTeX"
    "NTS"
    "OzMF"
    "OzMP"
    "OzTeX"
    "OzTtH"
    "PCTeX"
    "pdfTeX"
    "pdfLaTeX"
    "PiC"
    "PiCTeX"
    "plainTeX"
    "SageTeX"
    "SLiTeX"
    "SliTeX"
    "teTeX"
    "TeX"
    "TeX4ht"
    "TTH"
    "virTeX"
    "VTeX"
    "Xe"
    "XeLaTeX"
    "XeTeX")
  "List of logos provided by \"hologo.sty\".")

(defvar LaTeX-hologo-key-val-options-global
  '(("break"              ("true" "false"))
    ("hyphenbreak"        ("true" "false"))
    ("spacebreak"         ("true" "false"))
    ("discretionarybreak" ("true" "false")))
  "Global key=value options for hologo macros.")

(defvar LaTeX-hologo-key-val-options-local
  '(("variant" ("sf" "sc"                          ; BibTeX
                "lift"                             ; SliTeX
                "narrow" "simple"                  ; SliTeX, ConTeXt
                "space"  "hyphen" "runtogether"))) ; plainTeX
  "Local key=value options for hologo macros.")

(defun LaTeX-hologo--arg-use-region-or-query-logo-name ()
  "Check if region is active and over element from `LaTeX-hologo-logo-names'."
  (and (use-region-p)
       (member (buffer-substring (region-beginning) (region-end))
               LaTeX-hologo-logo-names)))

(defun LaTeX-hologo--arg-use-region (_optional)
  "Wrap region around braces.
OPTIONAL is ignored."
  (insert TeX-grop)
  (goto-char (region-end))
  (insert TeX-grcl))

(TeX-add-style-hook
 "hologo"
 (lambda ()
   (TeX-add-symbols

    ;; Insert logo macros
    `("hologo"
      (TeX-arg-conditional (LaTeX-hologo--arg-use-region-or-query-logo-name)
          (LaTeX-hologo--arg-use-region)
        ((TeX-arg-completing-read LaTeX-hologo-logo-names "Logo name"))))
    `("Hologo"
      (TeX-arg-conditional (LaTeX-hologo--arg-use-region-or-query-logo-name)
          (LaTeX-hologo--arg-use-region)
        ((TeX-arg-completing-read LaTeX-hologo-logo-names "Logo name"))))

    ;; Setup macros
    '("hologoSetup" (TeX-arg-key-val LaTeX-hologo-key-val-options-global))

    `("hologoLogoSetup"
      (TeX-arg-completing-read LaTeX-hologo-logo-names "Logo name")
      (TeX-arg-key-val ,(lambda ()
                          (save-excursion
                            (re-search-backward "\\\\hologoLogoSetup{\\([^}]+\\)}"
                                                (line-beginning-position) t))
                          (let ((logo (match-string-no-properties 1)))
                            (cond ((string= logo "BibTeX")
                                   (append '(("variant" ("sf" "sc")))
                                           LaTeX-hologo-key-val-options-global))
                                  ((string= logo "ConTeXt")
                                   (append '(("variant" ("narrow" "simple")))
                                           LaTeX-hologo-key-val-options-global))
                                  ((string= logo "plainTeX")
                                   (append '(("variant" ("space" "hyphen" "runtogether")))
                                           LaTeX-hologo-key-val-options-global))
                                  ((or (string= logo "SLiTeX")
                                       (string= logo "SliTeX"))
                                   (append '(("variant" ("lift" "narrow" "lift")))
                                           LaTeX-hologo-key-val-options-global))
                                  (t
                                   LaTeX-hologo-key-val-options-global))))))

    '("hologoDriverSetup" (TeX-arg-completing-read
                           ("pdftex"  "luatex"
                            "dvipdfm" "dvipdfmx"
                            "dvips"   "dvipsone" "xdvi"
                            "xetex"   "vtex"     "driverfallback")
                           "Driver"))

    '("hologoFontSetup"
      (TeX-arg-key-val (("general") ("bibsf")
                        ("rm") ("sc") ("sf") ("sy") ("logo"))))

    `("hologoLogoFontSetup"
      (TeX-arg-completing-read ("BibTeX" "ExTeX" "SliTeX" "AmS" "NTS"
                                "KOMAScript" "METAFONT" "METAPOST")
                               "Logo name")
      (TeX-arg-key-val ,(lambda ()
                          (save-excursion
                            (re-search-backward "\\\\hologoLogoFontSetup{\\([^}]+\\)}"
                                                (line-beginning-position) t))
                          (let ((logo (match-string-no-properties 1)))
                            (cond ((string= logo "BibTeX")
                                   '(("bibsf") ("sc")))
                                  ((string= logo "ExTeX")
                                   '(("rm") ("sy")))
                                  ((string= logo "SliTeX")
                                   '(("rm") ("sc")))
                                  ((or (string= logo "AmS")
                                       (string= logo "NTS"))
                                   '(("sy")))
                                  ((string= logo "KOMAScript")
                                   '(("sf")))
                                  ((or (string= logo "METAFONT")
                                       (string= logo "METAPOST"))
                                   '(("logo")))
                                  (t
                                   nil))))))

    ;; Additional user macros
    `("hologoVariant"
      (TeX-arg-conditional (LaTeX-hologo--arg-use-region-or-query-logo-name)
          (LaTeX-hologo--arg-use-region)
        ((TeX-arg-completing-read LaTeX-hologo-logo-names "Logo name")))
      (TeX-arg-key-val ,(append LaTeX-hologo-key-val-options-local
                                LaTeX-hologo-key-val-options-global)))

    `("HologoVariant"
      (TeX-arg-conditional (LaTeX-hologo--arg-use-region-or-query-logo-name)
          (LaTeX-hologo--arg-use-region)
        ((TeX-arg-completing-read LaTeX-hologo-logo-names "Logo name")))
      (TeX-arg-key-val ,(append LaTeX-hologo-key-val-options-local
                                LaTeX-hologo-key-val-options-global)))

    '("hologoList" 0)

    '("hologoEntry" "Logo name" "Variant" "Since"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("hologo"   "{")
                                ("Hologo"   "{"))
                              'textual)
     (font-latex-add-keywords '(("hologoSetup"         "{")
                                ("hologoLogoSetup"     "{{")
                                ("hologoDriverSetup"   "{")
                                ("hologoFontSetup"     "{")
                                ("hologoLogoFontSetup" "{{")
                                ("hologoVariant"       "{{")
                                ("HologoVariant"       "{{")
                                ("hologoList"          "")
                                ("hologoEntry"         "{{{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-hologo-package-options nil
  "Package options for the hologo package.")

;;; hologo.el ends here
