;;; ulem.el --- AUCTeX style for `ulem.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2022  Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
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

;; This file adds support for `ulem.sty' dated 2019-11-18.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "ulem"
 (lambda ()
   (TeX-add-symbols
    '("uline" 1)
    '("uuline" 1)
    '("uwave" 1)
    '("sout" 1)
    '("xout" 1)
    '("dashuline" 1)
    '("dotuline" 1)
    ;; can be used with \renewcommand
    "ULthickness"
    ;; custom commands can be defined with these commands; see the
    ;; documentation for an example
    "ULon"
    ;; \markoverwith takes on argument
    '("markoverwith" 1)
    ;; \useunder {underline_command}{font_declaration}{font_command}
    ;; replaces occurences of font_declaration and font_command with the
    ;; underline_command
    '("useunder"
      (TeX-arg-completing-read ("\\uline" "\\uuline" "\\uwave" "\\sout"
                                "\\xout"  "\\dashuline" "\\dotuline")
                               "Underline command")
      (TeX-arg-completing-read ("\\itshape"  "\\bfseries" "\\scshape"
                                "\\ttfamily" "\\upshape"  "\\mdseries"
                                "\\rmfamily" "\\sffamily" "\\slshape")
                               "Font declaration")
      (TeX-arg-completing-read ("\\textit" "\\textbf" "\\textsc"
                                "\\texttt" "\\textup" "\\textmd"
                                "\\textrm" "\\textsf" "\\textsl")
                               "Font command")))

   ;; \ULdepth can be changed with \setlength
   (LaTeX-add-lengths "ULdepth")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     ;; Tell font-lock about the update.
     (font-latex-add-keywords '(("useunder" "{{{"))
                              'function)
     (font-latex-add-keywords '(("sout" "{")
                                ("xout" "{"))
                              'textual)
     (font-latex-add-keywords '(("uline" "{")
                                ("uuline" "{")
                                ("uwave" "{")
                                ("dashuline" "{")
                                ("dotuline" "{"))
                              'underline-command)))
 TeX-dialect)

(defvar LaTeX-ulem-package-options
  '("UWforbf" "ULforem" "normalbf" "normalem")
  "Package options for the ulem package.")

;;; ulem.el ends here
