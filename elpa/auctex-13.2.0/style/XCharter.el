;;; XCharter.el --- AUCTeX style for `XCharter.sty' (v1.24)  -*- lexical-binding: t; -*-

;; Copyright (C) 2014--2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-10-30
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

;; This file adds support for `XCharter.sty' (v1.24) from 2022/04/16.
;; `XCharter.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "XCharter"
 (lambda ()

   ;; Run style hook for various packages loaded by XCharter
   (if (memq TeX-engine '(xetex luatex))
       (TeX-run-style-hooks "fontspec")
     (TeX-run-style-hooks "textcomp" "fontaxes"))

   ;; New symbols
   (TeX-add-symbols

    ;; Only preamble commands
    '("useosf"  0)
    '("useosfI" 0)
    '("useproportional" 0)

    ;; Text commands
    '("textlf"     t)   ; lining figures
    '("lfstyle"   -1)   ;
    '("textosf"    t)   ; oldstyle figures
    '("textosfI"   t)   ; oldstyle figures alternate
    '("osfstyle"  -1)   ; whatever oldstyle option is in force
    '("texttlf"    t)
    '("tlfstyle"  -1)
    '("texttosf"   t)
    '("tosfstyle" -1)

    '("liningnums"       -1)
    '("tabularnums"      -1)
    '("oldstylenums"     -1)
    '("proportionalnums" -1)

    '("textsu"     t)   ; superior figures
    '("sustyle"   -1)   ;
    '("textinf"    t)   ; inferior figures
    '("instyle"   -1)   ;

    '("textnumerator"   t) ; numerators
    '("textnum"         t)
    '("textdenominator" t) ; denominators
    '("textde"          t) ;
    '("textfrac"  ["Number"] "Numerator" "Denominator")
    '("textsfrac" ["Number"] "Numerator" "Denominator")

    '("textth"      t)
    '("textthit"    t)
    '("thfamily"   -1))

   ;; \textnu isn't available with 'notextnu' package option
   (unless (LaTeX-provided-package-options-member "xcharter" "notextnu")
     (TeX-add-symbols '("textnu" t))
     (when (and (featurep 'font-latex)
                (eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords '(("textnu" "{"))
                                'type-command)))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("textlf"    "{")
                                ("textosf"   "{")
                                ("textosfI"  "{")
                                ("texttlf"   "{")
                                ("texttosf"  "{")
                                ("textsu"    "{")
                                ("textinf"   "{")
                                ("textnumerator"   "{")
                                ("textnum"         "{")
                                ("textdenominator" "{")
                                ("textde"          "{")
                                ("textth"          "{")
                                ("textthit"        "{"))
                              'type-command)
     (font-latex-add-keywords '(("lfstyle"           "")
                                ("osfstyle"          "")
                                ("tlfstyle"          "")
                                ("tosfstyle"         "")
                                ("liningnums"        "")
                                ("tabularnums"       "")
                                ("oldstylenums"      "")
                                ("proportionalnums"  "")
                                ("sustyle"           "")
                                ("instyle"           "")
                                ("thfamily"          ""))
                              'type-declaration)
     (font-latex-add-keywords '(("useosf"          "")
                                ("useosfI"         "")
                                ("useproportional" ""))
                              'function)
     (font-latex-add-keywords '(("textfrac"        "[{{")
                                ("textsfrac"       "[{{"))
                              'textual)))
 TeX-dialect)

(defun LaTeX-XCharter-package-options ()
  "Read the XCharter package options."
  (TeX-read-key-val t (append
                       (when (memq TeX-engine '(xetex luatex))
                         '(("nofontspec" ("true" "false"))
                           ("type1text" ("true" "false"))
                           ("type1" ("true" "false"))
                           ("defaultfeatures")))
                       '(("scaled")
                         ("scale")
                         ("lining" ("true" "false"))
                         ("lf" ("true" "false"))
                         ("oldstyle" ("true" "false"))
                         ("osf" ("true" "false"))
                         ("proportional" ("true" "false"))
                         ("p" ("true" "false"))
                         ("tabular" ("true" "false"))
                         ("t" ("true" "false"))
                         ("oldstyleI" ("true" "false"))
                         ("osfI" ("true" "false"))
                         ("sups")
                         ("scosf")
                         ("serbianc")
                         ("theoremfont")
                         ("thmlining")
                         ("oldSS")
                         ("notextnu")))))

;;; XCharter.el ends here
