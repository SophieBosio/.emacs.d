;;; ltx-base.el --- AUCTeX style for basic LaTeX commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2004--2022 Free Software Foundation, Inc.

;; Author: Frank Küster <frank@kuesterei.ch>
;; Maintainer: auctex-devel@gnu.org
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

;; This file adds general support for basic LaTeX commands used for
;; writing LaTeX class files (.cls), style files (.sty) and package
;; files (.dtx).  Most of the macros are taken from clsguide.pdf.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "ltx-base"
 (lambda ()
   (TeX-add-symbols

    ;; 4.1 Identification.  Other '\Provide*' macros are available in
    ;; latex.el
    '("NeedsTeXFormat" "Format" [ "Release date" ])

    ;; 4.2 Loading files
    ;; \RequirePackage is provided in latex.el
    '("RequirePackageWithOptions" "Package" [ "Release information" ])

    '("LoadClass" [ "Options" ] "Class" [ "Release information" ])
    '("LoadClassWithOptions"    "Class" [ "Release information" ])

    ;; 4.3 Option declaration
    '("DeclareOption" "Option" t)
    '("DeclareOption*" t)

    ;; 4.4 Commands within option code
    '("CurrentOption" 0)
    '("OptionNotUsed" 0)

    ;; 4.5 Moving options around
    '("PassOptionsToPackage" "Option(s) list" "Package")
    '("PassOptionsToClass" "Option(s) list" "Class")

    ;; 4.6 Delaying code
    '("AtEndOfPackage" t)
    '("AtEndOfClass" t)
    '("AtBeginDocument" t)
    '("AtEndDocument" t)
    '("AtBeginDvi" t)

    ;; 4.7 Option processing
    '("ProcessOptions" (TeX-arg-literal "\\relax"))
    "ProcessOptions*"
    '("ExecuteOptions" "Option list")

    ;; 4.8 Safe file commands
    '("IfFileExists" "File" 2)
    '("InputIfFileExists" "File" 2)

    ;; 4.9 Reporting errors, etc
    '("ClassError" "Class name" "Error text" t)
    '("PackageError" "Package name" "Error text" t)

    '("ClassWarning" "Class name" t)
    '("PackageWarning" "Package name" t)
    '("ClassWarningNoLine" "Class name" t)
    '("PackageWarningNoLine" "Package name" t)

    '("ClassInfo" "Class name" t)
    '("PackageInfo" "Package name" t)

    '("MessageBreak" 0)
    '("message" "Log Message")

    ;; 4.10 Defining commands
    '("DeclareRobustCommand"
      TeX-arg-define-macro [ TeX-arg-define-macro-arguments ] t)
    '("DeclareRobustCommand*"
      TeX-arg-define-macro [ TeX-arg-define-macro-arguments ] t)

    '("CheckCommand" TeX-arg-macro [ TeX-arg-define-macro-arguments ] t)
    '("CheckCommand*" TeX-arg-macro [ TeX-arg-define-macro-arguments ] t)

    ;; 5.1 Layout parameters
    ;; \paperheight & \paperwidth are provided in latex.el

    ;; 5.2 Case changing
    ;; \MakeUppercase & \MakeLppercase are provided in latex.el

    ;; 5.4 Better user-defined math display environments
    "ignorespacesafterend"

    ;; 5.5 Normalising spacing
    "normalsfcodes"

    ;; Some general macros not mentioned in clsguide.pdf
    '("@addtoreset" TeX-arg-counter (TeX-arg-counter "Within counter"))
    '("addpenalty" "Penalty")
    '("@ifundefined" TeX-arg-macro 2)
    '("@ifnextchar" (TeX-arg-literal " ") (TeX-arg-free "Character") 2)
    '("expandafter" 0)

    ;; These macros are currently (June 2022) described in ltkeys.dtx:
    '("DeclareKeys" ["Family"] t)
    '("DeclareUnknownKeyHandler" ["Family"] t)
    '("ProcessKeyOptions" ["Family"])
    '("SetKeys" ["Family"] t))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("NeedsTeXFormat"       "{[")
                                ("RequirePackageWithOptions" "{[")
                                ("LoadClass"            "[{[")
                                ("LoadClassWithOptions" "{[")
                                ;; Don't fontify the second argument
                                ;; which will contain code:
                                ("DeclareOption"         "*{")
                                ("CurrentOption"         "")
                                ("OptionNotUsed"         "")
                                ("PassOptionsToPackage"  "{{")
                                ("PassOptionsToClass"    "{{")

                                ("AtEndOfPackage"  "")
                                ("AtEndOfClass"    "")
                                ("AtBeginDocument" "")
                                ("AtEndDocument"   "")
                                ("AtBeginDvi"      "")

                                ("ProcessOptions" "*")
                                ("ExecuteOptions" "{")
                                ("DeclareRobustCommand" "*|{\\[[{")
                                ("CheckCommand"         "*|{\\[[{")

                                ("DeclareKeys"              "[{")
                                ("DeclareUnknownKeyHandler" "[{")
                                ("ProcessKeyOptions"        "[")
                                ("SetKeys"                  "[{"))
                              'function)))
 TeX-dialect)

;; Local Variables:
;; coding: utf-8
;; End:

;;; ltx-base.el ends here
