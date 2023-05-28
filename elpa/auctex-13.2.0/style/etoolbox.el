;;; etoolbox.el --- AUCTeX style for `etoolbox.sty' v2.5k  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Created: 2022-03-19
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

;; This file adds support for `etoolbox.sty' v2.5k from 2020/10/05.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defun LaTeX-arg-etoolbox-csname (optional &optional prompt)
  "Query and insert the name of a TeX control sequence.
If OPTIONAL is non-nil, then insert it in square brackets.
PROMPT replaces the standard one."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Control sequence")
    (TeX-symbol-list))
   optional))

(defun LaTeX-arg-etoolbox-macro-free (optional &optional prompt)
  "Query and insert a macro not surrounded by braces.
If OPTIONAL is non-nil, insert the macro only when given by the
user."
  (let ((TeX-arg-opening-brace "")
        (TeX-arg-closing-brace "")
        (macro (completing-read
                (TeX-argument-prompt optional prompt
                                     (concat "Macro: " TeX-esc)
                                     t)
                (TeX-symbol-list))))
    (TeX-argument-insert macro optional TeX-esc)))

(TeX-add-style-hook
 "etoolbox"
 (lambda ()

   (TeX-add-symbols

    ;; 2.1 Definitions
    '("newrobustcmd"  TeX-arg-define-macro [ TeX-arg-define-macro-arguments ] t)
    '("newrobustcmd*" TeX-arg-define-macro [ TeX-arg-define-macro-arguments ] t)
    '("renewrobustcmd"  TeX-arg-macro [ TeX-arg-define-macro-arguments ] t)
    '("renewrobustcmd*" TeX-arg-macro [ TeX-arg-define-macro-arguments ] t)
    '("providerobustcmd"  TeX-arg-define-macro [ TeX-arg-define-macro-arguments ] t)
    '("providerobustcmd*" TeX-arg-define-macro [ TeX-arg-define-macro-arguments ] t)

    ;; 2.2 Patching
    '("robustify" TeX-arg-macro)

    ;; 2.3 Protection
    '("protecting" t)

    ;; 2.4 Length and Counter Assignments
    '("defcounter" TeX-arg-counter t)
    '("deflength"  TeX-arg-length t)

    ;; 2.5 Additional Document Hooks
    '("AfterPreamble" t)
    '("AtEndPreamble" t)
    '("AfterEndPreamble" t)
    '("AfterEndDocument" t)

    ;; 2.6 Environment Hooks
    '("AtBeginEnvironment" TeX-arg-environment t)
    '("AtEndEnvironment" TeX-arg-environment t)
    '("BeforeBeginEnvironment" TeX-arg-environment t)
    '("AfterEndEnvironment" TeX-arg-environment t)

    ;; 3 Author Commands
    ;; 3.1.1 Macro Definitions
    '("csdef"  LaTeX-arg-etoolbox-csname [TeX-arg-free "Arguments"] t)
    '("csgdef" LaTeX-arg-etoolbox-csname [TeX-arg-free "Arguments"] t)
    '("csedef" LaTeX-arg-etoolbox-csname [TeX-arg-free "Arguments"] t)
    '("csxdef" LaTeX-arg-etoolbox-csname [TeX-arg-free "Arguments"] t)
    '("protected@csedef"
      LaTeX-arg-etoolbox-csname [TeX-arg-free "Arguments"] t)
    '("protected@csxdef"
      LaTeX-arg-etoolbox-csname [TeX-arg-free "Arguments"] t)
    '("cslet"   LaTeX-arg-etoolbox-csname TeX-arg-macro)
    '("letcs"   TeX-arg-macro LaTeX-arg-etoolbox-csname)
    '("csletcs" LaTeX-arg-etoolbox-csname LaTeX-arg-etoolbox-csname)
    '("csuse"   LaTeX-arg-etoolbox-csname)
    '("undef"   LaTeX-arg-etoolbox-macro-free)
    '("gundef"  LaTeX-arg-etoolbox-macro-free)
    '("csundef"   LaTeX-arg-etoolbox-csname)
    '("csgundef"  LaTeX-arg-etoolbox-csname)
    '("csmeaning" LaTeX-arg-etoolbox-csname)
    '("csshow"    LaTeX-arg-etoolbox-csname)

    ;; 3.1.2 Arithmetic Definitions
    '("numdef" LaTeX-arg-etoolbox-macro-free "Integer expression")
    '("numgdef" LaTeX-arg-etoolbox-macro-free "Integer expression")
    '("csnumdef" LaTeX-arg-etoolbox-csname "Integer expression")
    '("csnumgdef" LaTeX-arg-etoolbox-csname "Integer expression")

    '("dimdef" LaTeX-arg-etoolbox-macro-free "Dimension expression")
    '("dimgdef" LaTeX-arg-etoolbox-macro-free "Dimension expression")
    '("csdimdef" LaTeX-arg-etoolbox-csname "Dimension expression")
    '("csdimgdef" LaTeX-arg-etoolbox-csname "Dimension expression")

    '("gluedef" LaTeX-arg-etoolbox-macro-free "Glue expression")
    '("gluegdef" LaTeX-arg-etoolbox-macro-free "Glue expression")
    '("csgluedef" LaTeX-arg-etoolbox-csname "Glue expression")
    '("csgluegdef" LaTeX-arg-etoolbox-csname "Glue expression")

    '("mudef" LaTeX-arg-etoolbox-macro-free "Muglue expression")
    '("mugdef" LaTeX-arg-etoolbox-macro-free "Muglue expression")
    '("csmudef" LaTeX-arg-etoolbox-csname "Muglue expression")
    '("csmugdef" LaTeX-arg-etoolbox-csname "Muglue expression")

    ;; 3.2 Expansion Control
    '("expandonce" LaTeX-arg-etoolbox-macro-free)
    '("csexpandonce" LaTeX-arg-etoolbox-csname)

    ;; 3.3.1 Appending to a Hook
    '("appto" LaTeX-arg-etoolbox-macro-free t)
    '("gappto" LaTeX-arg-etoolbox-macro-free t)
    '("eappto" LaTeX-arg-etoolbox-macro-free t)
    '("xappto" LaTeX-arg-etoolbox-macro-free t)
    '("protected@eappto" LaTeX-arg-etoolbox-macro-free t)
    '("protected@xappto" LaTeX-arg-etoolbox-macro-free t)

    '("csappto" LaTeX-arg-etoolbox-csname t)
    '("csgappto" LaTeX-arg-etoolbox-csname t)
    '("cseappto" LaTeX-arg-etoolbox-csname t)
    '("csxappto" LaTeX-arg-etoolbox-csname t)
    '("protected@cseappto" LaTeX-arg-etoolbox-csname t)
    '("protected@csxappto" LaTeX-arg-etoolbox-csname t)

    ;; 3.3.2 Prepending to a Hook
    '("preto" LaTeX-arg-etoolbox-macro-free t)
    '("gpreto" LaTeX-arg-etoolbox-macro-free t)
    '("epreto" LaTeX-arg-etoolbox-macro-free t)
    '("xpreto" LaTeX-arg-etoolbox-macro-free t)
    '("protected@epreto" LaTeX-arg-etoolbox-macro-free t)
    '("protected@xpreto" LaTeX-arg-etoolbox-macro-free t)

    '("cspreto" LaTeX-arg-etoolbox-csname t)
    '("csgpreto" LaTeX-arg-etoolbox-csname t)
    '("csepreto" LaTeX-arg-etoolbox-csname t)
    '("csxpreto" LaTeX-arg-etoolbox-csname t)
    '("protected@csepreto" LaTeX-arg-etoolbox-csname t)
    '("protected@csxpreto" LaTeX-arg-etoolbox-csname t)

    ;; 3.4 Patching

    '("patchcmd" ["Prefix"] TeX-arg-macro 4)
    '("ifpatchable" TeX-arg-macro 3)
    '("ifpatchable*" TeX-arg-macro 2)
    '("apptocmd" TeX-arg-macro 3)
    '("pretocmd" TeX-arg-macro 3)
    "tracingpatches"

    ;; 3.5.1 TeX Flags
    '("newbool"     "Name")
    '("providebool" "Name")
    '("booltrue"    "Name")
    '("boolfalse"   "Name")
    '("setbool"     "Name" (TeX-arg-completing-read ("true" "false") "Value"))
    '("ifbool"      "Name" 2)
    '("notbool"     "Name" 2)

    ;; 3.5.2 LaTeX Flags
    '("newtoggle"     "Name")
    '("providetoggle" "Name")
    '("toggletrue"    "Name")
    '("togglefalse"   "Name")
    '("settoggle"     "Name" (TeX-arg-completing-read ("true" "false") "Value"))
    '("iftoggle"      "Name" 2)
    '("nottoggle"     "Name" 2)

    ;; 3.6.1 Macro Tests
    '("ifdef" TeX-arg-macro 2)
    '("ifcsdef" LaTeX-arg-etoolbox-csname 2)
    '("ifundef" TeX-arg-macro 2)
    '("ifcsundef" LaTeX-arg-etoolbox-csname 2)
    '("ifdefmacro" TeX-arg-macro 2)
    '("ifcsmacro" LaTeX-arg-etoolbox-csname 2)
    '("ifdefparam" TeX-arg-macro 2)
    '("ifcsparam" LaTeX-arg-etoolbox-csname 2)
    '("ifdefprefix" TeX-arg-macro 2)
    '("ifcsprefix" LaTeX-arg-etoolbox-csname 2)
    '("ifdefprotected" TeX-arg-macro 2)
    '("ifcsprotected" LaTeX-arg-etoolbox-csname 2)
    '("ifdefltxprotected" TeX-arg-macro 2)
    '("ifcsltxprotected" LaTeX-arg-etoolbox-csname 2)
    '("ifdefempty" TeX-arg-macro 2)
    '("ifcsempty" LaTeX-arg-etoolbox-csname 2)
    '("ifdefvoid" TeX-arg-macro 2)
    '("ifcsvoid" LaTeX-arg-etoolbox-csname 2)
    '("ifdefequal" TeX-arg-macro TeX-arg-macro 2)
    '("ifcsequal" LaTeX-arg-etoolbox-csname LaTeX-arg-etoolbox-csname 2)
    '("ifdefstring" TeX-arg-macro 3)
    '("ifcsstring" LaTeX-arg-etoolbox-csname 3)
    '("ifdefstrequal" TeX-arg-macro TeX-arg-macro 2)
    '("ifcsstrequal" LaTeX-arg-etoolbox-csname LaTeX-arg-etoolbox-csname 2)

    ;; 3.6.2 Counter and Length Tests
    '("ifdefcounter" TeX-arg-macro 2)
    '("ifcscounter" LaTeX-arg-etoolbox-csname 2)
    '("ifltxcounter" TeX-arg-counter 2)
    '("ifdeflength" TeX-arg-length 2)
    '("ifcslength" (TeX-arg-completing-read (LaTeX-length-list) "Length") 2)
    '("ifdefdimen" TeX-arg-macro 2)
    '("ifcsdimen" "Control sequence" 2)

    ;; 3.6.3 String Tests
    '("ifstrequal" 4)
    '("ifstrempty" 3)
    '("ifblank" 3)
    '("notblank" 3)

    ;; 3.6.4 Arithmetic Tests
    '("ifnumcomp" 5)
    '("ifnumequal" 4)
    '("ifnumgreater" 4)
    '("ifnumless" 4)
    '("ifnumodd" 3)

    '("ifdimcomp" 5)
    '("ifdimequal" 4)
    '("ifdimgreater" 4)
    '("ifdimless" 4)

    ;; 3.6.5 Boolean Expressions
    '("ifboolexpr" 3)
    '("ifboolexpe" 3)
    '("whileboolexpr" 2)
    '("unlessboolexpr" 2)

    ;; 3.7 List Processing
    ;; 3.7.1 User Input
    '("DeclareListParser" TeX-arg-define-macro "Separator")
    '("DeclareListParser*" TeX-arg-define-macro "Separator")
    '("docsvlist" t)
    '("forcsvlist" 2)

    ;; 3.7.2 Internal Lists
    '("listadd" TeX-arg-macro t)
    '("listdadd" TeX-arg-macro t)
    '("listeadd" TeX-arg-macro t)
    '("listxadd" TeX-arg-macro t)

    '("listcsadd" LaTeX-arg-etoolbox-csname t)
    '("listcsgadd" LaTeX-arg-etoolbox-csname t)
    '("listcseadd" LaTeX-arg-etoolbox-csname t)
    '("listcsxadd" LaTeX-arg-etoolbox-csname t)

    '("listremove" TeX-arg-macro t)
    '("listgremove" TeX-arg-macro t)
    '("listcsremove" LaTeX-arg-etoolbox-csname t)
    '("listcsgremove" LaTeX-arg-etoolbox-csname t)

    '("dolistlool" TeX-arg-macro)
    '("dolistcslool" LaTeX-arg-etoolbox-csname)

    '("forlistloop" t TeX-arg-macro)
    '("forlistcsloop" t LaTeX-arg-etoolbox-csname)

    '("ifinlist" t TeX-arg-macro nil nil)
    '("xifinlist" t TeX-arg-macro nil nil)

    '("ifinlistcs" t LaTeX-arg-etoolbox-csname nil nil)
    '("xifinlistcs" t LaTeX-arg-etoolbox-csname nil nil)

    ;; 3.8 Miscellaneous Tools
    '("rmntonum" "Roman numeral")
    '("ifrmnum" 3))

   ;; Add the exceptions to `LaTeX-indent-begin-exceptions-list' and
   ;; run `LaTeX-indent-commands-regexp-make':
   (let ((exceptions '("ifpatchable"
                       "ifbool"
                       "iftoggle"
                       "ifdef"
                       "ifcsdef"
                       "ifundef"
                       "ifcsundef"
                       "ifdefmacro"
                       "ifcsmacro"
                       "ifdefparam"
                       "ifcsparam"
                       "ifdefprefix"
                       "ifcsprefix"
                       "ifdefprotected"
                       "ifcsprotected"
                       "ifdefltxprotected"
                       "ifcsltxprotected"
                       "ifdefempty"
                       "ifcsempty"
                       "ifdefvoid"
                       "ifcsvoid"
                       "ifdefequal"
                       "ifcsequal"
                       "ifdefstring"
                       "ifcsstring"
                       "ifdefstrequal"
                       "ifcsstrequal"
                       "ifdefcounter"
                       "ifcscounter"
                       "ifltxcounter"
                       "ifdeflength"
                       "ifcslength"
                       "ifdefdimen"
                       "ifcsdimen"
                       "ifstrequal"
                       "ifstrempty"
                       "ifblank"
                       "ifnumcomp"
                       "ifnumequal"
                       "ifnumgreater"
                       "ifnumless"
                       "ifnumodd"
                       "ifdimcomp"
                       "ifdimequal"
                       "ifdimgreater"
                       "ifdimless"
                       "ifboolexpr"
                       "ifboolexpe"
                       "ifinlist"
                       "ifinlistcs"
                       "ifrmnum")))
     (dolist (elt exceptions)
       (add-to-list 'LaTeX-indent-begin-exceptions-list elt t))
     (LaTeX-indent-commands-regexp-make))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newrobustcmd"     "*|{\\[[{")
                                ("renewrobustcmd"   "*|{\\[[{")
                                ("providerobustcmd" "*|{\\[[{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-etoolbox-package-options nil
  "Package options for the etoolbox package.")

;;; etoolbox.el ends here
