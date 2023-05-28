;;; doc.el --- AUCTeX style for `doc.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2022  Free Software Foundation, Inc.

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

;; This file adds support for `doc.sty' (v3.0h) dated 2022/06/01.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defun LaTeX-env-doc-no-comment (environment)
  "Insert ENVIRONMENT and make sure there is no commented empty line inside."
  (LaTeX-insert-environment environment)
  (unless (TeX-active-mark)
    (when (save-excursion
            (beginning-of-line)
            (looking-at (concat "[ \t]+$\\|[ \t]*"
                                TeX-comment-start-regexp "+[ \t]*$")))
      (delete-region (line-beginning-position) (line-end-position))
      (indent-according-to-mode))))

(defun LaTeX-env-doc-commented (environment)
  "Insert ENVIRONMENT and make sure all parts are in comments.
This functions search for the begin and the end of the inserted
environment and makes sure those parts are in comments.  The same
applies also to the point inside after the insertion."
  (LaTeX-insert-environment
   environment
   (if (string= environment "macro")
       ;; For 'macro' environment, elements will start with a
       ;; backslash, so we insert them initially:
       (progn
         (let ((opt (mapconcat #'identity
                               (TeX-completing-read-multiple
                                (TeX-argument-prompt t nil "Suppress option")
                                '("noindex" "noprint"))
                               ","))
               (mac (TeX-read-string
                     (TeX-argument-prompt nil nil "Macro(s)")
                     TeX-esc)))
           (concat (when (and opt (not (string= opt "")))
                     (format "[%s]" opt))
                   (format "{%s}" mac))))
     ;; For other environments, we don't know about the elements,
     ;; so do nothing.  For 'environment', we adjust the prompt in
     ;; minibuffer.
     (let ((opt (mapconcat #'identity
                           (TeX-completing-read-multiple
                            (TeX-argument-prompt t nil "Suppress option")
                            '("noindex" "noprint"))
                           ","))
           (env (TeX-read-string
                 (TeX-argument-prompt nil nil
                                      (if (string= environment "environment")
                                          "Environment(s)"
                                        "Element(s)")))))
       (concat
        (when (and opt (not (string= opt "")))
          (format "[%s]" opt))
        (format "{%s}" env)))))
  ;; Now make sure everything is commented:
  (let ((p (point-marker))
        (active-mark (and (TeX-active-mark)
                          (not (eq (mark) (point)))))
        (func (lambda ()
                (if (TeX-in-line-comment)
                    (indent-according-to-mode)
                  (delete-horizontal-space)
                  (beginning-of-line)
                  (insert "%")
                  (indent-according-to-mode)))))
    ;; Go to the start of the env we have inserted:
    (search-backward (concat "\\begin" TeX-grop environment TeX-grcl)
                     (if active-mark nil (line-beginning-position 0))
                     t)
    ;; If the line is not commented, insert %
    (funcall func)
    (goto-char p)
    ;; Do the same for the end of the environment
    (search-forward (concat "\\end" TeX-grop environment TeX-grcl)
                    (if active-mark nil (line-end-position 2))
                    t)
    (goto-char (match-beginning 0))
    (funcall func)
    ;; Finally for where we started and clean up only when region was
    ;; not active:
    (goto-char p)
    (unless active-mark (funcall func))
    (set-marker p nil)))

(defun LaTeX-doc-after-insert-macrocode (env start end)
  "Make sure the macrocode environment is properly formatted after insertion."
  (when (TeX-member env '("macrocode" "macrocode*")
                    #'string-equal)
    (save-excursion
      (goto-char end)
      (skip-chars-backward " \t")
      (when (bolp)
        (insert "%")
        (indent-according-to-mode))
      (goto-char start)
      (skip-chars-backward " \t")
      (when (bolp)
        (insert "%")
        (indent-according-to-mode)))))

(defvar LaTeX-doc-newdocelement-key-val-options
  '(("macrolike" ("true" "false"))
    ("envlike" ("true" "false"))
    ("toplevel" ("true" "false"))
    ("notoplevel" ("true" "false"))
    ("idxtype")
    ("printtype")
    ("idxgroup")
    ("noindex" ("true" "false"))
    ("noprint" ("true" "false")))
  "Key=value options for '\\NewDocElement' macro.")

;; Setup for \NewDocElement:

(TeX-auto-add-type "doc-NewDocElement" "LaTeX")

(defvar LaTeX-doc-NewDocElement-regexp
  `(,(concat  "^[ \t%]*"
              "\\\\NewDocElement"
              "[ \t\n\r%]*"
              "\\(?:"
              (LaTeX-extract-key-value-label 'none)
              "\\)?"
              "[ \t\n\r%]*"
              "{\\([^}]+\\)}"
              "[ \t\n\r%]*"
              "{\\([^}]+\\)}")
    (1 2) LaTeX-auto-doc-NewDocElement)
  "Matches the arguments of '\\NewDocElement' from doc package.
AUCTeX parser doesn't look for text parts commented out.
Therefore, the regexp in this variable explicitly looks for a
percent sign at the beginning of a line before
'\\NewDocElement'.")

(defun LaTeX-doc-auto-prepare ()
  "Clear `LaTeX-auto-doc-NewDocElement' before parsing."
  (setq LaTeX-auto-doc-NewDocElement nil))

(defun LaTeX-doc-auto-cleanup ()
  "Process elements defined with '\\NewDocElement'."
  (when (LaTeX-doc-NewDocElement-list)
    ;; Make sure `docTeX-indent-inner-fixed' is local:
    (make-local-variable 'docTeX-indent-inner-fixed)

    ;; \NewDocElement[<options>]{<element-name>}{<env-name>} defines:
    ;; 1. \Describe<element-name>[<options>]{<element>}
    ;; 2. \begin{<env-name>}[<options>]{<element>}
    ;; 3. \PrintDescribe<element-name>{<element>}
    ;; 4. \Print<element-name>Name
    (let (macs)
      (dolist (elt (LaTeX-doc-NewDocElement-list))
        (let ((eltname (car elt))
              (envname (cadr elt)))
          (TeX-add-symbols
           ;; Cater for \Describe<eltname>[options]{<elements query>}
           `(,(concat "Describe" eltname)
             [TeX-arg-completing-read ("noindex" "noprint") "Suppress option"]
             "Element")

           ;; Cater for \PrintDescribe<eltname>{<elements query>}
           `(,(concat "PrintDescribe" eltname) "Element")

           ;; Cater for \Print<eltname>Name
           (concat "Print" eltname "Name"))

          ;; Add the \Describe<element-name> to macs
          (push (concat "Describe" eltname) macs)

          ;; Cater for \begin{<envname>}[options]{<elements query>}
          (LaTeX-add-environments
           `(,envname LaTeX-env-doc-commented))

          ;; Make sure we have fixed inner indent for our environments:
          (add-to-list 'docTeX-indent-inner-fixed
                       `(,(concat (regexp-quote TeX-esc)
                                  "\\(begin\\|end\\)[ \t]*"
                                  (regexp-quote TeX-grop)
                                  envname
                                  (regexp-quote TeX-grcl))
                         0 nil)
                       t)
          ;; Add fontification:
          (when (and (featurep 'font-latex)
                     (eq TeX-install-font-lock 'font-latex-setup))
            (font-latex-add-keywords `((,(concat "Describe" eltname) "[|{\\" ))
                                     'variable))))

      ;; Let \Describe<element-name> stay in their own lines:
      (LaTeX-paragraph-commands-add-locally macs))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-doc-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-doc-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "doc"
 (lambda ()

   ;; Add doc to the parser:
   (TeX-auto-add-regexp LaTeX-doc-NewDocElement-regexp)

   (add-hook 'LaTeX-after-insert-env-hook #'LaTeX-doc-after-insert-macrocode
             nil t)

   (LaTeX-add-environments
    ;; 2.3 General conventions
    '("macrocode"  LaTeX-env-doc-no-comment)
    '("macrocode*" LaTeX-env-doc-no-comment)

    ;; 2.5 Describing the definition of macros and environments
    '("macro"       LaTeX-env-doc-commented)
    '("environment" LaTeX-env-doc-commented)

    '("theglossary" LaTeX-env-item))

   (TeX-add-symbols
    ;; 2.1 The driver file: Note that `l3doc.el' also loads `doc.el'
    ;; but `\DocInput' behaves differently.  With `l3doc.el', it takes
    ;; comma-separated arguments, with `doc.el' it takes only one
    ;; argument.
    `("DocInput"
      (TeX-arg-conditional (member "l3doc" (TeX-style-list))
          (,(lambda (optional)
              (let ((file (TeX-read-string
                           (TeX-argument-prompt optional nil
                                                (format "File(s) to input (default %s)"
                                                        (buffer-name)))
                           nil nil (buffer-name))))
                (TeX-argument-insert file optional))))
        (,(lambda (optional)
            (let ((file (file-relative-name
                         (read-file-name
                          (TeX-argument-prompt optional nil
                                               (format "File to input (default %s)"
                                                       (buffer-name)))
                          nil (buffer-name) nil nil
                          (lambda (x)
                            (or (file-directory-p x)
                                (string-match "\\.\\(fdd\\|dtx\\)\\'" x))))
                         (TeX-master-directory))))
              (TeX-argument-insert file optional))))))

    `("IndexInput"
      ,(lambda (optional)
         (let ((file (file-relative-name
                      (read-file-name
                       (TeX-argument-prompt optional nil "File to input")
                       nil nil nil nil
                       (lambda (x)
                         (or (file-directory-p x)
                             (string-match "\\.\\(tex\\|ltx\\|fdd\\|dtx\\)\\'" x))))
                      (TeX-master-directory))))
           (TeX-argument-insert file optional))))

    ;; 2.2 Package options
    '("SetupDoc" (TeX-arg-completing-read-multiple LaTeX-doc-package-options
                                                   "Options"))

    ;; 2.4 Describing the usage of macros and environments
    `("DescribeMacro"
      [TeX-arg-completing-read ("noindex" "noprint") "Suppress option"]
      (TeX-arg-string "Macro" ,TeX-esc))
    '("DescribeEnv"
      [TeX-arg-completing-read ("noindex" "noprint") "Suppress option"]
      "Environment")

    ;; 2.5 Describing the definition of macros and environments
    "MacroFont"

    ;; 2.6 Formatting names in the margin
    '("PrintDescribeMacro" "Element")
    '("PrintDescribeEnv"   "Element")
    "PrintMacroName"
    "PrintEnvName"

    ;; 2.7 Providing further documentation items
    '("NewDocElement"
      [TeX-arg-key-val LaTeX-doc-newdocelement-key-val-options]
      "Element name" "Environment name")

    ;; 2.8 Displaying sample code verbatim
    ;; "verbatim" environment and "verb" macro are provided by
    ;; latex.el, so we don't add them here again.

    ;; 2.9 Using a special escape character
    '("SpecialEscapechar" "Character")

    ;; 2.10 Cross-referencing all macros used
    "DisableCrossrefs"
    "EnableCrossrefs"
    ;; We don't fontify the next macro since it is a one-liner anyway
    '("DoNotIndex" t)
    "CodelineIndex"
    "PageIndex"
    "theCodelineNo"
    "CodelineNumbered"

    ;; 2.11 Producing the actual index entries
    "actualchar"
    "quotechar"
    "encapchar"
    "levelchar"

    "SpecialMainMacroIndex"
    "SpecialMainEnvIndex"
    "SpecialMacroIndex"
    "SpecialEnvIndex"
    "SpecialIndex"
    "SpecialShortIndex"
    "SortIndex"
    "verbatimchar"

    "subitem"
    "subsubitem"
    "indexspace"
    "efill"
    "pfill"

    ;; 2.12 Setting the index entries: theindex environment is
    ;; provided by latex.el.
    "PrintIndex"
    '("IndexPrologue" t)
    "IndexParms"
    "main"
    "usage"
    "code"

    ;; 2.13 Changing the default values of style parameters
    "DocstyleParms"

    ;; 2.14 Short input of verbatim text pieces: These macros are
    ;; provided by 'shortvrb.el' which is run later

    ;; 2.15 Additional bells and whistles
    "Web"
    "AmSTeX"
    "BibTeX"
    "SliTeX"
    "PlainTeX"
    '("meta" "Text")
    "OnlyDescription"
    '("StopEventually" t)
    '("MaybeStop" t)
    "Finale"
    "AlsoImplementation"
    "IndexInput"
    '("changes" "version" TeX-arg-date t)
    "generalname"
    "RecordChanges"
    "PrintChanges"
    "GlossaryPrologue"
    "GlossaryParms"
    "bslash"
    "MakePrivateLetters"
    "DontCheckModules"
    "CheckModules"
    "Module"
    '("AltMacroFont" t)

    ;; 5.1 makeindex bugs
    "PercentIndex"
    ;; 5.2 File transmission issues
    '("CheckSum" t)
    '("CharacterTable" t))

   (TeX-run-style-hooks "shortvrb")

   (LaTeX-add-lengths "MacrocodeTopsep" "MacroTopsep" "MacroIndent"
                      "IndexMin" "GlossaryMin")
   (LaTeX-add-counters "IndexColumns" "GlossaryColumns" "StandardModuleDepth")

   ;; Macros which should be on their own line:
   (LaTeX-paragraph-commands-add-locally '("DescribeEnv"
                                           "DescribeMacro"
                                           "changes"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("SetupDoc"           "{")
                                ("NewDocElement"      "[{{")
                                ("RenewDocElement"    "[{{")
                                ("SpecialEscapechar"  "{")
                                ("DisableCrossrefs"   "")
                                ("EnableCrossrefs"    "")
                                ("CodelineIndex"      "")
                                ("PageIndex"          "")
                                ("CodelineNumbered"   "")
                                ("PrintIndex"         "")
                                ("IndexPrologue"      "")
                                ("AmSTeX"             "")
                                ("BibTeX"             "")
                                ("SliTeX"             "")
                                ("PlainTeX"           "")
                                ("OnlyDescription"    "")
                                ("StopEventually"     "")
                                ("MaybeStop"          "")
                                ("Finale"             "")
                                ("AlsoImplementation" "")
                                ("changes"            "{{{")
                                ("PrintChanges"       "")
                                ("RecordChanges"      ""))
                              'function)
     (font-latex-add-keywords '(("DescribeMacro" "[|{\\")
                                ("DescribeEnv"   "[{"))
                              'variable)
     (font-latex-add-keywords '(("meta"       "{"))
                              'textual)
     (font-latex-add-keywords '(("DocInput"   "{")
                                ("IndexInput" "{"))
                              'reference)))
 TeX-dialect)

(defvar LaTeX-doc-package-options
  '("hyperref" "nohyperref"
    "multicol" "nomulticol"
    "debugshow"
    "noindex" "noprint"
    "reportchangedates")
  "Package options for the doc package.")

;; Local Variables:
;; coding: utf-8
;; End:

;;; doc.el ends here
