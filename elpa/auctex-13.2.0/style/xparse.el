;;; xparse.el --- AUCTeX style for `xparse.sty' version 2022-07-05  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2020--2022 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <mose@gnu.org>
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

;; This file adds basic support for `xparse.sty' version 2022-07-05.
;; It parses argument specification of macros and environments.

;; The "yet not more supported" specifiers `l', `u', `g' and `G' are
;; ignored completely and may lead to wrong parsing results.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-auto-add-type "xparse-macro" "LaTeX")

(defvar LaTeX-xparse-macro-regexp
  `(,(concat
      (regexp-quote TeX-esc)
      "\\(New\\|Renew\\|Provide\\|Declare\\)"
      "\\(?:Expandable\\)?"
      "DocumentCommand"
      "[ \t\n\r]*"
      "{?"
      "[ \t\n\r]*"
      (regexp-quote TeX-esc)
      "\\([A-Za-z]+\\)"
      "[ \t\n\r]*"
      "}?"
      "[ \t\n\r]*"
      "{\\([^}{]*\\({[^}{]*\\({[^}{]*\\({[^}{]*}[^}{]*\\)*}[^}{]*\\)*}[^}{]*\\)*\\)}")
    (0 2 3 1) LaTeX-auto-xparse-macro)
  "Matches macros by xparse package.")

(TeX-auto-add-type "xparse-environment" "LaTeX")

(defvar LaTeX-xparse-environment-regexp
  `(,(concat
      (regexp-quote TeX-esc)
      "\\(New\\|Renew\\|Provide\\|Declare\\)"
      "DocumentEnvironment"
      "[ \t\n\r]*"
      "{"
      "[ \t\n\r]*"
      "\\([A-Za-z]+\\)"
      "[ \t\n\r]*"
      "}"
      "[ \t\n\r]*"
      "{\\([^}{]*\\({[^}{]*\\({[^}{]*\\({[^}{]*}[^}{]*\\)*}[^}{]*\\)*}[^}{]*\\)*\\)}")
    (0 2 3 1) LaTeX-auto-xparse-environment)
  "Matches environments by xparse package.")

(defun LaTeX-arg-xparse-query (optional op-brace cl-brace &optional prompt)
  "Special query function for parsed elements from xparse package.
If OPTIONAL is non-nil, indicate it in minibuffer.  OP-BRACE sets
the opening brace, CL-BRACE sets the closing one.  PROMPT
replaces the standard one."
  (let ((TeX-arg-opening-brace op-brace)
        (TeX-arg-closing-brace cl-brace))
    (TeX-argument-insert
     (TeX-read-string (TeX-argument-prompt optional prompt "Text"))
     optional)))

(defun LaTeX-arg-xparse-embellishment-query (_optional embellish)
  "Special insert function for embellishments from xparse package.
Compatibility argument OPTIONAL is ignored.  EMBELLISH is a
string with parsed elements inserted in the buffer.  This
function also sets the value of `TeX-exit-mark' where the point
will be once the insertion is completed."
  (let (p)
    (just-one-space)
    (setq p (point))
    (insert embellish)
    (set-marker TeX-exit-mark (1+ p))))

(defun LaTeX-xparse-macro-parse (type)
  "Process parsed macro and environment definitions.
TYPE is one of the symbols mac or env."
  (dolist (xcmd (if (eq type 'mac)
                    (LaTeX-xparse-macro-list)
                  (LaTeX-xparse-environment-list)))
    (let ((name (nth 1 xcmd))
          (spec (nth 2 xcmd))
          (what (nth 3 xcmd))
          args opt-star opt-token)
      (with-temp-buffer
        (set-syntax-table LaTeX-mode-syntax-table)
        ;; This one is probably not really needed?
        (goto-char (point-min))
        (insert (replace-regexp-in-string "[ \t\r\n%]" "" spec))
        (goto-char (point-min))
        (while (looking-at-p "[+!>=bmrRvodODsteE]")
          (cond ((looking-at-p "[+!b]")
                 ;; + or !: Long argument or space aware: Move over
                 ;; them.  b is special; only available for
                 ;; enviroments
                 (forward-char 1))
                ;; Argument processors and key-val modifier: Move
                ;; over [>=] and a balanced {}
                ((looking-at-p "[>=]")
                 (forward-char 1)
                 (forward-sexp))
                ;; Mandatory arguments:
                ;; m: Ask for input with "Text" as prompt
                ((looking-at-p "m")
                 (forward-char 1)
                 (push "Text" args))
                ;; r<token1><token2>
                ((looking-at-p "r")
                 (re-search-forward "r\\(.\\)\\(.\\)" (+ (point) 3) t)
                 (push `(LaTeX-arg-xparse-query
                         ,(match-string-no-properties 1)
                         ,(match-string-no-properties 2))
                       args))
                ;; R<token1><token2>{default}
                ((looking-at-p "R")
                 (re-search-forward "R\\(.\\)\\(.\\)" (+ (point) 3) t)
                 (forward-sexp)
                 (push `(LaTeX-arg-xparse-query
                         ,(match-string-no-properties 1)
                         ,(match-string-no-properties 2))
                       args))
                ;; v: Use `TeX-arg-verb-delim-or-brace'
                ((looking-at-p "v")
                 (forward-char 1)
                 (push #'TeX-arg-verb-delim-or-brace args))
                ;; Optional arguments:
                ;; o standard LaTeX optional in square brackets
                ((looking-at-p "o")
                 (forward-char 1)
                 (push (vector "Text") args))
                ;; d<token1><token2>
                ((looking-at-p "d")
                 (re-search-forward "d\\(.\\)\\(.\\)" (+ (point) 3) t)
                 (push (vector #'LaTeX-arg-xparse-query
                               (match-string-no-properties 1)
                               (match-string-no-properties 2))
                       args))
                ;; O{default}
                ((looking-at-p "O")
                 (forward-char 1)
                 (forward-sexp)
                 (push (vector "Text") args))
                ;; D<token1><token2>{default}
                ((looking-at-p "D")
                 (re-search-forward "D\\(.\\)\\(.\\)" (+ (point) 3) t)
                 (forward-sexp)
                 (push (vector #'LaTeX-arg-xparse-query
                               (match-string-no-properties 1)
                               (match-string-no-properties 2))
                       args))
                ;; s: optional star
                ((looking-at-p "s")
                 (forward-char 1)
                 (setq opt-star t))
                ;; t: optional <token>
                ((looking-at-p "t")
                 (re-search-forward "t\\(.\\)" (+ (point) 2) t)
                 (setq opt-token (match-string-no-properties 1)))
                ;; e{tokes} a set of optional embellishments
                ((looking-at-p "e")
                 (forward-char)
                 (if (looking-at-p TeX-grop)
                     (re-search-forward "{\\([^}]+\\)}" nil t)
                   (re-search-forward "\\(.\\)" (1+ (point)) t))
                 (push `(LaTeX-arg-xparse-embellishment-query
                         ,(match-string-no-properties 1))
                       args))
                ;; E{tokes}{defaults}
                ((looking-at-p "E")
                 (forward-char)
                 (if (looking-at-p TeX-grop)
                     (re-search-forward "{\\([^}]+\\)}" nil t)
                   (re-search-forward "\\(.\\)" (1+ (point)) t))
                 (push `(LaTeX-arg-xparse-embellishment-query
                         ,(match-string-no-properties 1))
                       args)
                 (when (looking-at-p TeX-grop)
                   (forward-sexp)))
                ;; Finished:
                (t nil))))
      (if (eq type 'env)
          ;; Parsed enviroments: If we are Renew'ing or Delare'ing, we
          ;; delete the enviroment first from `LaTeX-environment-list'
          ;; before adding the new one.  We have to sort the value of
          ;; `LaTeX-environment-list' by running the function of the
          ;; same name:
          (progn
            (when (member what '("Renew" "Declare"))
              (LaTeX-environment-list)
              (setq LaTeX-environment-list
                    (assq-delete-all (car (assoc name LaTeX-environment-list))
                                     LaTeX-environment-list)))
            (LaTeX-add-environments `(,name
                                      LaTeX-env-args
                                      ,@(reverse args))))
        ;; Parsed macros: If we are Renew'ing or Delare'ing, we delete
        ;; the macros first from `TeX-symbol-list' before adding the
        ;; new ones.  We have to sort the value of `TeX-symbol-list'
        ;; by running the function of the same name:
        (when (member what '("Renew" "Declare"))
          (TeX-symbol-list)
          (setq TeX-symbol-list
                (assq-delete-all (car (assoc name TeX-symbol-list))
                                 TeX-symbol-list))
          (when opt-star
            (setq TeX-symbol-list
                  (assq-delete-all (car (assoc (concat name "*") TeX-symbol-list))
                                   TeX-symbol-list)))
          (when opt-token
            (setq TeX-symbol-list
                  (assq-delete-all (car (assoc (concat name opt-token) TeX-symbol-list))
                                   TeX-symbol-list))))
        (TeX-add-symbols (cons name
                               (reverse args)))
        (when opt-star
          (TeX-add-symbols (cons (concat name "*")
                                 (reverse args))))
        (when opt-token
          (TeX-add-symbols (cons (concat name opt-token)
                                 (reverse args))))))))

(defun LaTeX-xparse-auto-prepare ()
  "Clear various `LaTeX-auto-xparse-*' variables before parsing."
  (setq LaTeX-auto-xparse-macro nil
        LaTeX-auto-xparse-environment nil))

(defun LaTeX-xparse-auto-cleanup ()
  "Process parsed elements for xparse package."
  (LaTeX-xparse-macro-parse 'mac)
  (LaTeX-xparse-macro-parse 'env))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-xparse-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-xparse-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "xparse"
 (lambda ()
   (TeX-auto-add-regexp LaTeX-xparse-macro-regexp)
   (TeX-auto-add-regexp LaTeX-xparse-environment-regexp)
   (TeX-run-style-hooks
    "expl3")
   (TeX-add-symbols
    ;; Declaring commands
    '("DeclareDocumentCommand"
      TeX-arg-define-macro "Argument specification" t)
    '("NewDocumentCommand"
      TeX-arg-define-macro "Argument specification" t)
    '("RenewDocumentCommand"
      TeX-arg-macro "Argument specification" t)
    '("ProvideDocumentCommand"
      TeX-arg-define-macro "Argument specification" t)

    ;; Declaring commands and environments
    '("DeclareDocumentEnvironment" TeX-arg-define-environment
      "Argument specification" t t)
    '("NewDocumentEnvironment" TeX-arg-define-environment
      "Argument specification" t t)
    '("RenewDocumentEnvironment" TeX-arg-environment
      "Argument specification" t t)
    '("ProvideDocumentEnvironment" TeX-arg-define-environment
      "Argument specification" t t)

    ;; Fully-expandable document commands
    '("DeclareExpandableDocumentCommand"
      TeX-arg-define-macro "Argument specification" t)
    '("NewExpandableDocumentCommand"
      TeX-arg-define-macro "Argument specification" t)
    '("RenewExpandableDocumentCommand"
      TeX-arg-macro "Argument specification" t)
    '("ProvideExpandableDocumentCommand"
      TeX-arg-define-macro "Argument specification" t)

    ;; Testing special values
    '("IfBooleanTF" 3)
    '("IfBooleanT" 2)
    '("IfBooleanF" 2)
    '("IfNoValueTF" 3)
    '("IfNoValueT" 2)
    '("IfNoValueF" 2)
    '("IfValueTF" 3)
    '("IfValueT" 2)
    '("IfValueF" 2)
    '("IfBlankTF" 3)
    '("IfBlankT" 2)
    '("IfBlankF" 2)
    "BooleanTrue"
    "BooleanFalse"
    ;; Argument processors
    "ProcessedArgument"
    "ReverseBoolean"
    '("SplitArgument" "Number" "Token")
    '("SplitList" "Token")
    "TrimSpaces"
    '("ProcessList" "List" "Function")
    ;; Access to the argument specification
    '("GetDocumentCommandArgSpec" TeX-arg-macro)
    '("GetDocumentEnvironmmentArgSpec" TeX-arg-environment)
    '("ShowDocumentCommandArgSpec" TeX-arg-macro)
    '("ShowDocumentEnvironmentArgSpec" TeX-arg-environment))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("DeclareDocumentCommand" "|{\\{{")
                                ("NewDocumentCommand"     "|{\\{{")
                                ("ProvideDocumentCommand" "|{\\{{")
                                ("RenewDocumentCommand"   "|{\\{{")
                                ;;
                                ("DeclareExpandableDocumentCommand" "|{\\{{")
                                ("NewExpandableDocumentCommand"     "|{\\{{")
                                ("ProvideExpandableDocumentCommand" "|{\\{{")
                                ("RenewExpandableDocumentCommand"   "|{\\{{")
                                ;;
                                ("DeclareDocumentEnvironment" "{{{{")
                                ("NewDocumentEnvironment"     "{{{{")
                                ("ProvideDocumentEnvironment" "{{{{")
                                ("RenewDocumentEnvironment"   "{{{{"))
                              'function)))
 TeX-dialect)

(defun LaTeX-xparse-package-options ()
  "Read the xparse package options from the user."
  (TeX-read-key-val t '(("log-declarations" ("true" "false")))))

;;; xparse.el ends here
