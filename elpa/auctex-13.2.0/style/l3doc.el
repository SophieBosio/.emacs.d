;;; l3doc.el --- AUCTeX style for `l3doc.cls'  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-03-05
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

;; This file adds support for `l3doc.cls' dated 2022/02/24.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(defvar LaTeX-article-class-options)

(defvar LaTeX-l3doc-cmd-key-val-options
  '(("index")
    ("noindex")
    ("module" ("TeX"))
    ("replace" ("true" "false")))
  "Key=value options for l3doc macros.")

(defun LaTeX-env-l3doc-function (environment)
  "Insert the ENVIRONMENT provided by l3doc.cls.
This function should be used for the environments \"function\" and
\"variable\" provided by the l3doc class.  Also ask the user if a
\"syntax\" environment should be added as well."
  (let* ((time (format-time-string "%Y-%m-%d" (current-time)))
         (exp-flag (TeX-read-key-val t `(("added" (,time))
                                         ("updated" (,time))
                                         ("deprecated")
                                         ("tested")
                                         ("EXP")
                                         ("rEXP")
                                         ("TF")
                                         ("pTF")
                                         ("noTF")
                                         ("label")
                                         ("verb"))
                                     "Flags (k=v)"))
         (mac (TeX-read-string
               (TeX-argument-prompt nil nil "Macro(s)")
               TeX-esc))
         (active-mark (and (TeX-active-mark)
                           (not (eq (mark) (point)))))
         (elt-count 0)
         (count 1)
         (comment-func (lambda ()
                         (if (TeX-in-line-comment)
                             (indent-according-to-mode)
                           (delete-horizontal-space)
                           (beginning-of-line)
                           (insert "%")
                           (indent-according-to-mode))))
         p)
    (LaTeX-insert-environment environment
                              (concat
                               (unless (zerop (length exp-flag))
                                 (format "[%s]" exp-flag))
                               TeX-grop mac TeX-grcl))
    ;; Remember where we start:
    (setq p (point-marker))
    ;; Now make sure we have '%' everywhere, start at the beginning:
    (search-backward (concat TeX-esc "begin" TeX-grop environment TeX-grcl)
                     (if active-mark nil (line-beginning-position 0))
                     t)
    (funcall comment-func)
    ;; Now at the end:
    (goto-char p)
    (search-forward (concat TeX-esc "end" TeX-grop environment TeX-grcl)
                    (if active-mark nil (line-end-position 2))
                    t)
    (goto-char (match-beginning 0))
    (funcall comment-func)
    ;; Finally for where we started and clean up only when region was
    ;; not active:
    (goto-char p)
    (unless active-mark (funcall comment-func))
    (set-marker p nil)
    ;; Ask if we should insert a 'syntax' environment:
    (when (and (not active-mark)
               (y-or-n-p "Insert syntax environment? "))
      (LaTeX-environment-menu "syntax")
      (funcall comment-func)
      ;; Try to be smart: insert \cs{mac} into buffer.  First, delete
      ;; any whitespaces after the ',' if inserted:
      (setq mac (split-string
                 (replace-regexp-in-string "[[:blank:]]"
                                           ""
                                           mac)
                 "," t))
      ;; Count the number of elements for the number of linebreaks:
      (setq elt-count (length mac))
      ;; Now insert the functions wrapped in \cs:
      (save-excursion
        (dolist (elt mac)
          (insert TeX-esc "cs" TeX-grop (substring elt 1) TeX-grcl)
          (when (< count elt-count)
            (LaTeX-newline)
            (setq count (1+ count)))))
      ;; Now move to end of the first line:
      (end-of-line)
      (just-one-space))))

(defun LaTeX-item-l3doc-syntax ()
  "Insert line break macro on the last line.
For syntax environment from l3doc class."
  (save-excursion
    (end-of-line 0)
    (just-one-space)
    (TeX-insert-macro "\\")))

(TeX-add-style-hook
 "l3doc"
 (lambda ()

   ;; l3doc.cls loads shortvrb.sty and sets '|' and '"' as shorthands.
   ;; We append them to a local version of `LaTeX-shortvrb-chars'
   ;; before running the style hook for 'shortvrb' which is done
   ;; inside 'doc.el':
   (make-local-variable 'LaTeX-shortvrb-chars)
   (dolist (elt '(?| ?\"))
     (add-to-list 'LaTeX-shortvrb-chars elt t))

   (TeX-run-style-hooks "expl3" "doc" "ltx-base"
                        "article" "array" "amsmath" "booktabs"
                        "color" "colortbl" "hologo" "enumitem"
                        "textcomp" "csquotes" "fancyvrb" "verbatim"
                        "underscore")

   (TeX-add-symbols
    ;; 4.2 Partitioning documentation and implementation
    '("EnableDocumentation" 0)
    '("EnableImplementation" 0)
    '("DisableDocumentation" 0)
    '("DisableImplementation" 0)
    '("DocInputAgain" 0)

    ;; 4.3 General text markup
    '("cmd"
      [TeX-arg-key-val LaTeX-l3doc-cmd-key-val-options]
      TeX-arg-macro)
    '("cs"
      [TeX-arg-key-val LaTeX-l3doc-cmd-key-val-options]
      (TeX-arg-completing-read (TeX-symbol-list) "Macro"))
    '("tn"
      [TeX-arg-key-val LaTeX-l3doc-key-val-options]
      (TeX-arg-completing-read (TeX-symbol-list) "Macro"))

    ;; "meta" is provided by doc.el, so don't add here again
    '("Arg" "Argument")
    '("marg" "Mandatory argument")
    '("oarg" "Optional argument")
    '("parg" "Picture mode argument")

    '("file" "File name")
    '("env" (TeX-arg-completing-read (LaTeX-environment-list) "Environment"))
    '("pkg" "Package name")
    '("cls" "Class name")

    '("NB" "Tag" t)

    ;; "DocInput": This macro is supplied in `doc.el'

    ;; 4.5 Describing functions in the implementation
    '("TestFiles" "File(s)")
    '("UnitTested" 0)
    '("TestMissing" t))

   (LaTeX-add-environments
    '("documentation")
    '("implementation")
    '("NOTE" "Tag")
    '("function" LaTeX-env-l3doc-function)
    '("variable" LaTeX-env-l3doc-function)
    ;; Feature in 'syntax' environment: Hit 'M-RET' to insert '\\':
    '("syntax")
    '("texnote")
    '("arguments" LaTeX-env-item))

   ;; Do not indent the content of the 'documentation' and
   ;; 'implementation' environments; it is odd when major parts of the
   ;; document are indented.  Append them to a local version of
   ;; `LaTeX-document-regexp':
   (unless (string-match-p "\\<implementation\\>" LaTeX-document-regexp)
     (set (make-local-variable 'LaTeX-document-regexp)
          (concat LaTeX-document-regexp
                  "\\|documentation\\|implementation")))

   ;; Append syntax to `LaTeX-item-list' with `LaTeX-item-l3doc-syntax'
   (add-to-list 'LaTeX-item-list '("syntax" . LaTeX-item-l3doc-syntax) t)

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("cmd"  "[{")
                                ("cs"   "[{")
                                ("Arg"  "{")
                                ("marg" "{")
                                ("oarg" "{")
                                ("parg" "{")
                                ("file" "{")
                                ("env"  "{")
                                ("pkg"  "{")
                                ("cls"  "{")
                                ("NB"   "{{"))
                              'textual)
     (font-latex-add-keywords '(("EnableDocumentation"   "")
                                ("EnableImplementation"  "")
                                ("DisableDocumentation"  "")
                                ("DisableImplementation" "")
                                ("DocInputAgain"         "")
                                ("TestFiles"             "{")
                                ("UnitTested"            "")
                                ("TestMissing"           "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-l3doc-class-options
  (progn
    (TeX-load-style "article")
    (append (remove "a5paper" LaTeX-article-class-options)
            '("full" "onlydoc" "check" "nocheck" "checktest"
              "nochecktest" "kernel" "stdmodule" "cm-default"
              "lm-default" "cs-break-off" "cs-break-nohyphen"
              "show-notes" "hide-notes")))
  "Class options for the l3doc class.")

;;; l3doc.el ends here
