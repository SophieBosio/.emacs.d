;;; url.el --- AUCTeX style for `url.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2023  Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-10-13
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

;; This file adds support for `url.sty' v3.4 dated 2013-09-16.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(declare-function font-latex-set-syntactic-keywords
                  "font-latex")

;; Setup for \DeclareUrlCommand:
(TeX-auto-add-type "url-DeclareUrlCommand" "LaTeX")

(defvar LaTeX-url-DeclareUrlCommand-regexp
  `(,(concat
      "\\\\DeclareUrlCommand"
      "[ \n\r\t]*"
      "{?"
      "\\\\\\([a-zA-Z]+\\)"
      "}?")
    1 LaTeX-auto-url-DeclareUrlCommand)
  "Matches the argument of `\\DeclareUrlCommand' from `url' package.")

(defun LaTeX-url-DeclareUrlCommand-prepare ()
  "Process macros parsed from `\\DeclareUrlCommand'."
  ;; \DeclareUrlCommand\abc{settings}: makes \abc{ } like \url{ } with
  ;; settings.
  (when (LaTeX-url-DeclareUrlCommand-list)
    (dolist (cmd (mapcar #'car (LaTeX-url-DeclareUrlCommand-list)))
      (TeX-add-symbols `(,cmd TeX-arg-verb-delim-or-brace))
      (add-to-list 'LaTeX-verbatim-macros-with-delims-local cmd)
      (add-to-list 'LaTeX-verbatim-macros-with-braces-local cmd))
    ;; Fontification
    (when (and (fboundp 'font-latex-add-keywords)
               (fboundp 'font-latex-set-syntactic-keywords)
               (eq TeX-install-font-lock 'font-latex-setup))
      (font-latex-add-keywords
       (mapcar (lambda (cmd)
                 (list cmd  ""))
               (mapcar #'car (LaTeX-url-DeclareUrlCommand-list)))
       'reference)
      ;; Tell font-lock about the update.
      (font-latex-set-syntactic-keywords))))

;; Setup for \urldef:
(TeX-auto-add-type "url-urldef" "LaTeX")

(defvar LaTeX-url-urldef-regexp
  `(,(concat
      "\\\\urldef"
      "[ \n\r\t]*"
      "{?"
      "\\\\\\([a-zA-Z]+\\)"
      "}?")
    1 LaTeX-auto-url-urldef)
  "Matches the argument of `\\urldef' from `url' package.")

(defun LaTeX-url-urldef-prepare ()
  "Process macros parsed from `\\urldef'."
  (when (LaTeX-url-urldef-list)
    (mapc #'TeX-add-symbols (mapcar #'car (LaTeX-url-urldef-list)))
    ;; Fontification
    (when (and (fboundp 'font-latex-add-keywords)
               (eq TeX-install-font-lock 'font-latex-setup))
      (font-latex-add-keywords
       (mapcar (lambda (cmd)
                 (list cmd ""))
               (mapcar #'car (LaTeX-url-urldef-list)))
       'reference))))

(defun LaTeX-url-auto-prepare ()
  "Clear `LaTeX-auto-url-*' before parsing."
  (setq LaTeX-auto-url-DeclareUrlCommand nil
        LaTeX-auto-url-urldef            nil))

(defun LaTeX-url-auto-cleanup ()
  "Process parsed elements from url.sty."
  (LaTeX-url-DeclareUrlCommand-prepare)
  (LaTeX-url-urldef-prepare))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-url-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-url-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun TeX-arg-url-DeclareUrlCommand (optional &optional prompt)
  "Prompt for arguments of \\DeclareUrlCommand with completion.
If OPTIONAL is non-nil, indicate it in the minibuffer during the
query and insert the result in brackets.  PROMPT replaces the
standard one."
  (let ((cmd (TeX-read-string
              (TeX-argument-prompt optional prompt "Command: \\" t)))
        (style (completing-read
                (TeX-argument-prompt optional prompt "Style")
                '("rm" "same" "sf" "tt"))))
    (insert TeX-esc cmd TeX-grop)
    (when (and style (not (string= style "")))
      (insert TeX-esc "urlstyle" TeX-grop style TeX-grcl))
    (insert TeX-grcl)
    (LaTeX-add-url-DeclareUrlCommands cmd)
    (LaTeX-url-DeclareUrlCommand-prepare)))

(defun TeX-arg-url-urldef (optional &optional prompt)
  "Prompt for arguments of \\urldef with completion."
  (let ((cmd (TeX-read-string
              (TeX-argument-prompt optional prompt "Command: \\" t))))
    (TeX-argument-insert cmd optional TeX-esc)
    (LaTeX-add-url-urldefs cmd)
    (LaTeX-url-urldef-prepare))
  (TeX-insert-macro
   (completing-read
    (TeX-argument-prompt optional prompt "Macro: \\" t)
    (append (mapcar #'car (LaTeX-url-DeclareUrlCommand-list))
            '("url")))))

(TeX-add-style-hook
 "url"
 (lambda ()

   ;; Add url to the parser
   (TeX-auto-add-regexp LaTeX-url-DeclareUrlCommand-regexp)
   (TeX-auto-add-regexp LaTeX-url-urldef-regexp)

   ;; New symbols
   (TeX-add-symbols
    ;; Macros for defining new styles, changing font, linebreaks etc.
    "Url"
    "UrlBigBreakPenalty"
    "UrlBigBreaks"
    "UrlBreakPenalty"
    "UrlBreaks"
    "UrlFont"
    "UrlLeft"
    "UrlNoBreaks"
    "UrlOrds"
    "UrlRight"
    "UrlSpecials"

    ;; "hyperref" redefines \url so that the argument is only in
    ;; braces.  We check here if hyperref is loaded:
    '("url" (TeX-arg-conditional (member "hyperref" (TeX-style-list))
                ("Url")
              ((TeX-arg-verb-delim-or-brace "Url"))))

    '("urldef" TeX-arg-url-urldef)

    '("urlstyle" (TeX-arg-completing-read ("rm" "same" "sf" "tt")
                                          "Style"))

    '("DeclareUrlCommand" TeX-arg-url-DeclareUrlCommand))

   ;; For '\path', use the facilities provided by this style.  Also
   ;; don't add "path" for fontification below since
   ;; `LaTeX-url-DeclareUrlCommand-prepare' takes care of it.
   (LaTeX-add-url-DeclareUrlCommands "path")
   (LaTeX-url-DeclareUrlCommand-prepare)

   ;; Don't do the same for '\url' because hyperref.el has some code
   ;; to remove "url" from `LaTeX-verbatim-macros-with-delims-local',
   ;; but we check here as well if "hyperref" is already loaded:
   (unless (member "hyperref" (TeX-style-list))
     (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url"))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("url" ""))
                              'reference)
     ;; Don't fontify the second argument of 'DeclareUrlCommand' since
     ;; it gets the `font-latex-verbatim-face' from the first
     ;; argument.  Same applies also to '\urldef' where we don't
     ;; fontify any arguments:
     (font-latex-add-keywords '(("DeclareUrlCommand" "\\")
                                ("urldef"            "")
                                ("urlstyle"          "{"))
                              'function)
     ;; Tell font-lock about the update.
     (font-latex-set-syntactic-keywords)))
 TeX-dialect)

(defvar LaTeX-url-package-options '("hyphens" "obeyspaces" "spaces"
                                    "allowmove" "lowtilde")
  "Package options for the url package.")

;;; url.el ends here
