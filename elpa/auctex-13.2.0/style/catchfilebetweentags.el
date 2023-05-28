;;; catchfilebetweentags.el --- AUCTeX style for catchfilebetweentags package  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Uwe Brauer <oub@mat.ucm.es>
;; Maintainer: auctex-devel@gnu.org
;; Created: Aug 23, 2022
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

;; This file adds support for the catchfilebetweentags package.

;; Acknowledgements
;; Arash Esbati <arash@gnu.org> for, basically, a complete rewrite, thanks.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar-local LaTeX-catchfilebetweentags-counter nil
  "Counter for LaTeX-catchfilebetweentags numbers.")

;; Scanning function, stolen from markdown-mode
(defun LaTeX-catchfilebetweentags-counter-inc ()
  "Increment `LaTeX-catchfilebetweentags-counter' and return the new value."
  (when (null LaTeX-catchfilebetweentags-counter)
    (setq LaTeX-catchfilebetweentags-counter 0)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^%<\\*[^:]*:\\([^>]+\\)>$"
                                (point-max) t)
        (let ((fn (string-to-number (match-string 1))))
          (when (> fn LaTeX-catchfilebetweentags-counter)
            (setq LaTeX-catchfilebetweentags-counter fn))))))
  (setq LaTeX-catchfilebetweentags-counter
        (1+ LaTeX-catchfilebetweentags-counter)))

(defun LaTeX-env-catchfilebetweentags (_environment)
  "Insert a tag-skeleton defined by `LaTeX-catchfilebetweentags'.
ENVIRONMENT is ignored."
  ;; The following code, adds the file name as a prefix to the tag, in
  ;; a similar way reftex does this, which is useful for combining
  ;; several external files to a singular one.
  (let* ((file (file-name-sans-extension
                (file-name-nondirectory
                 (buffer-file-name (current-buffer)))))
         (fn (when LaTeX-catchfilebetweentags-use-numeric-label
               (LaTeX-catchfilebetweentags-counter-inc)))
         (tag (concat file ":"
                      (TeX-read-string
                       (if fn (format "Tag (default %s): " fn) "Tag: ")
                       nil nil (when fn (number-to-string fn))))))
    (unless (bolp)
      (newline)
      (delete-horizontal-space))
    (save-excursion
      (insert (concat (format "%%<*%s>" tag)
                      "\n\n"
                      (format "%%</%s>" tag)))))
  (forward-line))

(TeX-add-style-hook
 "catchfilebetweentags"
 (lambda ()
   (TeX-add-symbols
    '("ExecuteMetaData"
      ;; Act like \include and not like \input:
      [TeX-arg-input-file "File" t] "Tag")
    '("ExecuteMetaData*"
      [TeX-arg-input-file "File" t] "Tag")
    '("CatchFileBetweenTags"
      TeX-arg-define-macro (TeX-arg-input-file  "File-name" t) "Tag")
    '("CatchFileBetweenTags*"
      TeX-arg-define-macro (TeX-arg-input-file  "File-name" t) "Tag")

    '("CatchFileBetweenDelims"
      TeX-arg-define-macro (TeX-arg-input-file  "File-name" t)
      "Start delimiter" "Stop delimiter" ["Setup"]))

   (LaTeX-add-environments
    '("catchfilebetweenfiletags" LaTeX-env-catchfilebetweentags))

   ;; Add `LaTeX-catchfilebetweentags-counter' to
   ;; `TeX-normal-mode-reset-list' in case the variable gets out of
   ;; sync:
   (add-to-list 'TeX-normal-mode-reset-list
                'LaTeX-catchfilebetweentags-counter)

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("ExecuteMetaData" "*[{")
                                ("CatchFileBetweenTags"   "*|{\\{{")
                                ("CatchFileBetweenDelims" "|{\\{{{["))
                              'function)))
 TeX-dialect)

;;; catchfilebetweentags.el ends here
