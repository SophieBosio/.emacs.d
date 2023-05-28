;;; footnotehyper.el --- AUCTeX style for `footnotehyper.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-01-31
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
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary:

;; This file adds support for `footnotehyper.sty'.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defun LaTeX-arg-footnotehyper-savenotes (_optional)
  "Insert the corresponding \\spewnotes macro after \\savenotes.
OPTIONAL is ignored."
  (just-one-space)
  (save-excursion
    (insert TeX-esc "spewnotes")))

(TeX-add-style-hook
 "footnotehyper"
 (lambda ()

   ;; Environment defined by the package.  Query for optional footnote
   ;; numbers depends on the value of `TeX-arg-footnote-number-p':
   (LaTeX-add-environments
    '("savenotes")
    '("footnote" LaTeX-env-args
      (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil))
    '("footnotetext" LaTeX-env-args
      (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)))

   ;; New symbols
   (TeX-add-symbols
    '("savenotes" LaTeX-arg-footnotehyper-savenotes)
    '("spewnotes" 0)

    ;; The syntax is \makesavenoteenv[bar]{foo} where `bar' is a new
    ;; environment of patched environment `foo'.  This command isn't
    ;; recommended; hence we don't add parsing capabilities to this
    ;; style in order to extract the newly defined environments
    ;; automatically:
    '("makesavenoteenv"
      [TeX-arg-environment "New environment" t]
      (TeX-arg-environment "Patched environment")))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("savenotes" "")
                                ("spewnotes" "")
                                ("makesavenoteenv" "[{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-footnotehyper-package-options nil
  "Package options for the footnotehyper package.")

;;; footnotehyper.el ends here
