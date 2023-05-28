;;; booktabs.el -- AUCTeX style for booktabs.sty  -*- lexical-binding: t; -*-

;; Copyright (C) 2003--2022 Free Software Foundation, Inc.

;; Author:   Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created:  2003-10-21
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

;; This file adds support for `booktabs.sty'.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "booktabs"
 (lambda ()
   ;; Do not append an empty group to toprule, midrule, and bottomrule macros,
   ;; otherwise one gets a wrong spacing in the table.
   (setq TeX-insert-braces-alist (append TeX-insert-braces-alist
                                         '(("toprule" . nil)
                                           ("midrule" . nil)
                                           ("bottomrule" . nil))))
   ;; New symbols
   (TeX-add-symbols
    '("toprule" [ "Thickness" ])
    '("midrule" [ "Thickness" ])
    '("bottomrule" [ "Thickness" ])
    ;; The `ignore' resets `TeX-last-optional-rejected' to nil so that the trim
    ;; argument is prompted also when the thickness is skipped.
    '("cmidrule" [ "Thickness" ] (ignore)
      [TeX-arg-string "Trim" nil nil nil "(" ")"]
      "Column(s)")
    '("addlinespace" [ "Height" ])
    '("morecmidrules")
    '("specialrule" "Thickness" "Space above" "Space below"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("toprule" "[")
                                ("midrule" "[")
                                ("bottomrule" "[")
                                ("cmidrule" "[({")
                                ("addlinespace" "[")
                                ("morecmidrules" "")
                                ("specialrule" "{{{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-booktabs-package-options nil
  "Package options for the booktabs package.")

;;; booktabs.el ends here
