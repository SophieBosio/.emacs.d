;;; tcolorboxlib-raster.el --- AUCTeX style for `raster' library from tcolorbox  -*- lexical-binding: t; -*-

;; Copyright (C) 2016--2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
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

;; This file adds support for `raster' library from tcolorbox.sty.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(declare-function LaTeX-tcolorbox-keyval-options "tcolorbox" ())
(defvar LaTeX-tcolorbox-keyval-options-full)

(defvar LaTeX-tcolorbox-lib-raster-keyval-options
  '(;; 14.3 Option Keys of the Library
    ("raster columns")
    ("raster rows")
    ("raster width")
    ("raster height")
    ("raster before skip")
    ("raster after skip")
    ("raster equal skip")
    ("raster left skip")
    ("raster right skip")
    ("raster column skip")
    ("raster row skip")
    ("raster halign" ("left" "center" "right"))
    ("raster valign" ("top" "center" "bottom"))
    ("raster equal height" ("noe" "rows" "all"))
    ("raster equal height group")
    ("raster force size" ("true" "false"))
    ("raster reset")
    ;; 14.4 Adding Styles for Specific Boxes
    ("raster every box")
    ("raster odd column")
    ("raster even column")
    ("raster column 1")
    ("raster column 2")
    ("raster column 3")
    ("raster column 4")
    ("raster odd row")
    ("raster even row")
    ("raster row 1")
    ("raster row 2")
    ("raster row 3")
    ("raster row 4")
    ("raster odd number")
    ("raster even number")
    ;; raster row m column n is left to user
    ("raster number 1")
    ("raster number 2")
    ("raster number 3")
    ("raster number 4")
    ;; 14.5 Combining Columns or Rows
    ("raster multicolumn")
    ("raster multirow"))
  "Key=value options for raster library from tcolorbox.")

(defun LaTeX-tcolorbox-lib-raster-insert-item ()
  "Insert \"tcbitem\" and query for optional argument."
  (TeX-insert-macro "tcbitem"))

(TeX-add-style-hook
 "tcolorboxlib-raster"
 (lambda ()

   ;; Register key-vals from library to `LaTeX-tcolorbox-keyval-options-full':
   (add-to-list 'LaTeX-tcolorbox-keyval-options-full
                'LaTeX-tcolorbox-lib-raster-keyval-options)

   (TeX-add-symbols
    ;; 14.2 Macros of the Library
    '("tcbitem"
      [TeX-arg-key-val (LaTeX-tcolorbox-keyval-options) "Item options"]
      (TeX-arg-literal " ")))

   (LaTeX-add-environments
    ;; 14.2 Macros of the Library
    `("tcbraster" LaTeX-env-args
      [TeX-arg-key-val ,(lambda ()
                          (append LaTeX-tcolorbox-lib-raster-keyval-options
                                  (LaTeX-tcolorbox-keyval-options)))])

    `("tcbitemize" LaTeX-env-item-args
      [TeX-arg-key-val ,(lambda ()
                          (append LaTeX-tcolorbox-lib-raster-keyval-options
                                  (LaTeX-tcolorbox-keyval-options)))])

    `("tcboxedraster" LaTeX-env-args
      [TeX-arg-key-val ,(lambda ()
                          (append LaTeX-tcolorbox-lib-raster-keyval-options
                                  (LaTeX-tcolorbox-keyval-options)))
                       "Raster options"]
      (TeX-arg-key-val (LaTeX-tcolorbox-keyval-options)
                       "Box options"))

    `("tcboxeditemize" LaTeX-env-item-args
      [TeX-arg-key-val ,(lambda ()
                          (append LaTeX-tcolorbox-lib-raster-keyval-options
                                  (LaTeX-tcolorbox-keyval-options)))
                       "Raster options"]
      (TeX-arg-key-val (LaTeX-tcolorbox-keyval-options)
                       "Box options")))

   ;; Append tcb(oxed)?itemize to `LaTeX-item-list':
   (add-to-list 'LaTeX-item-list
                '("tcbitemize" . LaTeX-tcolorbox-lib-raster-insert-item) t)
   (add-to-list 'LaTeX-item-list
                '("tcboxeditemize" . LaTeX-tcolorbox-lib-raster-insert-item) t)

   ;; Append tcbitem to `LaTeX-item-regexp':
   (unless (string-match "tcbitem" LaTeX-item-regexp)
     (set (make-local-variable 'LaTeX-item-regexp)
          (concat
           LaTeX-item-regexp
           "\\|"
           "tcbitem\\b"))
     (LaTeX-set-paragraph-start))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("tcbitem" "["))
                              'textual)))
 TeX-dialect)

;;; tcolorboxlib-raster.el ends here
