;;; ltcaption.el --- AUCTeX style for `ltcaption.sty' version v1.4c  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-05-04
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

;; This file adds support for `ltcaption.sty' version v1.4c from
;; 2021/01/08.  `ltcaption.sty' is part of TeXLive.

;; This style tracks ltcaption.sty which doesn't load longtable.sty
;; and emits an error when longtable package is missing.  The same
;; happens here when longtable.el isn't loaded yet where
;; `LaTeX-item-longtable' is undefined.  The remedy is to load
;; longtable before ltcaption.  The caption package loads ltcaption
;; automatically if longtable is loaded.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex" (keywords class))
(declare-function LaTeX-item-longtable
                  "longtable" (&optional suppress))

(defun LaTeX-env-longtable* (environment)
  "Insert a longtable* ENVIRONMENT with a starred caption."
  (let ((pos (and LaTeX-default-position ; `LaTeX-default-position'
                                        ; can be nil, i.e. no prompt
                  (completing-read (TeX-argument-prompt t nil "Position")
                                   '("l" "r" "c")
                                   nil nil LaTeX-default-position)))
        (fmt (TeX-read-string
              (if (string= LaTeX-default-format "")
                  "Format: "
                (format "Format (default %s): " LaTeX-default-format))
              nil nil
              (if (string= LaTeX-default-format "")
                  nil
                LaTeX-default-format)))
        (caption (TeX-read-string
                  (TeX-argument-prompt nil nil "Starred caption"))))
    (setq LaTeX-default-position pos
          LaTeX-default-format   fmt)
    (LaTeX-insert-environment environment
                              (concat
                               (unless (zerop (length pos))
                                 (concat LaTeX-optop pos LaTeX-optcl))
                               (concat TeX-grop fmt TeX-grcl)))
    ;; top caption -- do nothing if user skips caption
    (unless (zerop (length caption))
      ;; insert '\caption*{text} \\':
      (insert TeX-esc "caption*" TeX-grop caption TeX-grcl " \\\\")
      ;; fill the caption
      (when auto-fill-function (LaTeX-fill-paragraph))
      ;; Insert a new line and indent
      (LaTeX-newline)
      (indent-according-to-mode))
    ;; Insert suitable number of &'s, suppress line break
    (LaTeX-item-longtable t)))

(TeX-add-style-hook
 "ltcaption"
 (lambda ()

   (TeX-add-symbols
    "LTcapmarginsfalse"
    "LTcaptype")

   ;; These parameters are set with \setlength
   (LaTeX-add-lengths
    "LTcapskip" "LTcapleft" "LTcapright")

   (LaTeX-add-environments
    '("longtable*" LaTeX-env-longtable*))

   ;; Use the enhanced table formatting.  Append to
   ;; `LaTeX-indent-environment-list' in order not to override custom
   ;; settings.
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
                '("longtable*" LaTeX-indent-tabular) t)

   ;; Append 'longtable*' to `LaTeX-item-list' with `LaTeX-item-longtable':
   (add-to-list 'LaTeX-item-list '("longtable*" . LaTeX-item-longtable) t)

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("LTcapmarginsfalse" ""))
                              'function)))
 TeX-dialect)

(defvar LaTeX-ltcaption-package-options nil
  "Package options for the ltcaption package.")

;;; ltcaption.el ends here
