;;; sidecap.el --- AUCTeX style for `sidecap.sty' (v1.6f)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-12-11
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

;; This file adds support for `sidecap.sty' (v1.6f) from 2003/06/06.
;; `sidecap.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

(defun LaTeX-env-sidecap-float (environment)
  "Create ENVIRONMENT with \\caption and \\label commands.
This function runs `LaTeX-env-figure' and inserts the first
optional argument \\='relwidth\\=' provided by environments of the
package sidecap."
  (let ((relwidth (TeX-read-string
                   (TeX-argument-prompt t nil "Relative caption width")))
        (sc-active-mark (and (TeX-active-mark)
                             (not (eq (mark) (point)))))
        (p (point-marker))
        s)
    ;; Run `LaTeX-env-figure' which does the major part of the job:
    (LaTeX-env-figure environment)
    ;; Now save the position:
    (setq s (point-marker))
    ;; Search backwards to see if an optional float-placement arg is
    ;; inserted; this would be the 2nd arg for sidecap environments:
    (save-excursion
      (re-search-backward (concat (regexp-quote TeX-esc)
                                  "begin"
                                  "[ \t]*"
                                  TeX-grop
                                  (regexp-quote environment)
                                  "\\(" TeX-grcl "\\)"
                                  "[ \t]*"
                                  "\\("
                                  (regexp-quote LaTeX-optop)
                                  ;; Float placement:
                                  "\\([a-zA-Z!]*\\)"
                                  (regexp-quote LaTeX-optcl)
                                  "\\)?")
                          p t))
    (cond (;; Insert the first optional arg at any rate if non-empty:
           (and relwidth (not (string= relwidth "")))
           (goto-char (match-end 1))
           (insert LaTeX-optop relwidth LaTeX-optcl))
          ;; Insert a pair of empty brackets if relwidth is empty and
          ;; float-placement is given:
          ((and (or (null relwidth)
                    (string= relwidth ""))
                (match-string 3))
           (goto-char (match-beginning 2))
           (insert LaTeX-optop LaTeX-optcl))
          (t nil))
    ;; Go back to where we started if we have moved at all:
    (unless (= s (point))
      (goto-char s))
    ;; Insert a tabular stored in `LaTeX-default-tabular-environment':
    (when (and (member environment '("SCtable" "SCtable*"))
               (not sc-active-mark))
      (LaTeX-environment-menu LaTeX-default-tabular-environment))
    ;; Clean up the markers:
    (set-marker s nil)
    (set-marker p nil)))

(TeX-add-style-hook
 "sidecap"
 (lambda ()

   ;; Add the environments provided by the package:
   (LaTeX-add-environments
    '("SCtable"   LaTeX-env-sidecap-float)
    '("SCtable*"  LaTeX-env-sidecap-float)
    '("SCfigure"  LaTeX-env-sidecap-float)
    '("SCfigure*" LaTeX-env-sidecap-float)
    '("wide"))

   ;; Add the float environments to `LaTeX-label-alist':
   (dolist (env '("SCfigure" "SCfigure*"))
     (add-to-list 'LaTeX-label-alist `(,env . LaTeX-figure-label) t))

   (dolist (env '("SCtable" "SCtable*"))
     (add-to-list 'LaTeX-label-alist `(,env . LaTeX-table-label) t))

   ;; The next 2 can be set with '\renewcommand':
   (TeX-add-symbols
    "sidecaptionsep"
    "sidecaptionrelwidth")

   ;; Run the style hook for 'ragged2e' if necessary:
   (when (or (LaTeX-provided-package-options-member "sidecap" "raggedright")
             (LaTeX-provided-package-options-member "sidecap" "raggedleft")
             (LaTeX-provided-package-options-member "sidecap" "ragged"))
     (TeX-run-style-hooks "ragged2e")))

 TeX-dialect)

(defvar LaTeX-sidecap-package-options
  '("outercaption" "innercaption"
    "leftcaption" "rightcaption"
    "wide"
    "raggedright" "raggedleft" "ragged")
  "Package options for the sidecap package.")

;;; sidecap.el ends here
