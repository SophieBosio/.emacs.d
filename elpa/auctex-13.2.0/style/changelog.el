;;; changelog.el --- AUCTeX style for `changelog.sty' (v2.0.0)  -*- lexical-binding: t; -*-

;; Copyright (C) 2019--2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2019-05-05
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

;; This file adds support for `changelog.sty' (v2.0.0).
;; `changelog.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(defvar reftex-label-alist)

(defvar LaTeX-changelog-env-key-val-options
  '(("section" ("true" "false"))
    ("title"))
  "Key=value options for changelog environment.
The keys sectioncmd and label are added in the function
`LaTeX-env-changelog'.")

(defvar LaTeX-changelog-version-env-key-val-options
  '(("version")
    ("v")
    ("author")
    ("date")
    ("yanked" ("true" "false"))
    ("simple" ("true" "false"))
    ("short"  ("true" "false")))
  "key=value options for version environment.")

(defun LaTeX-changelog-key-val-options ()
  "Return an updated list of key=vals from changelog package."
  (let ((currenv (LaTeX-current-environment))
        (seccmds (mapcar #'car LaTeX-section-list)))
    (if (string= currenv "changelog")
        (append
         `(("sectioncmd"
            ,(if (< (LaTeX-largest-level) 2)
                 (append (mapcar (lambda (cmd) (concat TeX-esc cmd))
                                 seccmds)
                         (mapcar (lambda (cmd) (concat TeX-esc cmd "*"))
                                 seccmds))
               (append (mapcar (lambda (cmd) (concat TeX-esc cmd))
                               (remove "chapter" seccmds))
                       (mapcar (lambda (cmd) (concat TeX-esc cmd "*"))
                               (remove "chapter" seccmds))))))
         LaTeX-changelog-env-key-val-options
         LaTeX-changelog-version-env-key-val-options)
      LaTeX-changelog-version-env-key-val-options)))

(defun LaTeX-env-changelog (_optional)
  "Insert a label into the optional argument of changelog environment.
OPTIONAL is ignored."
  (let* ((s (save-excursion
              (LaTeX-find-matching-begin)
              (point)))
         (currenv LaTeX-current-environment)
         ;; Extract the chosen sectioning command
         (sec (save-excursion
                (re-search-backward
                 (concat "sectioncmd=\\\\\\([a-z]+\\)\\(\\*?\\)"
                         "\\|"
                         "\\<\\(section\\)\\(?:=true\\)?")
                 s t)
                (or (match-string-no-properties 1)
                    (match-string-no-properties 3))))
         ;; Temp. re-bind `LaTeX-label-alist' and pick the label
         ;; prefix from `LaTeX-section-label'
         (LaTeX-label-alist
          (when (or (and (match-string 2)
                         (not (string= (match-string 2) "*")))
                    (match-string 3))
            `(,(cons currenv
                     (cdr (assoc sec LaTeX-section-label))))))
         ;; Temp. re-bind `reftex-label-alist' as well and make
         ;; `reftex-label' DTRT:
         (reftex-label-alist
          (when (and (boundp 'reftex-label-alist)
                     LaTeX-label-alist)
            `((,currenv ?s ,(cdr (assoc sec LaTeX-section-label)) nil t)))))
    ;; Add a label into the opt. argument
    (when LaTeX-label-alist
      (LaTeX-env-label-as-keyval nil "\\<section\\(?:cmd\\)?\\>"
                                 nil currenv))))

(TeX-add-style-hook
 "changelog"
 (lambda ()

   (LaTeX-add-environments
    '("changelog" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-changelog-key-val-options)]
      LaTeX-env-changelog)
    '("version"   LaTeX-env-item-args
      [TeX-arg-key-val (LaTeX-changelog-key-val-options)]))

   (TeX-add-symbols
    '("added"      0)
    '("changed"    0)
    '("deprecated" 0)
    '("removed"    0)
    '("fixed"      0)
    '("security"   0)
    `("shortversion" (TeX-arg-key-val
                      ,(append
                        '(("changes"))
                        LaTeX-changelog-version-env-key-val-options))))

   ;; Tell RefTeX that the optional arg of changelog env. can contain a label:
   (when (and (boundp 'reftex-label-regexps)
              (fboundp 'reftex-compile-variables)
              (not (string-match  "\\bchangelog\\b"
                                  (mapconcat #'identity
                                             reftex-label-regexps
                                             "|"))))
     (add-to-list (make-local-variable 'reftex-label-regexps)
                  (concat "\\\\begin{changelog}"
                          (LaTeX-extract-key-value-label nil 1))
                  t)
     (reftex-compile-variables))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("added"        "")
                                ("changed"      "")
                                ("deprecated"   "")
                                ("removed"      "")
                                ("fixed"        "")
                                ("security"     "")
                                ("shortversion" "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-changelog-package-options nil
  "Package options for the changelog package.")

;;; changelog.el ends here
