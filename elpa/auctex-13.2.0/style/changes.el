;;; changes.el --- AUCTeX style for `changes.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021--2022 Free Software Foundation, Inc.

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

;; This file adds support for `changes.sty' v4.0.0. from 2021/01/28.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(defvar LaTeX-xcolor-base-colors)
(defvar LaTeX-truncate-package-options)
(defvar LaTeX-ulem-package-options)
(defvar LaTeX-xcolor-package-options)

(TeX-auto-add-type "changes-definechangesauthor" "LaTeX")

(defvar LaTeX-changes-definechangesauthor-regexp
  `(,(concat "\\\\definechangesauthor"
             "[ \t\n\r%]*"
             "\\(?:\\[[^]]*\\]\\)?"
             "[ \t\n\r%]*"
             "{\\([^}]+\\)}")
    1 LaTeX-auto-changes-definechangesauthor)
  "Matches the id defined by \\definechangesauthor.")

(defun LaTeX-changes-auto-prepare ()
  "Reset `LaTeX-auto-changes-definechangesauthor'."
  (setq LaTeX-auto-changes-definechangesauthor nil))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-changes-auto-prepare t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-changes-definechangesauthor-key-val-options ()
  "Return a key=val list for the \\definechangesauthor macro."
  `(("name")
    ("color" ,(cond ((and (member "xcolor" (TeX-style-list))
                          (fboundp 'LaTeX-xcolor-definecolor-list))
                     (mapcar #'car (LaTeX-xcolor-definecolor-list)))
                    ((and (member "color" TeX-active-styles)
                          (fboundp 'LaTeX-color-definecolor-list))
                     (mapcar #'car (LaTeX-color-definecolor-list)))
                    (t nil)))))

(defun LaTeX-changes-key-val-options ()
  "Return a key=val list for the change management macros."
  `(("id" ,(mapcar #'car
                   (LaTeX-changes-definechangesauthor-list)))
    ("comment")))

(TeX-add-style-hook
 "changes"
 (lambda ()

   ;; Add changes to the parser
   (TeX-auto-add-regexp LaTeX-changes-definechangesauthor-regexp)

   ;; Run AUCTeX style hooks based on given package options: This is
   ;; more complicated since we're only looking after "ulem" or
   ;; "xcolor" and don't really care about the given options to them:
   (when (assoc "changes" LaTeX-provided-package-options)
     (dolist (pkg '("ulem" "xcolor"))
       (let ((opts (cdr (assoc "changes"
                               LaTeX-provided-package-options))))
         (when (string-match (concat "\\<" pkg "\\>")
                             (mapconcat #'identity opts "|"))
           (TeX-run-style-hooks pkg)))))

   ;; truncate.sty is always loaded:
   (TeX-run-style-hooks "truncate")

   (TeX-add-symbols
    ;; 4.2 Change management
    '("added"    [TeX-arg-key-val LaTeX-changes-key-val-options nil nil ?\s] 1)
    '("deleted"  [TeX-arg-key-val LaTeX-changes-key-val-options nil nil ?\s] 1)
    '("replaced" [TeX-arg-key-val LaTeX-changes-key-val-options nil nil ?\s] 2)

    ;; 4.3 Highlighting and Comments
    '("highlight" [TeX-arg-key-val LaTeX-changes-key-val-options nil nil ?\s] 1)
    `("comment"
      [TeX-arg-key-val
       ,(lambda ()
          `(("id" ,(mapcar #'car (LaTeX-changes-definechangesauthor-list)))))]
      1)

    ;; 4.4 Overview of changes
    '("listofchanges"
      [TeX-arg-key-val (("style" ("list" "summary" "compactsummary"))
                        ("title")
                        ("show" ("all" "added" "deleted"
                                 "replaced" "highlight" "comment")))])

    ;; 4.5 Author management \definechangesauthor
    `("definechangesauthor"
      [TeX-arg-key-val (LaTeX-changes-definechangesauthor-key-val-options)
                       nil nil ?\s]
      ,(lambda (optional)
         (let ((id (TeX-read-string
                    (TeX-argument-prompt optional nil "Author ID"))))
           (LaTeX-add-changes-definechangesauthors id)
           (TeX-argument-insert id optional))))

    ;; 4.6 Adaption of the output:
    '("setaddedmarkup" "Definition")
    '("setdeletedmarkup" "Definition")
    '("sethighlightmarkup" "Definition")
    '("setcommentmarkup" "Definition")
    '("setauthormarkup" "Definition")
    '("setauthormarkupposition"
      (TeX-arg-completing-read ("left" "right") "Position"))
    '("setauthormarktext"
      (TeX-arg-completing-read ("id" "name") "Markup"))
    '("settruncatewidth" (TeX-arg-length "Width"))
    '("setsummarywidth" (TeX-arg-length "Width"))
    '("setsummarytowidth" "Text")
    '("setlocextension" "Extension")
    '("setsocextension" "Extension"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("definechangesauthor" "[{")
                                ("setaddedmarkup"      "{")
                                ("setdeletedmarkup"    "{")
                                ("sethighlightmarkup"  "{")
                                ("setcommentmarkup"    "{")
                                ("setauthormarkup"     "{")
                                ("setauthormarkupposition"   "{")
                                ("setauthormarktext"   "{")
                                ("settruncatewidth"    "{")
                                ("setsummarywidth"     "{")
                                ("setsummarytowidth"   "{")
                                ("setlocextension"     "{")
                                ("setsocextension"     "{"))
                              'function)
     (font-latex-add-keywords '(("added"     "[{")
                                ("deleted"   "[{")
                                ("replaced"  "[{{")
                                ("highlight" "[{")
                                ("comment"   "[{"))
                              'textual)
     (font-latex-add-keywords '(("listofchanges" "["))
                              'reference)))
 TeX-dialect)

(defun LaTeX-changes-package-options ()
  "Prompt for package options for the changes package."
  (TeX-load-style "xcolor")
  (TeX-load-style "truncate")
  (TeX-load-style "ulem")
  (TeX-read-key-val
   t
   (append
    `(("defaultcolor"
       ,(if (and (fboundp 'LaTeX-xcolor-definecolor-list)
                 (LaTeX-xcolor-definecolor-list))
            (mapcar #'car (LaTeX-xcolor-definecolor-list))
          LaTeX-xcolor-base-colors)))
    `(("draft")
      ("final")
      ("commandnameprefix" ("none" "ifneeded" "always"))
      ("markup" ("default" "underlined" "bfit" "nocolor"))
      ("addedmarkup" ("colored" "uline" "uuline" "uwave"
                      "dashuline" "dotuline"
                      "bf" "it" "sl" "em"))
      ("deletedmarkup" ("sout" "xout" "colored"
                        "uline" "uuline" "uwave"
                        "dashuline" "dotuline"
                        "bf" "it" "sl" "em"))
      ("highlightmarkup" ("background" "uuline" "uwave"))
      ("commentmarkup" ("todo" "margin" "footnote" "uwave"))
      ("authormarkup" ("superscript" "subscript" "brackets"
                       "footnote" "none"))
      ("authormarkupposition" ("right" "left"))
      ("authormarkuptext" ("id" "name"))
      ("todonotes")
      ("truncate" ,LaTeX-truncate-package-options)
      ("ulem" ,LaTeX-ulem-package-options)
      ("xcolor" ,LaTeX-xcolor-package-options)))))

;;; changes.el ends here
