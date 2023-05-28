;;; tcolorboxlib-theorems.el --- AUCTeX style for `theorems' lib from tcolorbox  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-08-18
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

;; This file adds support for `theorems' library from tcolorbox.sty.
;; It supports automatic parsing of newly defined environments with
;; the '\newtcbtheorem' and addition of 'label={prefix:value}' to the
;; optional arguments of environments.  To do so, users must make some
;; additions to `LaTeX-label-alist' and `reftex-label-alist' if RefTeX
;; is used (which is of course highly recommended).  Suppose a new
;; environment 'Theorem' is defined like this:

;; \newtcbtheorem{Theorem}{Theorem}{%
;;   many key-values,
;; }{}

;; Note that the last mandatory argument 'prefix' is left empty.
;; Suppose you want to use the 'thm' prefix for the labels, the
;; additions look like this:
;;
;; For AUCTeX:
;; (with-eval-after-load 'latex
;;   (add-to-list 'LaTeX-label-alist '("Theorem" . "thm") t))
;;
;; For AUCTeX and RefTeX:
;; (with-eval-after-load 'latex
;;   (add-to-list 'LaTeX-label-alist '("Theorem" . "thm") t))
;;
;; (with-eval-after-load 'reftex-vars
;;   (add-to-list 'reftex-label-alist
;;                '("Theorem" ?m "thm:" "~\\ref{%s}"
;;                  LaTeX-tcolorbox-lib-theorems-reftex-label-context-function
;;                  ("Theorem" "theorem") nil)
;;                t))
;;
;; This file defines the function
;; `LaTeX-tcolorbox-lib-theorems-reftex-label-context-function' which
;; extracts the context for RefTeX in Select Label buffer.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(declare-function LaTeX-xcolor-definecolor-list "xcolor" ())
(declare-function LaTeX-tcolorbox-keyval-options "tcolorbox" ())
(defvar LaTeX-tcolorbox-keyval-options-full)

;; Needed for auto-parsing:
(require 'tex)
(require 'latex)

(defvar LaTeX-tcolorbox-lib-theorems-keyval-options
  '(;; 18.2 Option Keys of the Library
    ("separator sign")
    ("separator sign colon")
    ("separator sign dash")
    ("separator sign none")
    ("description delimiters" ("{}{}"))
    ("description delimiters parenthesis")
    ("description delimiters none")
    ("description color")
    ("description font"
     ("\\rmfamily" "\\sffamily" "\\ttfamily" "\\mdseries" "\\bfseries"
      "\\upshape" "\\itshape" "\\slshape" "\\scshape"
      "\\tiny"  "\\scriptsize" "\\footnotesize"
      "\\small" "\\normalsize" "\\large"
      "\\Large" "\\LARGE" "\\huge" "\\Huge" "\\normalfont"))
    ("description formatter")
    ("terminator sign")
    ("terminator sign colon")
    ("terminator sign dash")
    ("terminator sign none")
    ("label separator")
    ("theorem full label supplement")
    ("theorem label supplement" ("{}"))
    ("theorem hanging indent")
    ("theorem name and number")
    ("theorem number and name")
    ("theorem name")
    ("theorem number")
    ("theorem" ("{}{}{}{}"))
    ("highlight math")
    ("highlight math style" ("{}"))
    ("math upper")
    ("math lower")
    ("math")
    ("ams equation upper")
    ("ams equation lower")
    ("ams equation")
    ("ams equation* upper")
    ("ams equation* lower")
    ("ams equation*")
    ("ams align upper")
    ("ams align lower")
    ("ams align")
    ("ams align* upper")
    ("ams align* lower")
    ("ams align*")
    ("ams gather upper")
    ("ams gather lower")
    ("ams gather")
    ("ams gather* upper")
    ("ams gather* lower")
    ("ams gather*")
    ("ams nodisplayskip upper")
    ("ams nodisplayskip lower")
    ("ams nodisplayskip")
    ("theorem style" ("standard" "change standard"
                      "plain" "break" "plain apart"
                      "change" "change break" "change apart"
                      "margin" "margin break" "margin apart")))
  "Key=value options for theorems library from tcolorbox.")

(defun LaTeX-tcolorbox-lib-theorems-keyval-options ()
  "Return an updated list of key=vals for the theorems library."
  (append
   ;; `tcolorbox' loads the style `xcolor.el', so we use
   ;; `LaTeX-xcolor-definecolor-list' right away:
   (let ((colors (mapcar #'car (LaTeX-xcolor-definecolor-list)))
         (colkeys '("description color"))
         result)
     (dolist (key colkeys result)
       (push (list key colors) result)))
   (let ((lengths (cons "auto" (mapcar (lambda (x)
                                         (concat TeX-esc (car x)))
                                       (LaTeX-length-list))))
         (lenkeys '("theorem hanging indent"))
         result)
     (dolist (key lenkeys result)
       (push (list key lengths) result)))
   LaTeX-tcolorbox-lib-theorems-keyval-options))

(defun LaTeX-tcolorbox-lib--theorems-keyval-options ()
  "Return key=vals from theorems library incl. standard ones."
  (append (LaTeX-tcolorbox-keyval-options)
          (LaTeX-tcolorbox-lib-theorems-keyval-options)))

;; Setup for \newtcbtheorem:
(TeX-auto-add-type "tcolorbox-lib-theorems-newtcbtheorem" "LaTeX")

(defvar LaTeX-tcolorbox-lib-theorems-newtcbtheorem-regexp
  `(,(concat "\\\\newtcbtheorem"
             "[ \t\n\r%]*"
             ;; Init options:
             "\\(?:"
             (LaTeX-extract-key-value-label 'none)
             "\\)?"
             "[ \t\n\r%]*"
             ;; Name of the environment
             "{\\([^}]+\\)}")
    1 LaTeX-auto-tcolorbox-lib-theorems-newtcbtheorem)
  "Matches the name of new env defined with \\newtcbtheorem macro.")

(defun LaTeX-tcolorbox-lib-theorems-auto-prepare ()
  "Reset `LaTeX-auto-tcolorbox-lib-theorems-newtcbtheorem' before parsing."
  (setq LaTeX-auto-tcolorbox-lib-theorems-newtcbtheorem nil))

(defun LaTeX-tcolorbox-lib-theorems-auto-cleanup ()
  "Process user defined theorems with \\newtcbtheorem."
  (dolist (elt (LaTeX-tcolorbox-lib-theorems-newtcbtheorem-list))
    (let ((env (car elt)))
      ;; Add newly defined env's to AUCTeX:
      (LaTeX-add-environments
       `(,env LaTeX-env-args
              [TeX-arg-key-val (LaTeX-tcolorbox-lib--theorems-keyval-options)]
              "Title"
              (TeX-arg-literal ,TeX-grop ,TeX-grcl)
              (LaTeX-env-label-as-keyval nil nil ,env))
       `(,(concat env "*") LaTeX-env-args
         [TeX-arg-key-val (LaTeX-tcolorbox-lib--theorems-keyval-options)]
         "Title"))
      ;; RefTeX: Make `reftex-label-regexps' buffer local and add env
      ;; to it:
      (when (boundp 'reftex-label-regexps)
        (make-local-variable 'reftex-label-regexps)
        (add-to-list 'reftex-label-regexps
                     (concat
                      (regexp-quote TeX-esc)
                      "begin[[:space:]]*"
                      (regexp-quote TeX-grop)
                      (regexp-quote env)
                      (regexp-quote TeX-grcl)
                      "[[:space:]]*"
                      (LaTeX-extract-key-value-label "label" 1))
                     t)))))

(add-hook 'TeX-auto-prepare-hook
          #'LaTeX-tcolorbox-lib-theorems-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook
          #'LaTeX-tcolorbox-lib-theorems-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-arg-tcolorbox-lib-theorems-newtcbtheorem (optional)
  "Query and insert the first argument of \\newtcbtheorem macro.
If OPTIONAL is non-nil, insert the argument in brackets if not
empty."
  (let ((env (TeX-read-string
              (TeX-argument-prompt optional nil "Name"))))
    (LaTeX-add-tcolorbox-lib-theorems-newtcbtheorems env)
    (LaTeX-tcolorbox-lib-theorems-auto-cleanup)
    (TeX-argument-insert env optional)))

(defun LaTeX-tcolorbox-lib-theorems-reftex-label-context-function (env)
  "Return a context string for RefTeX in ENV."
  (let* ((envstart (save-excursion
                     (re-search-backward
                      (concat (regexp-quote TeX-esc)
                              "begin[[:space:]]*"
                              (regexp-quote TeX-grop)
                              (regexp-quote env)
                              (regexp-quote TeX-grcl)))))
         (label-key (save-excursion
                      (re-search-backward "\\<label[ \t\n\r%]*=[ \t\n\r%]*"
                                          envstart t))))
    (if label-key
        (save-excursion
          (goto-char label-key)
          ;; Move out of the optional argument:
          (up-list)
          ;; Move until the beginning of the title with {:
          (skip-chars-forward (concat "^" TeX-grop))
          ;; Return the title string:
          (buffer-substring-no-properties (1+ (point))
                                          (progn
                                            (forward-list)
                                            (1- (point)))))
      (error "No label found"))))

(TeX-add-style-hook
 "tcolorboxlib-theorems"
 (lambda ()

   ;; Add the style to the parser
   (TeX-auto-add-regexp LaTeX-tcolorbox-lib-theorems-newtcbtheorem-regexp)

   ;; Register key-vals from library to `LaTeX-tcolorbox-keyval-options-full':
   (add-to-list 'LaTeX-tcolorbox-keyval-options-full
                'LaTeX-tcolorbox-lib-theorems-keyval-options)

   ;; This library loads amsmath:
   (TeX-run-style-hooks "amsmath")

   (TeX-add-symbols
    ;; 18.1 Macros of the Library
    `("newtcbtheorem"
      [TeX-arg-key-val LaTeX-tcolorbox-init-options]
      LaTeX-arg-tcolorbox-lib-theorems-newtcbtheorem
      "Display Name"
      (TeX-arg-key-val (LaTeX-tcolorbox-lib--theorems-keyval-options))
      (TeX-arg-literal ,TeX-grop ,TeX-grcl))

    `("renewtcbtheorem"
      [TeX-arg-key-val LaTeX-tcolorbox-init-options]
      (TeX-arg-competing-read (tcolorbox-lib-theorems-newtcbtheorem-list) "Name")
      "Display Name"
      (TeX-arg-key-val (LaTeX-tcolorbox-lib--theorems-keyval-options))
      (TeX-arg-literal ,TeX-grop ,TeX-grcl))

    '("tcboxmath"
      [TeX-arg-key-val (LaTeX-tcolorbox-lib--theorems-keyval-options)]
      t)

    '("tcbhighmath"
      [TeX-arg-key-val (LaTeX-tcolorbox-lib--theorems-keyval-options)]
      t))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newtcbtheorem"   "[{{{{")
                                ("renewtcbtheorem" "[{{{{"))
                              'function)))
 TeX-dialect)

;;; tcolorboxlib-theorems.el ends here
