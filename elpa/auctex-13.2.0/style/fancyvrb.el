;;; fancyvrb.el --- AUCTeX style for `fancyvrb.sty' version 4.5.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014, 2016-2023 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <mose@gnu.org>
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

;; This file adds support for `fancyvrb.sty' version 4.5.

;; This style has some capabilities to parse user defined macros,
;; environments and saved blocks with `SaveVerbatim' environments and
;; offer them for completion.  After defining a new macro or
;; environment, hit `C-c C-n' to parse the document.  In case of
;; `SaveVerbatim', a second `C-c C-n' might be necessary.

;; The command `\CustomVerbatimEnvironment' is not mentioned in the
;; documenation; hence this command is removed from this style.  Use
;; `\DefineVerbatimEnvironment' for new environments and customize the
;; standard ones with `\RecustomVerbatimEnvironment'.

;; The entries in `LaTeX-fancyvrb-key-val-options' cover also some
;; keys which are not mentioned in the manual of the package.

;; Starting with version 3.6, fancyvrb.sty provides a `reflabel' key
;; in the optional arguments which acts like a `\label' macro.  This
;; key makes mostly sense for `\pageref'; referencing number of
;; missing \caption's with `\ref' in a fancyvrb environment isn't
;; useful.  This style provides support for AUCTeX and RefTeX in order
;; to add and reference these labels.  When inserting an environment
;; with `C-c C-e', choose `reflabel' key during the completion.  If
;; you're using RefTeX and want to pass the label insertion job to it,
;; simply leave the key value-less and proceed.  Otherwise enter the
;; label value by yourself.  AUCTeX and RefTeX will parse the value
;; and offer it for completion once a referencing macros is used.

;;; Code:

;; Needed for auto-parsing:
(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(declare-function font-latex-set-syntactic-keywords
                  "font-latex")

(declare-function LaTeX-color-definecolor-list "color" ())
(declare-function LaTeX-xcolor-definecolor-list "xcolor" ())
(defvar LaTeX-fvextra-key-val-options)
(defvar font-latex-syntactic-keywords-extra)

(defvar LaTeX-fancyvrb-key-val-options
  `(("commentchar" ("none"))
    ("gobble")
    ("formatcom")
    ("formatcom*")    ; Undocumented key
    ("fontfamily" ("tt" "courier" "helvetica"))
    ("fontsize"   ("auto" "\\tiny" "\\scriptsize"
                   "\\footnotesize" "\\small" "\\normalsize"
                   "\\large" "\\Large" "\\LARGE" "\\huge" "\\Huge"))
    ("fontshape"  ("auto" "n" "it" "sl" "sc" "scit"))
    ("fontseries" ("auto" "m" "b" "bx" "sb" "c" "l" "lc"))
    ("frame" ("none" "leftline" "topline" "bottomline" "lines" "single"))
    ("framerule")
    ("framesep")
    ("rulecolor" ("none"))
    ("fillcolor" ("none"))
    ("label" ("none"))
    ("labelposition" ("none" "topline" "bottomline" "all"))
    ("numbers" ("none" "left" "right"))
    ("numbersep")
    ("firstnumber" ("auto" "last" "integer"))
    ("stepnumber")
    ("numberblanklines" ("true" "false"))
    ("firstline")
    ("lastline")
    ("showspaces" ("true" "false"))
    ("showtabs" ("true" "false"))
    ("obeytabs" ("true" "false"))
    ("tabsize")
    ("baselinestretch" ("auto" "dimension"))
    ("commandchars" ("none"))
    ("xleftmargin")
    ("xrightmargin")
    ("resetmargins" ("true" "false"))
    ("hfuzz")
    ("samepage" ("true" "false"))
    ("codes")
    ("codes*")        ; Undocumented key
    ("defineactive")
    ("defineactive*") ; Undocumented key
    ("reflabel")
    ;; Undocumented key and introduced in version 2.81 2011/04/06
    ("vspace" ,(mapcar (lambda (x)
                         (concat TeX-esc (car x)))
                       (LaTeX-length-list)))
    ;; Actually, the following options are used only by the `BVerbatim'
    ;; environment.
    ("boxwidth" ("auto" "dimension"))
    ("baseline" ("b" "c" "t"))
    ;; The next key applies only to `\SaveVerb' macro.
    ("aftersave"))
  "Key=value options for fancyvrb macros and environments.")

(defun LaTeX-fancyvrb-key-val-options ()
  "Return an updated list of key=vals from fancyvrb package.
This function also checks if the package fvextra is used and
takes its key=vals into account."
  (append
   ;; Check if fvextra is loaded in combination with x?color:
   (when (and (member "fvextra" (TeX-style-list))
              (or (member "xcolor" TeX-active-styles)
                  (member "color" TeX-active-styles)))
     (let* ((colorcmd (if (member "xcolor" TeX-active-styles)
                          #'LaTeX-xcolor-definecolor-list
                        #'LaTeX-color-definecolor-list))
            (keys '("highlightcolor"
                    "rulecolor"
                    "fillcolor"
                    "tabcolor"
                    "spacecolor"))
            (colors (mapcar #'car (funcall colorcmd)))
            result)
       (dolist (key keys result)
         (if (string= key "highlightcolor")
             (push (list key colors) result)
           (push (list key (cons "none" colors)) result)))))

   ;; Check if fvextra is loaded:
   (when (member "fvextra" TeX-active-styles)
     LaTeX-fvextra-key-val-options)

   ;; Check if fancyvrb is loaded in combination with x?color:
   (when (and (not (member "fvextra" TeX-active-styles))
              (or (member "xcolor" TeX-active-styles)
                  (member "color" TeX-active-styles)))
     (let* ((colorcmd (if (member "xcolor" TeX-active-styles)
                          #'LaTeX-xcolor-definecolor-list
                        #'LaTeX-color-definecolor-list))
            (colors (mapcar (lambda (x)
                              (concat TeX-esc "color" TeX-grop x TeX-grcl))
                            (mapcar #'car (funcall colorcmd))))
            (keys '("rulecolor" "fillcolor"))
            val result)
       (dolist (key keys result)
         (setq val (cadr (assoc key LaTeX-fancyvrb-key-val-options)))
         (push (list key (append val colors)) result))))

   ;; Plain fancyvrb:
   LaTeX-fancyvrb-key-val-options))

(defvar LaTeX-fancyvrb-base-macros
  '("VerbatimInput" "BVerbatimInput" "LVerbatimInput"
    "SaveVerb" "UseVerb" "Verb")
  "List of base macros available with fancyvrb package.
Starred versions are not included in this list.")

(defvar LaTeX-fancyvrb-base-environments
  '("Verbatim" "BVerbatim" "LVerbatim" "SaveVerbatim" "VerbatimOut")
  "List of base environments available with fancyvrb package.
Starred versions are not included in this list.")

(defvar LaTeX-fancyvrb-key-val-skip-regexp
  (concat "\\(?:" (LaTeX-extract-key-value-label 'none) "\\)?")
  "Helper regexp to skip over an optional argument.")

;; Setup for defining new Verbatim commands:

(TeX-auto-add-type "fancyvrb-macro" "LaTeX")

(defvar LaTeX-fancyvrb-macro-regexp
  `(,(concat "\\\\\\(Rec\\|C\\)ustomVerbatimCommand"
             "[ \t\n\r]*{?[ \t\n\r]*\\\\\\([A-Za-z]+\\)[ \t\n\r]*}?"
             "[ \t\n\r]*{[ \t\n\r]*\\([A-Za-z]+\\)[ \t\n\r]*}")
    (2 3 1) LaTeX-auto-fancyvrb-macro)
  "Matches macros by fancyvrb package.")

;; Setup for defining new Verbatim environments:

(TeX-auto-add-type "fancyvrb-environment" "LaTeX")

(defvar LaTeX-fancyvrb-environment-regexp
  `(,(concat "\\\\DefineVerbatimEnvironment"
             "[ \t\n\r]*{[ \t\n\r]*\\([A-Za-z0-9]+\\)[ \t\n\r]*}"
             "[ \t\n\r]*{[ \t\n\r]*\\([A-Za-z]+\\)[ \t\n\r]*}")
    (1 2) LaTeX-auto-fancyvrb-environment)
  "Matches new environments defined by fancyvrb package.")

;; Setup for names in SaveVerb macros:

(TeX-auto-add-type "fancyvrb-saveverb" "LaTeX")

(defvar LaTeX-fancyvrb-saveverb-regexp
  `(,(concat "\\\\SaveVerb"
             LaTeX-fancyvrb-key-val-skip-regexp
             "{\\([^}]+\\)}")
    1 LaTeX-auto-fancyvrb-saveverb)
  "Match the name under which verbatim text is saved by SaveVerb macro.")

;; Setup for names in SaveVerbatim environments:

(TeX-auto-add-type "fancyvrb-saveverbatim" "LaTeX")

(defvar LaTeX-fancyvrb-saveverbatim-regexp
  `(,(concat "\\\\begin{SaveVerbatim}"
             LaTeX-fancyvrb-key-val-skip-regexp
             "{\\([^}]+\\)}")
    1 LaTeX-auto-fancyvrb-saveverbatim)
  "Match the name under which verbatim text is saved by SaveVerbatim environment.")

(defun LaTeX-fancyvrb-auto-prepare ()
  "Clear various LaTeX-auto-fancyvrb-* variables before parsing."
  (setq LaTeX-auto-fancyvrb-macro        nil
        LaTeX-auto-fancyvrb-environment  nil
        LaTeX-auto-fancyvrb-saveverb     nil
        LaTeX-auto-fancyvrb-saveverbatim nil))

(defun LaTeX-fancyvrb-auto-cleanup ()
  "Process parsed elements for fancyvrb package."
  (LaTeX-fancyvrb-arg-define-macro nil t)
  (LaTeX-fancyvrb-arg-define-environment nil t))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-fancyvrb-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-fancyvrb-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-fancyvrb-arg-define-macro (optional &optional cleanup recustom)
  "Query and insert a new verbatim macro with fancyvrb package.
If OPTIONAL is non-nil, insert the arguments in brackets.  If
CLEANUP is non-nil, do not insert any arguments in the buffer and
update only various AUCTeX variables for verbatim macros.  If
RECUSTOM is non-nil, delete macros from the variable
`TeX-symbol-list' before adding the new ones."
  ;; This part is only relevant when called by user:
  (unless cleanup
    (let ((new-mac (if recustom
                       (completing-read
                        (TeX-argument-prompt optional nil "Verbatim macro: \\" t)
                        (mapcar #'car (apply #'append LaTeX-fancyvrb-macro-list)))
                     (TeX-read-string
                      (TeX-argument-prompt optional nil "New verbatim macro: \\" t))))
          (base-mac (completing-read (TeX-argument-prompt optional nil "Based on macro")
                                     LaTeX-fancyvrb-base-macros))
          (rec-flag (if recustom "Rec" "C")))
      ;; We are (re-)defining a macro: Insert user queried input and
      ;; use `LaTeX-add-fancyvrb-macros' on the input.  Do not enclose
      ;; the first argument in braces as this will improve
      ;; fontification.  Otherwise, the part between 2 closing braces
      ;; get fontified, i.e.:
      ;; \CustomVerbatimCommand{\foo}{Verb}{}
      ;;                            ^     ^
      (let ((TeX-arg-opening-brace "")
            (TeX-arg-closing-brace ""))
        (TeX-argument-insert new-mac optional TeX-esc))
      (TeX-argument-insert base-mac optional)
      (TeX-argument-insert
       (TeX-read-key-val optional (LaTeX-fancyvrb-key-val-options)) optional)
      (LaTeX-add-fancyvrb-macros `(,new-mac ,base-mac ,rec-flag))))
  ;;
  ;; Now run the procdure: Do not use the function
  ;; `LaTeX-fancyvrb-macro-list' here which will remove dupes from the
  ;; variable `LaTeX-fancyvrb-macro-list' depending on car; we need
  ;; the variable as is, hence (apply #'append ...);
  (dolist (elt (apply #'append LaTeX-fancyvrb-macro-list))
    (let ((mac-name (nth 0 elt))
          (base-mac (nth 1 elt))
          (flag     (nth 2 elt)))
      ;; If we're Rec-ustomizing, delete the entry first from
      ;; `TeX-symbol-list':
      (when (string= flag "Rec")
        (setq TeX-symbol-list
              (assq-delete-all (car (assoc mac-name (TeX-symbol-list))) TeX-symbol-list)))
      ;; Now add the new item: Start with new macros for loading
      ;; files:
      (cond ((member base-mac '("VerbatimInput" "BVerbatimInput" "LVerbatimInput"))
             (TeX-add-symbols
              `(,mac-name
                [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                LaTeX-fancyvrb-arg-file-relative)
              ;; Defined macros have a starred version where the
              ;; `showspaces' key is set to true
              `(,(concat mac-name "*")
                [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                LaTeX-fancyvrb-arg-file-relative))
             (when (and (fboundp 'font-latex-add-keywords)
                        (eq TeX-install-font-lock 'font-latex-setup))
               (font-latex-add-keywords `((,mac-name "*[{"))
                                        'reference))
             (TeX-ispell-skip-setcar
              `((,(concat (regexp-quote TeX-esc) mac-name "\\*?") ispell-tex-arg-end))))
            ;; New macros for saving verbatim text:
            ((string= base-mac "SaveVerb")
             (TeX-add-symbols
              `(,mac-name
                [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                ,(lambda (optional)
                   (let ((name (TeX-read-string
                                (TeX-argument-prompt optional nil "Save name"))))
                     (LaTeX-add-fancyvrb-saveverbs name)
                     (TeX-argument-insert name optional)))
                (TeX-arg-conditional (member "fvextra" (TeX-style-list))
                    (TeX-arg-verb-delim-or-brace)
                  (TeX-arg-verb))))
             (when (and (fboundp 'font-latex-add-keywords)
                        (eq TeX-install-font-lock 'font-latex-setup))
               (font-latex-add-keywords `((,mac-name "[{"))
                                        'textual)
               (LaTeX-fancyvrb-add-syntactic-keywords-extra 'delim
                                                            mac-name)
               (when (member "fvextra" (TeX-style-list))
                 (LaTeX-fancyvrb-add-syntactic-keywords-extra 'brace
                                                              mac-name)))
             (TeX-ispell-skip-setcar
              `((,(concat (regexp-quote TeX-esc) mac-name)
                 TeX-ispell-tex-arg-verb-end 1))))
            ;; New macros for using previously saved text:
            ((string= base-mac "UseVerb")
             (TeX-add-symbols
              `(,mac-name
                [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                (TeX-arg-completing-read (LaTeX-fancyvrb-saveverb-list)
                                         "Saved name"))
              ;; Defined macros have a starred version where the
              ;; `showspaces' key is set to true
              `(,(concat mac-name "*")
                [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                (TeX-arg-completing-read (LaTeX-fancyvrb-saveverb-list)
                                         "Saved name")))
             (when (and (fboundp 'font-latex-add-keywords)
                        (eq TeX-install-font-lock 'font-latex-setup))
               (font-latex-add-keywords `((,mac-name "*[{"))
                                        'textual))
             (TeX-ispell-skip-setcar
              `((,(concat (regexp-quote TeX-esc) mac-name "\\*?") ispell-tex-arg-end))))
            ;; Anything else is considered as verbatim typesetting macro:
            (t
             (TeX-add-symbols
              `(,mac-name
                [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                (TeX-arg-conditional (member "fvextra" (TeX-style-list))
                    (TeX-arg-verb-delim-or-brace)
                  (TeX-arg-verb)))
              ;; Defined macros have a starred version where the
              ;; `showspaces' key is set to true
              `(,(concat mac-name "*")
                [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                (TeX-arg-conditional (member "fvextra" (TeX-style-list))
                    (TeX-arg-verb-delim-or-brace)
                  (TeX-arg-verb))))
             (dolist (elt `(,mac-name ,(concat mac-name "*")))
               (add-to-list 'LaTeX-verbatim-macros-with-delims-local
                            elt t))
             (when (member "fvextra" (TeX-style-list))
               (dolist (elt `(,mac-name ,(concat mac-name "*")))
                 (add-to-list 'LaTeX-verbatim-macros-with-braces-local
                              elt t)))
             (when (and (fboundp 'font-latex-add-keywords)
                        (eq TeX-install-font-lock 'font-latex-setup))
               (font-latex-add-keywords `((,mac-name "*["))
                                        'textual))
             (TeX-ispell-skip-setcar
              `((,(concat (regexp-quote TeX-esc) mac-name "\\*?")
                 TeX-ispell-tex-arg-verb-end)))))))
  ;; Update font-lock:
  (when (and (fboundp 'font-latex-set-syntactic-keywords)
             (eq TeX-install-font-lock 'font-latex-setup))
    (font-latex-set-syntactic-keywords)))

(defun LaTeX-fancyvrb-arg-define-environment (optional &optional cleanup)
  "Query and insert a new verbatim environment with fancyvrb package.
If OPTIONAL is non-nil, insert the arguments in brackets.  If
CLEANUP is non-nil, do not insert any arguments in the buffer and
update only various AUCTeX variables for verbatim environments."
  (unless cleanup
    (let ((new-env (TeX-read-string
                    (TeX-argument-prompt optional nil "New verbatim environment")))
          (base-env (completing-read
                     (TeX-argument-prompt optional nil "Based on environment")
                     LaTeX-fancyvrb-base-environments)))
      ;; We are defining a new env: First insert the arguments and then
      ;; run `LaTeX-add-fancyvrb-environments' on '(new-env base-env).
      ;; If base-env is SaveVerbatim, run
      ;; `LaTeX-add-fancyvrb-saveverbatims' on new-env as well.
      (TeX-argument-insert new-env optional)
      (TeX-argument-insert base-env optional)
      (TeX-argument-insert
       (TeX-read-key-val optional (LaTeX-fancyvrb-key-val-options)) optional)
      (LaTeX-add-fancyvrb-environments `(,new-env ,base-env))
      (when (string= base-env "SaveVerbatim")
        (LaTeX-add-fancyvrb-saveverbatims new-env))))
  ;;
  ;; Now run the procdure:
  (dolist (elt (LaTeX-fancyvrb-environment-list))
    (let ((env (car elt))
          (type (cadr elt)))
      (cond ((string= type "VerbatimOut")
             (LaTeX-add-environments
              `(,env LaTeX-env-args
                     [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                     "Output file")))
            ((string= type "SaveVerbatim")
             (TeX-auto-add-regexp `(,(concat "\\\\begin{"
                                             env
                                             "}"
                                             LaTeX-fancyvrb-key-val-skip-regexp
                                             "{\\([^}]+\\)}")
                                    1 LaTeX-auto-fancyvrb-saveverbatim)))
            (t
             ;; Regular verbatim environments have a starred
             ;; version; so add them here; the non-starred additions
             ;; to `LaTeX-verbatim-environments-local' and
             ;; `LaTeX-indent-environment-list' are done outside
             ;; (cond ...):
             (LaTeX-add-environments
              `(,env LaTeX-env-args
                     [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                     LaTeX-fancyvrb-env-reflabel-key-val))
             (LaTeX-add-environments
              `(,(concat env "*") LaTeX-env-args
                [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
                LaTeX-fancyvrb-env-reflabel-key-val))
             (add-to-list 'LaTeX-verbatim-environments-local (concat env "*"))
             (add-to-list 'LaTeX-indent-environment-list
                          `(,(concat env "*") current-indentation) t)
             ;; Tell AUCTeX about label values to reflabel key:
             (add-to-list 'LaTeX-label-alist
                          (cons env 'LaTeX-listing-label) t)
             (add-to-list 'LaTeX-label-alist
                          (cons (concat env "*") 'LaTeX-listing-label) t)
             (TeX-auto-add-regexp
              `(,(concat  (regexp-quote TeX-esc)
                          "begin[[:space:]]*"
                          (regexp-quote TeX-grop)
                          env
                          "\\*?"
                          (regexp-quote TeX-grcl)
                          "[[:space:]]*"
                          (LaTeX-extract-key-value-label "reflabel"))
                1 LaTeX-auto-label))
             ;; Tell RefTeX:
             (when (and (fboundp 'reftex-add-label-environments)
                        (boundp 'reftex-label-regexps))
               (reftex-add-label-environments
                `((,env
                   ?l ,LaTeX-listing-label "~\\pageref{%s}"
                   LaTeX-fancyvrb-reftex-label-context-function
                   (regexp "[Ll]isting" "[Vv]erbatim"
                           "[Cc]ode"    "Quell\\(code\\|text\\)"))

                  (,(concat env "*")
                   ?l ,LaTeX-listing-label "~\\pageref{%s}"
                   LaTeX-fancyvrb-reftex-label-context-function
                   (regexp "[Ll]isting" "[Vv]erbatim"
                           "[Cc]ode"    "Quell\\(code\\|text\\)"))))
               (add-to-list 'reftex-label-regexps
                            (concat
                             (regexp-quote TeX-esc)
                             "begin[[:space:]]*"
                             (regexp-quote TeX-grop)
                             (concat env "\\*?")
                             (regexp-quote TeX-grcl)
                             "[[:space:]]*"
                             (LaTeX-extract-key-value-label "reflabel" 1))
                            t))))
      ;; These apply for all environments defined:
      (add-to-list 'LaTeX-verbatim-environments-local env)
      (add-to-list 'LaTeX-indent-environment-list `(,env current-indentation) t)
      ;; Add new env's to `ispell-tex-skip-alist': skip the entire env
      (TeX-ispell-skip-setcdr `(,(cons (concat env "\\*?")
                                       (concat "\\\\end{" env "\\*?}"))))))
  ;; Update RefTeX:
  (when (fboundp 'reftex-compile-variables)
    (reftex-compile-variables))
  ;; Update font-lock:
  (when (and (fboundp 'font-latex-set-syntactic-keywords)
             (eq TeX-install-font-lock 'font-latex-setup))
    (font-latex-set-syntactic-keywords)))

(defun LaTeX-fancyvrb-arg-file-relative (optional)
  "Query and insert a file name relative to current master file.
If OPTIONAL is non-nil, insert the file name in brackets."
  (TeX-argument-insert
   (file-relative-name
    (read-file-name (TeX-argument-prompt optional nil "File"))
    (TeX-master-directory))
   optional))

(defun LaTeX-fancyvrb-env-reflabel-key-val (_optional)
  "Add a label value to reflabel key.
This function checks if the reflabel key is given in the optional
argument of a fancyvrb environment and then adds a label as value
to that key.  The label value is inserted only if the key is
value-less; user entered label values are recognized and
respected.  OPTIONAL is ignored."
  (let ((p (point-marker))
        (s (make-marker)))
    (set-marker s (save-excursion
                    (LaTeX-find-matching-begin)
                    (re-search-forward (regexp-quote LaTeX-optop)
                                       (line-end-position)
                                       t)))
    ;; Search for the reflabel and a potential value:
    (when (marker-position s)
      (re-search-backward
       (concat
        "\\(\\<reflabel\\>\\)"
        ;; Check if the key already has a label value:
        "\\("
        "[[:space:]]*=[[:space:]]*"
        (regexp-quote TeX-grop)
        "?"
        "[[:alnum:]:._-]"
        "\\)?")
       s t)
      ;; Insert a label value only if the key is value-less:
      (when (and (not (match-string 2))
                 (match-string 1))
        (goto-char (match-end 1))
        (insert "="
                TeX-grop
                (format "%s" (LaTeX-label (LaTeX-current-environment)
                                          'environment
                                          t))
                TeX-grcl)))
    ;; At any rate, go to where we started and clean up:
    (goto-char p)
    (set-marker p nil)
    (set-marker s nil)))

(defun LaTeX-fancyvrb-reftex-label-context-function (env)
  "Extract and return a context string for RefTeX.
The context string is the first line of the verbatim environment.
If no reflabel key is found, an error is issued.
ENV is the name of current environment as a string."
  (let* ((envstart (save-excursion
                     (re-search-backward (concat "\\\\begin[[:space:]]*{"
                                                 env
                                                 "}")
                                         nil t)))
         (label-key (save-excursion
                      (re-search-backward "\\<reflabel[ \t\n\r%]*=[ \t\n\r%]*"
                                          envstart t))))
    (if label-key
        (save-excursion
          (goto-char envstart)
          (re-search-forward (regexp-quote LaTeX-optop) label-key t)
          (up-list)
          (forward-line)
          ;; Return the first line of verbatim env:
          (buffer-substring-no-properties (point)
                                          (line-end-position)))
      (error "No label found"))))

(defvar LaTeX-fancyvrb-key-val-label-regexp
  `(,(concat
      (regexp-quote TeX-esc)
      "begin[[:space:]]*"
      (regexp-quote TeX-grop)
      "[BL]?Verbatim\\*?"
      (regexp-quote TeX-grcl)
      "[[:space:]]*"
      (LaTeX-extract-key-value-label "reflabel"))
    1 LaTeX-auto-label)
  "Matches the label inside an optional argument of fancyvrb environments.")

(defun LaTeX-fancyvrb-add-syntactic-keywords-extra (type macro)
  "Add MACRO from fancyvrb.sty to `font-latex-syntactic-keywords-extra'.
TYPE is one of the symbols `brace' or `delim' indicating how
verbatim text is enclosed after the macro.  MACRO is a string or
a list of strings."
  (let ((syntax (if (eq type 'brace)
                    '((1 "|") (2 ".") (3 "|"))
                  '((1 "\"") (2 ".") (3 "\""))))
        regexp)
    (when (listp macro)
      (setq macro (regexp-opt macro "\\(?:")))
    (setq regexp `(,(concat
                     ;; The backslash
                     (regexp-quote TeX-esc)
                     ;; Name of the macro(s)
                     macro
                     ;; The optional argument
                     "\\(?:\\[[^][]*\\(?:\\[[^][]*\\][^][]*\\)*\\]\\)?"
                     ;; The first mandatory argument which is the name
                     "\\(?:{[^}]+}\\)"
                     ;; With 'brace, allow one braced sub-group
                     ;; otherwise we might stop matching too early.
                     ;; With 'delim, copy font-latex.el:
                     (if (eq type 'brace)
                         (concat "\\({\\)[^}{]*?\\(?:{[^}{]*}[^}{]*?\\)*"
                                 "\\(" (regexp-quote TeX-esc) "*\\)"
                                 "\\(}\\)")
                       (concat
                        ;; Opening delimiter
                        "\\([^a-z@*\n\f{]\\).*?"
                        ;; Closing delimiter
                        "\\(" (regexp-quote TeX-esc) "*\\)\\(\\1\\)")))))
    (add-to-list 'font-latex-syntactic-keywords-extra (append regexp syntax))))

(TeX-add-style-hook
 "fancyvrb"
 (lambda ()
   (TeX-auto-add-regexp LaTeX-fancyvrb-macro-regexp)
   (TeX-auto-add-regexp LaTeX-fancyvrb-environment-regexp)
   (TeX-auto-add-regexp LaTeX-fancyvrb-saveverb-regexp)
   (TeX-auto-add-regexp LaTeX-fancyvrb-saveverbatim-regexp)
   (TeX-auto-add-regexp LaTeX-fancyvrb-key-val-label-regexp)
   (TeX-run-style-hooks
    "keyval")

   (TeX-add-symbols
    ;; Verbatim material in footnotes
    "VerbatimFootnotes"
    ;; Improved verbatim commands
    '("Verb" [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-conditional (member "fvextra" (TeX-style-list))
          (TeX-arg-verb-delim-or-brace)
        (TeX-arg-verb)))
    ;; \Verb also has a starred version
    '("Verb*" [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-conditional (member "fvextra" (TeX-style-list))
          (TeX-arg-verb-delim-or-brace)
        (TeX-arg-verb)))
    `("DefineShortVerb" (TeX-arg-string "Character" ,TeX-esc))
    `("UndefineShortVerb" (TeX-arg-string "Character" ,TeX-esc))
    ;; Verbatim environments
    '("fvset" (TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)))
    ;; Changing individual line formatting
    "FancyVerbFormatLine"
    ;; Line numbering
    "theFancyVerbLine"
    ;; Selection of lines to print
    "FancyVerbStartString"
    "FancyVerbStopString"

    ;; Personalized environments
    '("DefineVerbatimEnvironment"
      LaTeX-fancyvrb-arg-define-environment)
    '("RecustomVerbatimEnvironment"
      (TeX-arg-completing-read LaTeX-fancyvrb-base-environments
                               "Verbatim environment")
      (TeX-arg-completing-read LaTeX-fancyvrb-base-environments
                               "Based on environment")
      (TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)))

    '("CustomVerbatimCommand"
      LaTeX-fancyvrb-arg-define-macro)
    '("RecustomVerbatimCommand"
      (LaTeX-fancyvrb-arg-define-macro nil t))

    ;; Saving and restoring verbatim text and environments
    `("SaveVerb"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      ,(lambda (optional)
         (let ((name (TeX-read-string
                      (TeX-argument-prompt optional nil "Save name"))))
           (LaTeX-add-fancyvrb-saveverbs name)
           (TeX-argument-insert name optional)))
      (TeX-arg-conditional (member "fvextra" (TeX-style-list))
          (TeX-arg-verb-delim-or-brace)
        (TeX-arg-verb)))
    '("UseVerb"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-completing-read (LaTeX-fancyvrb-saveverb-list)
                               "Saved name"))
    ;; \UseVerb also has a starred version
    '("UseVerb*"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-completing-read (LaTeX-fancyvrb-saveverb-list)
                               "Saved name"))
    '("UseVerbatim"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-completing-read (LaTeX-fancyvrb-saveverbatim-list)
                               "Saved name"))
    ;; \UseVerbatim et al. also have a starred version
    '("UseVerbatim*"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-completing-read (LaTeX-fancyvrb-saveverbatim-list)
                               "Saved name"))
    '("LUseVerbatim"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-completing-read (LaTeX-fancyvrb-saveverbatim-list)
                               "Saved name"))
    '("LUseVerbatim*"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-completing-read (LaTeX-fancyvrb-saveverbatim-list)
                               "Saved name"))
    '("BUseVerbatim"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-completing-read (LaTeX-fancyvrb-saveverbatim-list)
                               "Saved name"))
    '("BUseVerbatim*"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      (TeX-arg-completing-read (LaTeX-fancyvrb-saveverbatim-list)
                               "Saved name"))

    ;; Writing and reading verbatim files
    '("VerbatimInput" [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      LaTeX-fancyvrb-arg-file-relative)
    '("BVerbatimInput" [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      LaTeX-fancyvrb-arg-file-relative)
    '("LVerbatimInput" [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      LaTeX-fancyvrb-arg-file-relative))

   (LaTeX-add-environments
    '("Verbatim" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      LaTeX-fancyvrb-env-reflabel-key-val)
    '("Verbatim*" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      LaTeX-fancyvrb-env-reflabel-key-val)
    '("BVerbatim" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      LaTeX-fancyvrb-env-reflabel-key-val)
    '("BVerbatim*" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      LaTeX-fancyvrb-env-reflabel-key-val)
    '("LVerbatim" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      LaTeX-fancyvrb-env-reflabel-key-val)
    '("LVerbatim*" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      LaTeX-fancyvrb-env-reflabel-key-val)
    `("SaveVerbatim" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      ,(lambda (optional)
         (let ((name (TeX-read-string
                      (TeX-argument-prompt optional nil "Save name"))))
           (LaTeX-add-fancyvrb-saveverbatims name)
           (TeX-argument-insert name optional))))
    '("VerbatimOut" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)]
      "Output file"))

   (let ((envs '("BVerbatim" "BVerbatim*"
                 "LVerbatim" "LVerbatim*"
                 "Verbatim"  "Verbatim*")))
     ;; Add pre-defined environments to `LaTeX-label-alist':
     (dolist (env envs)
       (add-to-list 'LaTeX-label-alist (cons env 'LaTeX-listing-label) t))

     ;; Tell RefTeX
     (when (and (fboundp 'reftex-add-label-environments)
                (fboundp 'reftex-compile-variables)
                (boundp 'reftex-label-regexps))
       (dolist (env envs)
         (reftex-add-label-environments
          `((,env ?l ,LaTeX-listing-label "~\\pageref{%s}"
                  LaTeX-fancyvrb-reftex-label-context-function
                  (regexp "[Ll]isting" "[Vv]erbatim"
                          "[Cc]ode"    "Quell\\(code\\|text\\)")))))

       (unless (string-match "\\<reflabel"
                             (mapconcat #'identity
                                        reftex-label-regexps
                                        "|"))
         (make-local-variable 'reftex-label-regexps)
         (add-to-list 'reftex-label-regexps
                      (concat
                       (regexp-quote TeX-esc)
                       "begin[[:space:]]*"
                       (regexp-quote TeX-grop)
                       "[BL]?Verbatim\\*?"
                       (regexp-quote TeX-grcl)
                       "[[:space:]]*"
                       (LaTeX-extract-key-value-label "reflabel" 1))
                      t)
         (reftex-compile-variables))))

   (LaTeX-add-counters
    "FancyVerbLine")

   ;; Filling
   (make-local-variable 'LaTeX-indent-environment-list)
   (let ((envs '("Verbatim"    "Verbatim*"
                 "BVerbatim"   "BVerbatim*"
                 "LVerbatim"   "LVerbatim*"
                 "VerbatimOut" "SaveVerbatim")))
     (dolist (env envs)
       (add-to-list 'LaTeX-verbatim-environments-local env)
       (add-to-list 'LaTeX-indent-environment-list `(,env current-indentation) t)))

   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb*")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
              (fboundp 'font-latex-set-syntactic-keywords)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("CustomVerbatimCommand"       "|{\\{{")
                                ("RecustomVerbatimCommand"     "|{\\{{")
                                ("DefineVerbatimEnvironment"   "{{{")
                                ("RecustomVerbatimEnvironment" "{{{")
                                ("DefineShortVerb"   "{")
                                ("UndefineShortVerb" "{")
                                ("fvset"             "{"))
                              'function)
     (font-latex-add-keywords '(("VerbatimInput"  "[{")
                                ("BVerbatimInput" "[{")
                                ("LVerbatimInput" "[{"))
                              'reference)
     (font-latex-add-keywords '(("Verb" "*[") ; The second argument is verbatim.
                                ("SaveVerb"     "[{")
                                ("UseVerb"      "*[{")
                                ("UseVerbatim"  "*[{")
                                ("LUseVerbatim" "*[{")
                                ("BUseVerbatim" "*[{"))
                              'textual)
     ;; Add \SaveVerb to `font-latex-syntactic-keywords-extra' and
     ;; cater for their special syntax:
     ;; \SaveVerb[optional]{name}|verbatim| or
     ;; \SaveVerb[optional]{name}{verbatim} with fvextra.sty loaded:
     (LaTeX-fancyvrb-add-syntactic-keywords-extra 'delim
                                                  "SaveVerb")
     (when (member "fvextra" (TeX-style-list))
       (LaTeX-fancyvrb-add-syntactic-keywords-extra 'brace
                                                    "SaveVerb"))
     ;; Tell font-lock about the update
     (font-latex-set-syntactic-keywords)))
 TeX-dialect)

(defvar LaTeX-fancyvrb-package-options nil
  "Package options for the fancyvrb package.")

;;; fancyvrb.el ends here
