;;; caption.el --- AUCTeX style for `caption.sty' (v3.4a)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015--2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2015-02-21
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

;; This file adds support for `caption.sty' (v3.4a) from 2019/10/18.
;; `caption.sty' is part of TeXLive.

;; If things do not work or when in doubt, press `C-c C-n'.  Comments
;; for improvement are welcome.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; Needed for compiling `LaTeX-check-insert-macro-default-style':
(require 'latex)

;; Needed for auto-parsing:
(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(declare-function LaTeX-babel-active-languages "babel" ())
(declare-function LaTeX-polyglossia-active-languages "polyglossia" ())
(declare-function LaTeX-newfloat-DeclareFloatingEnvironment-list
                  "newfloat" ())

(defvar LaTeX-bicaption-key-val-options)
(defvar LaTeX-subcaption-key-val-options)

(defvar LaTeX-caption-key-val-options
  '(("aboveskip")
    ("belowskip")
    ("font"   ("scriptsize" "footnotesize" "small" "normalsize" "large"
               "Large" "normalfont" "up" "it" "sl" "sc" "md" "bf" "rm"
               "sf" "tt" "singlespacing" "onehalfspacing" "doublespacing"
               "stretch" "normalcolor" "color" "normal"))
    ("font+"  ("scriptsize" "footnotesize" "small" "normalsize" "large"
               "Large" "normalfont" "up" "it" "sl" "sc" "md" "bf" "rm"
               "sf" "tt" "singlespacing" "onehalfspacing" "doublespacing"
               "stretch" "normalcolor" "color" "normal"))
    ("format" ("plain" "hang"))
    ("hangindent")
    ("hypcap" ("false" "no" "off" "0" "true" "yes" "on" "1"))
    ("hypcapspace")
    ("indention")
    ("justification" ("justified" "centering" "centerlast" "centerfirst"
                      "raggedright" "RaggedRight" "raggedleft"))
    ("labelfont"     ("scriptsize" "footnotesize" "small" "normalsize" "large"
                      "Large" "normalfont" "up" "it" "sl" "sc" "md" "bf" "rm"
                      "sf" "tt" "singlespacing" "onehalfspacing" "doublespacing"
                      "stretch" "normalcolor" "color" "normal"))
    ("labelfont+"    ("scriptsize" "footnotesize" "small" "normalsize" "large"
                      "Large" "normalfont" "up" "it" "sl" "sc" "md" "bf" "rm"
                      "sf" "tt" "singlespacing" "onehalfspacing" "doublespacing"
                      "stretch" "normalcolor" "color" "normal"))
    ("labelformat"   ("default" "empty" "simple" "brace" "parens"
                      "autodot" "unnumbered"))
    ("labelsep"      ("none" "colon" "period" "space" "quad" "newline" "endash"))
    ("list"          ("false" "no" "off" "0" "true" "yes" "on" "1"))
    ("listformat"    ("empty" "simple" "paren" "subsimple" "subparens"))
    ("margin")
    ("margin*")
    ("maxmargin")
    ("minmargin")
    ("name")
    ("oneside")
    ("parindent")
    ("parskip")
    ("position"        ("top" "above" "bottom" "below" "auto"))
    ("singlelinecheck" ("false" "no" "off" "0" "true" "yes" "on" "1"))
    ("slc"             ("false" "no" "off" "0" "true" "yes" "on" "1"))
    ("skip")
    ("strut"      ("false" "no" "off" "0" "true" "yes" "on" "1"))
    ("style"      ("base" "default"))
    ("textfont"   ("scriptsize" "footnotesize" "small" "normalsize" "large"
                   "Large" "normalfont" "up" "it" "sl" "sc" "md" "bf" "rm"
                   "sf" "tt" "singlespacing" "onehalfspacing" "doublespacing"
                   "stretch" "normalcolor" "color" "normal"))
    ("textfont+"  ("scriptsize" "footnotesize" "small" "normalsize" "large"
                   "Large" "normalfont" "up" "it" "sl" "sc" "md" "bf" "rm"
                   "sf" "tt" "singlespacing" "onehalfspacing" "doublespacing"
                   "stretch" "normalcolor" "color" "normal"))
    ("textformat" ("empty" "simple" "period"))
    ("twoside")
    ("type"       ("figure" "table"))
    ("type*"      ("figure" "table"))
    ("width"))
  "Key=value options for caption macros.")

(defun LaTeX-caption-key-val-options ()
  "Return an updated list of key=vals from caption package.
The key=val list will also contain elements from subcaption
package if loaded."
  ;; Update the style list once so we don't run this function more
  ;; than once:
  (TeX-style-list)
  (append
   ;; Check if `subcaption.el' is loaded:
   (when (member "subcaption" TeX-active-styles)
     ;; If loaded, update also the entry for `subrefformat' when
     ;; processing the `labelformat'.  `subrefformat' keys takes the
     ;; same values as `labelformat'.  We have to check if we have an
     ;; entry for `\DeclareCaptionLabelFormat', otherwise there is no
     ;; need to run this procedure:
     (if (and (LaTeX-caption-DeclareCaption-list)
              (string-match
               "\\\\DeclareCaptionLabelFormat"
               (mapconcat #'identity
                          (mapcar #'car
                                  (LaTeX-caption-DeclareCaption-list))
                          "|")))
         (progn
           (let (result)
             (dolist (keyvals (LaTeX-caption-DeclareCaption-list) result)
               (when (string-equal (nth 1 keyvals) "LabelFormat")
                 (let* ((key "subrefformat")
                        (val (nth 2 keyvals))
                        (vals-predefined
                         (cadr (assoc key
                                      LaTeX-subcaption-key-val-options)))
                        (vals-parsed
                         (cadr (assoc key result))))
                   (cl-pushnew (list key (TeX-delete-duplicate-strings
                                          (append vals-predefined
                                                  vals-parsed
                                                  (list val))))
                               result :test #'equal))))))
       LaTeX-subcaption-key-val-options))
   ;; Check if `bicaption.el' is loaded:
   (when (member "bicaption" TeX-active-styles)
     (append
      (cond ((and (member "babel" TeX-active-styles)
                  (LaTeX-babel-active-languages))
             `(,(list "language"
                      (butlast (LaTeX-babel-active-languages)))))
            ((and (member "polyglossia" TeX-active-styles)
                  (LaTeX-polyglossia-active-languages))
             `(,(list "language"
                      (butlast (LaTeX-polyglossia-active-languages)))))
            (t nil))
      LaTeX-bicaption-key-val-options))
   ;; Support for environments defined with `newfloat.sty': These
   ;; environments are added to `type' and `type*' keys:
   (when (and (member "newfloat" TeX-active-styles)
              (LaTeX-newfloat-DeclareFloatingEnvironment-list))
     (let (result)
       (dolist (key '("type" "type*") result)
         (let ((val (mapcar #'car
                            (LaTeX-newfloat-DeclareFloatingEnvironment-list)))
               (vals-predefined
                (cadr (assoc key LaTeX-caption-key-val-options))))
           (cl-pushnew (list key (TeX-delete-duplicate-strings
                                  (append vals-predefined
                                          val)))
                       result :test #'equal)))))
   ;; Update to standard key-vals from `caption.el':
   (when (LaTeX-caption-DeclareCaption-list)
     (let (result)
       (dolist (keyvals (LaTeX-caption-DeclareCaption-list) result)
         (let* ((key (if (string-equal (nth 1 keyvals) "LabelSeparator")
                         (downcase (substring (nth 1 keyvals) 0 8))
                       (downcase (nth 1 keyvals))))
                (val (nth 2 keyvals))
                (vals-predefined
                 (cadr (assoc key LaTeX-caption-key-val-options)))
                (vals-parsed (cadr (assoc key result))))
           ;; For `\DeclareCaptionOption', only add the value
           ;; (remember:      key=^^^^^^, val="defined key")
           (if (string-equal key "option")
               (cl-pushnew (list val) result :test #'equal)
             (cl-pushnew (list key (TeX-delete-duplicate-strings
                                    (append vals-predefined
                                            vals-parsed
                                            (list val))))
                         result :test #'equal))))))
   LaTeX-caption-key-val-options))

(defvar LaTeX-caption-supported-float-types
  '("figure" "table" "ContinuedFloat"   ; Standard caption.sty
    "sub" "subtable" "subfigure"        ; subcaption.sty
    "bi" "bi-first" "bi-second"         ; bicaption.sty
    "ruled" "boxed"                     ; float.sty
    "floatingfigure" "floatingtable"    ; floatflt.sty
    "lstlisting"                        ; listings.sty
    "longtable"                         ; longtable.sty
    "figwindow" "tabwindow"             ; picinpar.sty
    "parpic"                            ; picins.sty
    "SCfigure" "SCtable"                ; sidecap.sty
    "supertabular" "xtabular"           ; supertabular.sty & xtab.sty
    "threeparttable" "measuredfigure"   ; threeparttable.sty
    "wrapfigure" "wraptable")           ; wrapfigure.sty
  "List of float types supported by `caption.sty'.
These types are provided by other LaTeX packages.")

;; Setup for various \DeclareCaption's:
(TeX-auto-add-type "caption-DeclareCaption" "LaTeX")

;; The 2. argument to `DeclareCaption[A-Za-z]' contains (La)TeX code.
;; We deliberately ignore that argument in our regex since it is not
;; needed for this style and would pollute the auto generated
;; `docname.el' file.
(defvar LaTeX-caption-DeclareCaption-regexp
  `(,(concat "\\\\DeclareCaption\\(Font\\|Format\\|Justification"
             "\\|LabelFormat\\|LabelSeparator\\|ListFormat"
             "\\|Option\\|Style\\|TextFormat\\)"
             "\\*?"
             "[ \t\n\r%]*"
             "{\\([^}]+\\)}")
    (0 1 2) LaTeX-auto-caption-DeclareCaption)
  "Matches the arguments of different `\\DeclareCaption*' from `caption.sty'.")

(defun LaTeX-caption-auto-prepare ()
  "Clear `LaTeX-auto-caption-DeclareCaption' before parsing."
  (setq LaTeX-auto-caption-DeclareCaption nil))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-caption-auto-prepare t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

;; In `LaTeX-caption-DeclareCaption-regexp', we match (0 1 2).  When
;; adding a new `Name', we need something unique for `0'-match until
;; the next `C-c C-n'.  We mimic that regex-match bei concat'ing the
;; elements.  It will vanish upon next `C-c C-n'.
(defun LaTeX-arg-caption-DeclareCaption (optional format)
  "Insert various `\\DeclareCaptionFORMAT' commands.
If OPTIONAL, insert argument in square brackets.  FORMAT is the
suffix of the LaTeX macro."
  (let ((name (TeX-read-string "Name: ")))
    (LaTeX-add-caption-DeclareCaptions
     (list (concat "\\DeclareCaption" format "{" name "}")
           format name))
    (TeX-argument-insert name optional)))

;; Support for an undocumented feature of caption.sty:
;; `\captionbox' sets the width of the caption equal to the width of
;; the contents (a feature provided e.g. by `threeparttable.sty').
;; The starred version typesets the caption without label and without
;; entry to the list of figures or tables.

;; The first mandatory argument {<heading>} contains the caption text
;; and the label.  We used to use `TeX-insert-macro' to do the job
;; (Thanks to M. Giordano for his valuable comments on this!), but now
;; moved to `LaTeX-label'.

;; Syntax:
;; \captionbox[<list entry>]{<heading>}[<width>][<inner-pos>]{<contents>}
;; \captionbox*{<heading>}[<width>][<inner-pos>]{<contents>}

(defun LaTeX-arg-caption-captionbox (optional &optional star)
  "Query for the arguments of \"\\captionbox\" incl. a label and insert them.
If OPTIONAL is non-nil, indicate it while reading the caption.
If STAR is non-nil, then do not query for a \\label and a short
caption, insert only a caption."
  (let* ((currenv (LaTeX-current-environment))
         (caption (TeX-read-string
                   (TeX-argument-prompt optional nil "Caption")))
         (short-caption
          (when (and (not star)
                     (>= (length caption) LaTeX-short-caption-prompt-length))
            (TeX-read-string
             (TeX-argument-prompt t nil "Short caption")))))
    (indent-according-to-mode)
    (when (and short-caption (not (string= short-caption "")))
      (insert LaTeX-optop short-caption LaTeX-optcl))
    (insert TeX-grop caption)
    (unless star (LaTeX-label currenv 'environment))
    (insert TeX-grcl))
  (let* ((TeX-arg-opening-brace "[")
         (TeX-arg-closing-brace "]")
         (TeX-last-optional-rejected nil)
         (width (LaTeX-check-insert-macro-default-style
                 (completing-read (TeX-argument-prompt t nil "Width")
                                  (mapcar (lambda (elt) (concat TeX-esc (car elt)))
                                          (LaTeX-length-list)))))
         (TeX-last-optional-rejected (or (not width)
                                         (and width (string= width ""))))
         (inpos (LaTeX-check-insert-macro-default-style
                 (if (and width (not (string-equal width "")))
                     (completing-read (TeX-argument-prompt t nil "Inner position")
                                      '("c" "l" "r" "s"))
                   ""))))
    (and width (TeX-argument-insert width t))
    (and inpos (TeX-argument-insert inpos t)))
  ;; Fill the paragraph before inserting {}.  We can use
  ;; `LaTeX-fill-paragraph' without messing up the code since
  ;; \caption starts a new paragraph with AUCTeX
  ;; (cf. `paragraph-start').
  (when auto-fill-function (LaTeX-fill-paragraph)))

(defun LaTeX-arg-caption-captionof (optional &optional star)
  "Query for the arguments of \"\\captionof\" macro and insert them.
If OPTIONAL is non-nil, insert the arguments in brackets.  If
STAR is non-nil, do not query for a short-caption and a label."
  (let* ((envtype (completing-read (TeX-argument-prompt optional nil "Float type")
                                   LaTeX-caption-supported-float-types))
         (figtypes '("figure" "subfigure" "floatingfigure"
                     "figwindow" "SCfigure" "measuredfigure" "wrapfigure"))
         (tabtypes '("table"  "subtable" "floatingtable"  "tabwindow" "SCtable"
                     "supertabular" "xtabular" "threeparttable"  "wraptable"))
         (caption (TeX-read-string
                   (TeX-argument-prompt optional nil "Caption")))
         (short-caption
          (when (and (not star)
                     (>= (length caption) LaTeX-short-caption-prompt-length))
            (TeX-read-string
             (TeX-argument-prompt t nil "Short caption")))))
    (indent-according-to-mode)
    (TeX-argument-insert envtype optional)
    (when (and short-caption (not (string= short-caption "")))
      (insert LaTeX-optop short-caption LaTeX-optcl))
    (TeX-argument-insert caption optional)
    (when auto-fill-function (LaTeX-fill-paragraph))
    (when (and (not star)
               ;; Check if `envtype' is a figure or a table, also
               ;; consult `LaTeX-label-alist' for additions from user
               ;; or newfloat.el, then run `LaTeX-label' w/
               ;; 'environment arg, otherwise w/o.
               (save-excursion
                 (if (or (member envtype figtypes)
                         (member envtype tabtypes)
                         (assoc envtype LaTeX-label-alist))
                     (LaTeX-label (cond ((member envtype figtypes)
                                         "figure")
                                        ((member envtype tabtypes)
                                         "table")
                                        (t envtype))
                                  'environment)
                   (LaTeX-label envtype))))
      (LaTeX-newline)
      (indent-according-to-mode)
      (end-of-line))))

(TeX-add-style-hook
 "caption"
 (lambda ()

   ;; Add caption to the parser.
   (TeX-auto-add-regexp LaTeX-caption-DeclareCaption-regexp)

   ;; Run style hook for ltcaption if longtable package is loaded:
   (when (member "longtable" (TeX-style-list))
     (TeX-run-style-hooks "ltcaption"))

   ;; Caption commands:
   (TeX-add-symbols
    '("caption*" t)

    '("captionlistentry"
      [TeX-arg-completing-read LaTeX-caption-supported-float-types
                               "Float type"]
      t)

    '("captionof" LaTeX-arg-caption-captionof)

    '("captionof*" (LaTeX-arg-caption-captionof t))

    '("captionsetup"
      (TeX-arg-conditional (member "bicaption" (TeX-style-list))
                           ([LaTeX-arg-bicaption-captionsetup])
                           ([TeX-arg-completing-read
                             LaTeX-caption-supported-float-types "Float type"]))
      (TeX-arg-key-val (LaTeX-caption-key-val-options)))

    '("captionsetup*"
      (TeX-arg-conditional (member "bicaption" (TeX-style-list))
                           ([LaTeX-arg-bicaption-captionsetup])
                           ([TeX-arg-completing-read
                             LaTeX-caption-supported-float-types "Float type"]))
      (TeX-arg-key-val (LaTeX-caption-key-val-options)))

    '("clearcaptionsetup"
      [TeX-arg-completing-read (LaTeX-caption-key-val-options) "Single key"]
      (TeX-arg-completing-read LaTeX-caption-supported-float-types "Float type"))

    '("clearcaptionsetup*"
      [TeX-arg-completing-read (LaTeX-caption-key-val-options) "Single key"]
      (TeX-arg-completing-read LaTeX-caption-supported-float-types "Float type"))

    '("captionbox"  (LaTeX-arg-caption-captionbox) t)

    '("captionbox*" (LaTeX-arg-caption-captionbox t) t)

    '("continuedfloat" 0)
    '("continuedfloat*" 0)

    '("DeclareCaptionFont"
      (LaTeX-arg-caption-DeclareCaption "Font") t)

    '("DeclareCaptionFormat"
      (LaTeX-arg-caption-DeclareCaption "Format") t)

    '("DeclareCaptionFormat*"
      (LaTeX-arg-caption-DeclareCaption "Format") t)

    '("DeclareCaptionJustification"
      (LaTeX-arg-caption-DeclareCaption "Justification") t)

    '("DeclareCaptionLabelFormat"
      (LaTeX-arg-caption-DeclareCaption "LabelFormat") t)

    '("DeclareCaptionLabelSeparator"
      (LaTeX-arg-caption-DeclareCaption "LabelSeparator") t)

    '("DeclareCaptionLabelSeparator*"
      (LaTeX-arg-caption-DeclareCaption "LabelSeparator") t)

    '("DeclareCaptionListFormat"
      (LaTeX-arg-caption-DeclareCaption "ListFormat") t)

    '("DeclareCaptionOption"
      (LaTeX-arg-caption-DeclareCaption "Option") t)

    '("DeclareCaptionStyle"
      (LaTeX-arg-caption-DeclareCaption "Style")
      [TeX-arg-key-val (LaTeX-caption-key-val-options) "Additional options"]
      (TeX-arg-key-val (LaTeX-caption-key-val-options) "Options"))

    '("DeclareCaptionTextFormat"
      (LaTeX-arg-caption-DeclareCaption "TextFormat") t)

    '("bothIfFirst" 2)

    '("bothIfSecond" 2))

   ;; \caption(box|of) macros should get their own lines
   (LaTeX-paragraph-commands-add-locally '("captionbox" "captionof"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("caption"           "*[{")
                                ("captionlistentry"  "[{")
                                ("captionof"         "*{[{")
                                ("captionbox"        "*[{[["))
                              'textual)
     (font-latex-add-keywords '(("captionsetup"                  "*[[{")
                                ("clearcaptionsetup"             "*[{")
                                ("continuedfloat"                "")
                                ("DeclareCaptionFont"            "{{")
                                ("DeclareCaptionFormat"          "*{{")
                                ("DeclareCaptionJustification"   "{{")
                                ("DeclareCaptionLabelFormat"     "{{")
                                ("DeclareCaptionLabelSeparator"  "*{{")
                                ("DeclareCaptionListFormat"      "{{")
                                ("DeclareCaptionOption"          "{{")
                                ("DeclareCaptionStyle"           "{[{")
                                ("DeclareCaptionTextFormat"      "{{"))
                              'function)) )
 TeX-dialect)

(defun LaTeX-caption-package-options ()
  "Prompt for package options for the caption package."
  (TeX-read-key-val t
                    (append '(("compatibility"  ("true" "false")))
                            '(("figureposition" ("top" "above" "bottom" "below")))
                            '(("tableposition"  ("top" "above" "bottom" "below")))
                            LaTeX-caption-key-val-options)))

;;; caption.el ends here
