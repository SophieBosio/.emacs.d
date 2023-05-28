;;; beamer.el --- AUCTeX style for the latex-beamer class  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2022  Free Software Foundation, Inc.

;; Author: Thomas Baumann <thomas.baumann@ch.tum.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2003-12-20
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

;; This file adds support for the latex-beamer class.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defun LaTeX-beamer-after-insert-env (env start _end)
  "Do beamer-specific stuff after the insertion of an environment."
  ;; Add `fragile' as an optional argument to the frame environment if
  ;; a verbatim environment is inserted.
  (when (and (member env (LaTeX-verbatim-environments))
             (save-excursion
               (goto-char start)
               (string-equal (LaTeX-current-environment) "frame")))
    (save-excursion
      (when (re-search-backward "\\\\begin[ \t]*{frame}" nil t)
        (let ((end-of-begin (match-end 0)))
          (goto-char end-of-begin)
          ;; FIXME: Add support for skipping over overlay options.
          (while (forward-comment 1))
          (if (eq (char-after) (string-to-char LaTeX-optop))
              (progn
                (forward-char)
                (insert "fragile")
                (unless (looking-at (concat "[ \t]*" LaTeX-optcl))
                  (insert ",")))
            (goto-char end-of-begin)
            (insert "[fragile]")))))))

(defvar LaTeX-beamer-frametitle-history nil
  "History of frame titles in beamer.")

(TeX-add-style-hook
 "beamer"
 (lambda ()
   (add-hook 'LaTeX-after-insert-env-hook #'LaTeX-beamer-after-insert-env nil t)

   (TeX-run-style-hooks "amsmath" "amssymb" "amsthm" "color" "geometry"
                        "hyperref" "inputenc" "translator" "xcolor"
                        "graphicx")

   (LaTeX-section-list-add-locally
    '(("part" 0)
      ("section" 1)
      ("subsection" 2)
      ("subsubsection" 3))
    t)
   (LaTeX-largest-level-set "part")
   (make-local-variable 'LaTeX-section-hook)
   (setq LaTeX-section-hook
         '(LaTeX-section-heading
           LaTeX-section-title
           LaTeX-beamer-section))
   (if LaTeX-beamer-section-labels-flag
       (setq LaTeX-section-hook
             (append LaTeX-section-hook '(LaTeX-section-label))))

   (setq LaTeX-item-list
         (append '(("itemize" . LaTeX-item-beamer)
                   ("enumerate" . LaTeX-item-beamer)
                   ("thebibliography" . LaTeX-bibitem-beamer))
                 LaTeX-item-list))

   (setq LaTeX-default-document-environment "frame")

   (LaTeX-paragraph-commands-add-locally "frametitle")

   (TeX-add-symbols
    '("AtBeginSection" ["Special star text"] t)
    '("AtBeginSubsection" ["Special star text"] t)
    '("AtBeginSubsubsection" ["Special star text"] t)
    '("AtBeginPart" t)
    '("AtBeginLecture" t)
    '("AtBeginNote" t)
    '("AtEndNote" t)
    '("action" [TeX-arg-beamer-overlay-spec "Action spec"] t)
    '("againframe" [TeX-arg-beamer-overlay-spec]
      [TeX-arg-beamer-default-overlay-spec]
      ["Options"] "Frame label")
    '("alert" [TeX-arg-beamer-overlay-spec] 1)
    '("alt" TeX-arg-beamer-overlay-spec "Text on specified slides"
      "Text on other slides")
    '("appendix" [TeX-arg-beamer-overlay-spec "Mode spec"])
    '("author" [LaTeX-arg-author "Short author names"] LaTeX-arg-author)
    '("beamerdefaultoverlayspecification"
      TeX-arg-beamer-default-overlay-spec)
    '("beamerbutton" 1)
    '("beamergotobutton" 1)
    '("beamerreturnbutton" 1)
    '("beamerskipbutton" 1)
    '("column" [TeX-arg-beamer-overlay-spec] ["Placement(t,T,c,b)"]
      (TeX-arg-length "Column width"))
    '("date" [TeX-arg-date "Short date"] TeX-arg-date)
    ;; Beamer frame macro is obsolete, and standard LaTeX frame macro
    ;; is available in frame environment.
    ;; '("frame" TeX-arg-beamer-frametitle)
    "framebreak" "noframebreak"
    '("framelatex" t)
    '("frametitle"
      [TeX-arg-beamer-overlay-spec] ["Short title"]
      (TeX-arg-string "Title" nil LaTeX-beamer-frametitle-history))
    '("framesubtitle" [TeX-arg-beamer-overlay-spec] "Subtitle")
    '("framezoom" (TeX-arg-beamer-overlay-spec "Overlay having button")
      (TeX-arg-beamer-overlay-spec "Zoomed overlay") ["Options"]
      (TeX-arg-pair "Upper left X" "Upper left Y")
      (TeX-arg-pair "Zoom area width" "Zoom area depth"))
    '("hyperlink" [TeX-arg-beamer-overlay-spec] "Target name" t)
    '("hyperlinkslideprev" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkslidenext" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkframestart" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkframeend" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkframestartnext" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkframeendprev" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinksectionstart" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinksectionend" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinksectionstartnext" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinksectionendprev" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinksubsectionstart" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinksubsectionend" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinksubsectionstartnext" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinksubsectionendprev" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkpresentationstart" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkpresentationend" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkappendixstart" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkappendixend" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkdocumentstart" [TeX-arg-beamer-overlay-spec] 1)
    '("hyperlinkdocumentend" [TeX-arg-beamer-overlay-spec] 1)
    '("hypertarget" [TeX-arg-beamer-overlay-spec] "Target name" t)
    '("includegraphics" [TeX-arg-beamer-overlay-spec]
      [TeX-arg-key-val (LaTeX-graphicx-key-val-options) nil nil ?\s]
      LaTeX-arg-includegraphics)
    '("includeonlyframes" "Frame label(s)")
    '("includeonlylecture" "Lecture label")
    '("includeslide" ["Options"] "Label")
    '("institute" ["Short institute name"] t)
    '("invisible" [TeX-arg-beamer-overlay-spec] 1)
    '("keywords" t)
    '("label" [TeX-arg-beamer-overlay-spec] TeX-arg-label)
    '("lecture" ["Short lecture name"] "Lecture name" "Lecture label")
    '("logo" 1)
    '("mode" [TeX-arg-beamer-overlay-spec "Mode"] t)
    "mode*"

    ;; Adapted copies from latex.el
    '("newcommand<>" TeX-arg-define-macro [TeX-arg-define-macro-arguments] t)
    '("renewcommand<>" TeX-arg-macro [TeX-arg-define-macro-arguments] t)
    '("newenvironment<>" TeX-arg-define-environment
      [TeX-arg-define-macro-arguments] 2)
    '("renewenvironment<>" TeX-arg-environment
      [TeX-arg-define-macro-arguments] 2)

    ;; TODO: Support the case \note is used outside frame env.
    '("note" [TeX-arg-beamer-overlay-spec] ["Options"] t)
    '("only" [TeX-arg-beamer-overlay-spec] 1)
    '("onslide" [TeX-arg-beamer-overlay-spec])
    "partpage"
    '("pause" ["Slide number"])
    '("resetcounteronoverlays" TeX-arg-counter)
    "sectionpage"
    '("setbeameroption" t)
    '("setbeamersize" t)
    '("setjobnamebeamerversion" "Filename without extension")
    '("structure" [TeX-arg-beamer-overlay-spec] 1)
    "subsectionpage"
    '("subtitle" ["Short subtitle"] t)
    '("subject" t)
    '("tableofcontents" ["Options"])
    '("temporal" TeX-arg-beamer-overlay-spec
      "Before slide text" "Default text" "After slide text")
    "titlepage"
    '("title" ["Short title"] t)
    '("titlegraphic" 1)

    '("transblindshorizontal" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transblindsvertical" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transboxin" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transboxout" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transcover" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transdissolve" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transfade" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transfly" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transglitter" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transpush" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transreplace" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transsplitverticalin" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transsplitverticalout" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transsplithorizontalin" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transsplithorizontalout" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transwipe" [TeX-arg-beamer-overlay-spec] ["Options"])
    '("transduration" [TeX-arg-beamer-overlay-spec] "Seconds")

    '("uncover" [TeX-arg-beamer-overlay-spec] t)
    '("usetheme" ["Options"]
      (TeX-arg-completing-read (LaTeX-beamer-themes-list) "Theme"))
    '("useinnertheme" ["Options"]
      (TeX-arg-completing-read (LaTeX-beamer-inner-themes-list) "Theme"))
    '("useoutertheme" ["Options"]
      (TeX-arg-completing-read (LaTeX-beamer-outer-themes-list) "Theme"))
    '("usecolortheme" ["Options"]
      (TeX-arg-completing-read (LaTeX-beamer-color-themes-list) "Theme"))
    '("usefonttheme" ["Options"]
      (TeX-arg-completing-read (LaTeX-beamer-font-themes-list) "Theme"))
    '("setbeamercolor" "Beamer color" "Color specification")
    '("setbeamercolor*" "Beamer color" "Color specification")
    '("usebeamercolor" ["fg or bg"] "Beamer color")
    '("usebeamercolor*" ["fg or bg"] "Beamer color")
    '("setbeamerfont" "Beamer font" "Font attributes")
    '("setbeamerfont*" "Beamer font" "Font attributes")
    '("usebeamerfont" "Beamer font")
    '("usebeamerfont*" "Beamer font")
    '("setbeamertemplate" "Element" ["Predefined option"] t)
    '("addtobeamertemplate" "Element" "Pre text" "Post test")
    '("defbeamertemplate" [TeX-arg-beamer-overlay-spec "Mode spec"]
      "Element" "Predefined option" [TeX-arg-define-macro-arguments] t)
    '("defbeamertemplatealias" "Element" "New predefined option"
      "Existing predefined option")
    '("defbeamertemplateparent" "Parent template name"
      ["Predefined option"] "Child template list"
      [TeX-arg-define-macro-arguments] t)
    '("ifbeamercolorempty" ["fg or bg"] "Beamer color name"
      "If undefined" "If defined")
    '("setbeamercovered" "Options")
    '("opaqueness" [TeX-arg-beamer-overlay-spec] "Opaqueness (in %)")

    '("visible" [TeX-arg-beamer-overlay-spec] 1))

   ;; TODO: Font commands accept overlay specs.
   ;; Support commands to manipulate templates?

   (LaTeX-add-environments
    '("abstract" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"])
    '("actionenv" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"])
    '("alertblock" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] "Title")
    '("beamerboxesrounded" ["Options"] "Header")
    '("beamercolorbox" ["Options"] "Beamer color")
    '("block" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] "Title")
    '("column" LaTeX-env-args [TeX-arg-beamer-overlay-spec]
      ["Placement(t,T,c,b)"] (TeX-arg-length "Width"))
    '("columns" LaTeX-env-args [TeX-arg-beamer-overlay-spec] ["Options"])
    ;; FIXME: Not on user reference.
    ;; "columnsonlytextwidth"
    '("exampleblock" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] "Title")
    '("frame" LaTeX-env-args [TeX-arg-beamer-overlay-spec]
      [TeX-arg-beamer-default-overlay-spec]
      [TeX-arg-key-val (("allowdisplaybreaks" ("0" "1" "2" "3" "4"))
                        ("allowframebreaks") ("b") ("c") ("t") ("s")
                        ("noframenumbering")
                        ("fragile" ("singleslide"))
                        ("environment") ("label") ("plain") ("shrink")
                        ("squeeze"))]
      LaTeX-beamer-env-frame)

    '("onlyenv" LaTeX-env-args [TeX-arg-beamer-overlay-spec])
    '("altenv" LaTeX-env-args [TeX-arg-beamer-overlay-spec]
      "Begin text" "End text" "Alternate begin text" "Alternate end text")
    '("visibleenv" LaTeX-env-args [TeX-arg-beamer-overlay-spec])
    '("uncoverenv" LaTeX-env-args [TeX-arg-beamer-overlay-spec])
    '("invisibleenv" LaTeX-env-args [TeX-arg-beamer-overlay-spec])

    '("structureenv" LaTeX-env-args [TeX-arg-beamer-overlay-spec])
    '("alertenv" LaTeX-env-args [TeX-arg-beamer-overlay-spec])

    '("theorem" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] ["Additional text"])
    '("corollary" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] ["Additional text"])
    '("definition" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] ["Additional text"])
    '("definitions" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] ["Additional text"])
    '("fact" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] ["Additional text"])
    '("lemma" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] ["Additional text"])
    '("example" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] ["Additional text"])
    '("examples" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] ["Additional text"])
    '("proof" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"] ["Proof name"])

    '("overlayarea" LaTeX-env-args
      (TeX-arg-length "Area width") (TeX-arg-length "Area height"))
    '("overprint" LaTeX-env-args [TeX-arg-length "Area width"])
    "semiverbatim"

    '("verse" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"])
    '("quotation" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"])
    '("quote" LaTeX-env-args
      [TeX-arg-beamer-overlay-spec "Action spec"]))

   ;; TODO: itemize, enumerate and description envs accept default
   ;; overlay spec as optional argument.
   ;; We definitely need more completion support for both macros and
   ;; environments.

   (LaTeX-add-counters "lecture" "part" "section" "subsection" "subsubsection"
                       "subsectionslide" "framenumber" "figure" "table"
                       "beamerpauses")
   (LaTeX-add-pagestyles "navigation")
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
                '("semiverbatim" current-indentation) t)
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("title" "[{")
                                ("subtitle" "[{")
                                ("author" "[{")
                                ("date" "[{")
                                ("institute" "[{")
                                ("frametitle" "<[{")
                                ("framesubtitle" "<[{"))
                              'slide-title)
     (font-latex-add-keywords '(("item" "<[")
                                ("bibitem" "<[{"))
                              'textual)
     (font-latex-add-keywords '(("textbf" "<{")
                                ("textsc" "<{")
                                ("textup" "<{"))
                              'bold-command)
     (font-latex-add-keywords '(("emph" "<{")
                                ("textit" "<{")
                                ("textsl" "<{"))
                              'italic-command)
     (font-latex-add-keywords '(("textmd" "<{")
                                ("textrm" "<{")
                                ("textsf" "<{")
                                ("texttt" "<{")
                                ("textnormal" "<{")
                                ("textcolor" "<[{"))
                              'type-command)
     (font-latex-add-keywords '(("color" "<[{"))
                              'type-declaration)
     (font-latex-add-keywords '(("label" "<{")
                                ("hyperlink" "<{{<")
                                ("hypertarget" "<{{"))
                              'reference)
     (font-latex-add-keywords '(("pause" "[")
                                ("logo" "{")
                                ("newcommand" "<|{\\[[{")
                                ("renewcommand" "<|{\\[[{")
                                ("newenvironment" "<{[[{{")
                                ("renewenvironment" "<{[[{{")
                                ("AtBeginSection" "[{")
                                ("AtBeginSubsection" "[{")
                                ("AtBeginSubsubsection" "[{")
                                ("AtBeginPart" "{")
                                ("AtBeginLecture" "{")
                                ("titlepage" "")
                                ("sectionpage" "")
                                ("subsectionpage" "")
                                ("partpage" "")
                                ("tableofcontents" "[")
                                ("column" "<[{")
                                ("againframe" "<[[{")
                                ("usetheme" "[{")
                                ("usecolortheme" "[{")
                                ("usefonttheme" "[{")
                                ("useinnertheme" "[{")
                                ("useoutertheme" "[{")
                                ("includeonlyframes" "{")
                                ("includeonlylecture" "{"))
                              'function)
     (font-latex-add-keywords '(("resetcounteronoverlays" "{"))
                              'variable))
   ;; TODO: Adjust section commands to accept <> option.

   ;; Additional intermediate files
   (add-to-list (make-local-variable 'LaTeX-clean-intermediate-suffixes)
                "\\.vrb"))
 TeX-dialect)

(defun TeX-arg-beamer-overlay-spec (optional &optional prompt)
  "Prompt for overlay specification.
If OPTIONAL is non-nil, insert the specification only if
non-empty and enclosed in \"<>\".  PROMPT replaces the standard
one."
  (TeX-arg-string optional (or prompt "Overlay") nil nil nil "<" ">")
  (indent-according-to-mode))

(defun TeX-arg-beamer-default-overlay-spec (optional)
  "Prompt for default overlay specification.
If OPTIONAL is non-nil, insert the specification only if
non-empty and enclosed in \"<>\".  If empty and OPTIONAL is nil,
insert just \"{}\"."
  (let ((spec (TeX-read-string
               (TeX-argument-prompt optional nil "Default overlay"))))
    (cond
     ((< 0 (length spec))
      (TeX-argument-insert (concat "<" spec ">") optional))
     ((not optional)
      ;; Clear default for \beamerdefaultoverlayspecification.
      (insert TeX-grop TeX-grcl))
     (t
      ;; Nop for clarity.
      nil))))

;; (defun TeX-arg-beamer-frametitle (_optional &optional _prompt)
;;   "Prompt for the frametitle."
;;   (let ((title (TeX-read-string "Title: " nil 'LaTeX-beamer-frametitle-history)))
;;     (if (not (zerop (length title)))
;;         (insert TeX-grop TeX-esc "frametitle" TeX-grop
;;                 title TeX-grcl TeX-grcl)
;;       (insert TeX-grop TeX-grcl))))

(defun LaTeX-item-beamer (&optional macro)
  "Insert a new item with an optional overlay argument.
You can turn off the prompt for the overlay argument by setting
`LaTeX-beamer-item-overlay-flag' to nil.  Calling the function
with a prefix argument prompts for the overlay specification
unconditionally.

Optional MACRO can be, for example, \"bibitem\"."
  (TeX-insert-macro (or macro "item"))
  (delete-horizontal-space)
  (if (or current-prefix-arg LaTeX-beamer-item-overlay-flag)
      (TeX-arg-beamer-overlay-spec t))
  (insert " "))

(defun LaTeX-bibitem-beamer ()
  "Insert a new bibitem with an optional overlay argument.
You can turn off the prompt for the overlay argument by setting
`LaTeX-beamer-item-overlay-flag' to nil.  Calling the function
with a prefix argument prompts for the overlay specification
unconditionally."
  (LaTeX-item-beamer "bibitem"))

(defun LaTeX-beamer-search-themes (&optional regexp extensions length)
  "Search for beamer themes matching REGEXP with EXTENSIONS.
The function removes the first LENGTH characters and the
extension of the file and returns a list of strings.  LENGTH may
also be a string.  Then the length of the string is used."
  (let* ((match (or regexp "^beamertheme[A-Z]"))
         (exts  (or extensions '("tex" "sty")))
         (chars (cond ((integerp length)
                       length)
                      ((stringp length)
                       (length length))
                      ;; Try some DWIM magic...
                      ((and (not length)
                            (string-match "beamer[A-Za-z0-9]*theme" match))
                       (- (match-end 0) (match-beginning 0)))
                      (t (error "Invalid length: `%s'" length)))))
    ;; (message "match=`%s' chars=`%s'" match chars)
    (TeX-delete-duplicate-strings
     (delete nil
             (mapcar
              (lambda (file)
                (let ((case-fold-search nil))
                  (and (numberp (string-match match file))
                       (substring-no-properties file chars))))
              (TeX-search-files nil exts t t))))))

(defun LaTeX-beamer-themes-list ()
  "Return a list of beamer themes for completion."
  (cond ((eq LaTeX-beamer-themes 'local)
         (set (make-local-variable 'LaTeX-beamer-themes)
              (LaTeX-beamer-search-themes)))
        ((functionp LaTeX-beamer-themes)
         (funcall LaTeX-beamer-themes))
        ((listp LaTeX-beamer-themes)
         LaTeX-beamer-themes)
        (t (error
            "`LaTeX-beamer-themes' should be a list: `%s'"
            LaTeX-beamer-themes))))

(defun LaTeX-beamer-inner-themes-list ()
  "Return a list of beamer inner themes for completion."
  (cond ((eq LaTeX-beamer-inner-themes 'local)
         (set (make-local-variable 'LaTeX-beamer-inner-themes)
              (LaTeX-beamer-search-themes "^beamerinnertheme")))
        ((functionp LaTeX-beamer-inner-themes)
         (funcall LaTeX-beamer-inner-themes))
        ((listp LaTeX-beamer-inner-themes)
         LaTeX-beamer-inner-themes)
        (t (error
            "`LaTeX-beamer-inner-themes' should be a list: `%s'"
            LaTeX-beamer-inner-themes))))

(defun LaTeX-beamer-outer-themes-list ()
  "Return a list of beamer outer themes for completion."
  (cond ((eq LaTeX-beamer-outer-themes 'local)
         (set (make-local-variable 'LaTeX-beamer-outer-themes)
              (LaTeX-beamer-search-themes "^beameroutertheme")))
        ((functionp LaTeX-beamer-outer-themes)
         (funcall LaTeX-beamer-outer-themes))
        ((listp LaTeX-beamer-outer-themes)
         LaTeX-beamer-outer-themes)
        (t (error
            "`LaTeX-beamer-outer-themes' should be a list: `%s'"
            LaTeX-beamer-outer-themes))))

(defun LaTeX-beamer-color-themes-list ()
  "Return a list of beamer color themes for completion."
  (cond ((eq LaTeX-beamer-color-themes 'local)
         (set (make-local-variable 'LaTeX-beamer-color-themes)
              (LaTeX-beamer-search-themes "^beamercolortheme")))
        ((functionp LaTeX-beamer-color-themes)
         (funcall LaTeX-beamer-color-themes))
        ((listp LaTeX-beamer-color-themes)
         LaTeX-beamer-color-themes)
        (t (error
            "`LaTeX-beamer-color-themes' should be a list: `%s'"
            LaTeX-beamer-color-themes))))

(defun LaTeX-beamer-font-themes-list ()
  "Return a list of beamer font themes for completion."
  (cond ((eq LaTeX-beamer-font-themes 'local)
         (set (make-local-variable 'LaTeX-beamer-font-themes)
              (LaTeX-beamer-search-themes "^beamerfonttheme")))
        ((functionp LaTeX-beamer-font-themes)
         (funcall LaTeX-beamer-font-themes))
        ((listp LaTeX-beamer-font-themes)
         LaTeX-beamer-font-themes)
        (t (error
            "`LaTeX-beamer-font-themes' should be a list: `%s'"
            LaTeX-beamer-font-themes))))

(defun LaTeX-beamer-env-frame (_ignored)
  "Insert beamer frame environment title."
  (let ((title (TeX-read-string "(Optional) Title: " nil
                                'LaTeX-beamer-frametitle-history)))
    (unless (zerop (length title))
      (save-excursion
        (LaTeX-newline)
        ;; Indent the next macro insertion and don't rely on the
        ;; fill-function to do it:
        (indent-according-to-mode)
        (insert (format "\\frametitle{%s}" title))
        ;; This works because \frametitle is a paragraph command.
        (when auto-fill-function
          (backward-char)
          (LaTeX-fill-paragraph))))))

(defun LaTeX-beamer-section ()
  "Hook to prompt for beamer section and insert it."
  (let ((star (string-suffix-p "*" LaTeX-name))
        mode-spec short-title)
    (when (and LaTeX-level (< LaTeX-level 4))
      (setq mode-spec (TeX-read-string "(Optional) Mode spec: ")
            short-title (unless star
                          (TeX-read-string "(Optional) Short title: "))))
    ;; The rest of this function is adapted copy of
    ;; `LaTeX-section-section'.
    (unless (save-excursion
              (re-search-backward
               (concat "^\\s-*\n\\s-*\\=\\|^\\s-*" (regexp-quote TeX-esc)
                       "begin")
               (line-beginning-position 0) t))
      (LaTeX-newline))
    (insert TeX-esc (if star
                        (substring-no-properties LaTeX-name 0 -1)
                      LaTeX-name))
    (if (< 0 (length mode-spec))
        (insert "<" mode-spec ">"))
    (if star (insert "*"))
    (if (< 0 (length short-title))
        (insert LaTeX-optop short-title LaTeX-optcl))
    (insert TeX-grop)
    (if (zerop (length LaTeX-title))
        (set-marker LaTeX-done-mark (point)))
    (insert LaTeX-title TeX-grcl)
    (LaTeX-newline)
    ;; If RefTeX is available, tell it that we've just made a new section
    (and (fboundp 'reftex-notice-new-section)
         (reftex-notice-new-section))))

(defun LaTeX-beamer-class-options ()
  "Read the beamer class options from the user."
  (TeX-load-style "hyperref")
  (TeX-load-style "color")
  (TeX-load-style "xcolor")
  (TeX-read-key-val t '(("usepdftitle" ("false")) ("envcountsect")
                        ("notheorems") ("noamsthm") ("compress") ("t") ("c")
                        ("leqno") ("fleqn") ("handout") ("trans")
                        ("ignorenonframetext") ("onlytextwidth")
                        ("noamssymb") ("bigger") ("smaller") ("8pt") ("9pt")
                        ("10pt") ("11pt") ("12pt") ("14pt") ("17pt") ("20pt")
                        ("draft") ("CJK") ("cjk") ("pgf")
                        ("hyperref" LaTeX-hyperref-package-options-list)
                        ("color" LaTeX-color-package-options)
                        ("xcolor" LaTeX-xcolor-package-options)
                        ("ucs") ("utf8x") ("utf8")
                        ("aspectratio" ("2013" "1610" "169" "149" "141" "54" "43" "32")))))

;;; beamer.el ends here
