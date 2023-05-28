;;; minted.el --- AUCTeX style for `minted.sty' (v2.5)  -*- lexical-binding: t; -*-

;; Copyright (C) 2014--2023 Free Software Foundation, Inc.

;; Author: Tassilo Horn <tsdh@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-12-19
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

;; This file adds support for `minted.sty' (v2.5) from 2017/07/19.

;;; Code:

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
(declare-function LaTeX-add-newfloat-DeclareFloatingEnvironments
                  "newfloat" (&rest newfloat-declarefloatingenvironments))

(defvar font-latex-syntactic-keywords-extra)

(defvar LaTeX-minted-key-val-options
  '(("autogobble" ("true" "false"))
    ("baselinestretch" ("auto"))
    ("beameroverlays" ("true" "false"))
    ("breakafter")
    ("breakaftergroup" ("true" "false"))
    ("breakaftersymbolpre")
    ("breakaftersymbolpost")
    ("breakanywhere" ("true" "false"))
    ("breakanywheresymbolpre")
    ("breakanywheresymbolpost")
    ("breakautoindent" ("true" "false"))
    ("breakbefore")
    ("breakbeforegroup" ("true" "false"))
    ("breakbeforesymbolpre")
    ("breakbeforesymbolpost")
    ("breakbytoken" ("true" "false"))
    ("breakbytokenanywhere" ("true" "false"))
    ("breakindent")
    ("breakindentnchars")
    ("breaklines" ("true" "false"))
    ("breaksymbol")
    ("breaksymbolleft")
    ("breaksymbolright")
    ("breaksymbolindent")
    ("breaksymbolindentnchars")
    ("breaksymbolindentleft")
    ("breaksymbolindentleftnchars")
    ("breaksymbolindentright")
    ("breaksymbolseprightnchars")
    ("breaksymbolsep")
    ("breaksymbolsepnchars")
    ("breaksymbolsepleft")
    ("breaksymbolsepleftnchars")
    ("breaksymbolsepright")
    ("breaksymbolseprightnchars")
    ("bgcolor")
    ("codetagify")
    ("curlyquotes" ("true" "false"))
    ("encoding")
    ("escapeinside")
    ("firstline")
    ("firstnumber" ("auto" "last" "integer"))
    ("fontfamily" ("tt" "courier" "helvetica"))
    ("fontseries" ("auto"))
    ("fontsize" ("auto" "\\tiny" "\\large" "\\scriptsize" "\\Large"
                 "\\footnotesize" "\\LARGE" "\\small" "\\huge"
                 "\\normalsize" "\\Huge"))
    ("fontshape" ("auto"))
    ("formatcom")
    ("frame" ("none" "leftline" "topline" "bottomline" "lines" "single"))
    ("framerule")
    ("framesep")
    ("funcnamehighlighting" ("true" "false"))
    ("gobble")
    ("highlightcolor")
    ("highlightlines")
    ("keywordcase" ("lower" "upper" "capitalize"))
    ("label")
    ("labelposition" ("none" "topline" "bottomline" "all"))
    ("lastline")
    ("linenos" ("true" "false"))
    ("numberfirstline" ("true" "false"))
    ("numbers" ("left" "right" "both" "none"))
    ("mathescape" ("true" "false"))
    ("numberblanklines" ("true" "false"))
    ("numbersep")
    ("obeytabs" ("true" "false"))
    ("outencoding")
    ("python3" ("true" "false"))
    ("resetmargins" ("true" "false"))
    ("rulecolor")
    ("samepage" ("true" "false"))
    ("showspaces" ("true" "false"))
    ("showtabs" ("true" "false"))
    ("space")
    ("spacecolor")
    ("startinline" ("true" "false"))
    ;; FIXME: It would be nice to use the function
    ;; `LaTeX-minted-style-list' here, but with a file local var like:
    ;;     %%% TeX-command-extra-options: "-shell-escape"
    ;; in a .tex file, Emacs asks to apply a variable which is not
    ;; safe and does not restore the window; the splitted frame
    ;; remains.  I couldn't figure out why, so for now, I add the
    ;; styles from Pygments version 2.14.0 here.
    ("style" ("abap" "algol" "algol_nu" "arduino" "autumn"
              "borland" "bw" "colorful" "default" "dracula"
              "emacs" "friendly" "friendly_grayscale" "fruity"
              "github-dark" "gruvbox-dark" "gruvbox-light"
              "igor" "inkpot" "lilypond" "lovelace" "manni" "material"
              "monokai" "murphy" "native" "nord" "nord-darker"
              "one-dark" "paraiso-dark" "paraiso-light" "pastie" "perldoc"
              "rainbow_dash" "rrt" "sas" "solarized-dark" "solarized-light"
              "staroffice" "stata" "stata-dark" "stata-light"
              "tango" "trac" "vim" "vs" "xcode" "zenburn"))
    ("stepnumber")
    ("stepnumberfromfirst")
    ("stepnumberoffsetvalues" ("true" "false"))
    ("stripall" ("true" "false"))
    ("stripnl" ("true" "false"))
    ("tab")
    ("tabcolor")
    ("tabsize")
    ("texcl" ("true" "false"))
    ("texcomments" ("true" "false"))
    ("xleftmargin")
    ("xrightmargin"))
  "Key=value options for minted macros and environments.")

(defun LaTeX-minted-key-val-options ()
  "Return an updated list of key=vals from minted package.
This function retrieves values of (user) defined colors and
prepends them to variable `LaTeX-minted-key-val-options'."
  (append
   (when (or (member "xcolor" (TeX-style-list))
             (member "color" TeX-active-styles))
     (let* ((colorcmd (if (member "xcolor" TeX-active-styles)
                          #'LaTeX-xcolor-definecolor-list
                        #'LaTeX-color-definecolor-list))
            (colors (mapcar #'car (funcall colorcmd)))
            (keys '("bgcolor" "highlightcolor"
                    "rulecolor" "spacecolor" "tabcolor"))
            result)
       (dolist (key keys result)
         (push (list key colors) result))))
   LaTeX-minted-key-val-options))

(defvar LaTeX-minted-pygmentize-program (executable-find "pygmentize"))

(defvar LaTeX-minted-language-list nil
  "List containing languages provided by pymentize program.")

(defun LaTeX-minted-language-list (&rest _ignored)
  "Return a list of languages provided by pymentize program.
Update the variable `LaTeX-minted-language-list' if still nil."
  (or LaTeX-minted-language-list
      (when LaTeX-minted-pygmentize-program
        (with-temp-buffer
          (shell-command (concat LaTeX-minted-pygmentize-program " -L lexers")
                         (current-buffer))
          (goto-char (point-min))
          (let (languages)
            (while (re-search-forward "^\\*[[:space:]]\\([^:]+\\):" nil t)
              (dolist (lang (split-string (match-string 1) "[[:space:],]" t))
                (push lang languages)))
            (setq LaTeX-minted-language-list languages))
          LaTeX-minted-language-list))))

(defvar LaTeX-minted-style-list nil
  "List containing styles provided by pymentize program.")

(defun LaTeX-minted-style-list (&rest _ignored)
  "Return a list of styles provided by pymentize program.
Update the variable `LaTeX-minted-style-list' if still nil."
  (or LaTeX-minted-style-list
      (when LaTeX-minted-pygmentize-program
        (with-temp-buffer
          (shell-command (concat LaTeX-minted-pygmentize-program " -L styles")
                         (current-buffer))
          (goto-char (point-min))
          (let (styles)
            (while (re-search-forward "^\\*[[:space:]]\\([^:]+\\):" nil t)
              (dolist (style (split-string (match-string 1) "[[:space:],]" t))
                (push style styles)))
            (setq LaTeX-minted-style-list styles))
          LaTeX-minted-style-list))))

(defvar LaTeX-minted-auto-newminted nil)
(defvar LaTeX-minted-newminted-regexp
  '("\\\\newminted\\(?:\\[\\([^]]+\\)\\]\\)?{\\([^}]+\\)}{[^}]*}"
    (1 2) LaTeX-minted-auto-newminted))

(defvar LaTeX-minted-auto-newmint nil)
(defvar LaTeX-minted-newmint-regexp
  '("\\\\newmint\\(?:\\[\\([^]]+\\)\\]\\)?{\\([^}]+\\)}{[^}]*}"
    (1 2) LaTeX-minted-auto-newmint))

(defvar LaTeX-minted-auto-newmintinline nil)
(defvar LaTeX-minted-newmintinline-regexp
  '("\\\\newmintinline\\(?:\\[\\([^]]+\\)\\]\\)?{\\([^}]+\\)}{[^}]*}"
    (1 2) LaTeX-minted-auto-newmintinline))

(defvar LaTeX-minted-auto-newmintedfile nil)
(defvar LaTeX-minted-newmintedfile-regexp
  '("\\\\newmintedfile\\(?:\\[\\([^]]+\\)\\]\\)?{\\([^}]+\\)}{[^}]*}"
    (1 2) LaTeX-minted-auto-newmintedfile))

(defun LaTeX-minted-auto-prepare ()
  (setq LaTeX-minted-auto-newminted     nil
        LaTeX-minted-auto-newmint       nil
        LaTeX-minted-auto-newmintinline nil
        LaTeX-minted-auto-newmintedfile nil
        LaTeX-minted-language-list      nil
        LaTeX-minted-style-list         nil))

(defun LaTeX-minted-auto-cleanup ()
  ;; \newminted{lang}{opts} => new langcode and langcode* envs.
  ;; \newminted[envname]{lang}{opts} => new envname/envname* envs.
  (dolist (name-lang LaTeX-minted-auto-newminted)
    (let* ((env (if (> (length (car name-lang)) 0)
                    (car name-lang)
                  (concat (cadr name-lang) "code")))
           (env* (concat env "*")))
      (add-to-list 'LaTeX-auto-environment (list env))
      (add-to-list 'LaTeX-auto-environment
                   (list env* #'LaTeX-env-args
                         '(TeX-arg-key-val (LaTeX-minted-key-val-options))))
      (add-to-list 'LaTeX-indent-environment-list `(,env current-indentation) t)
      (add-to-list 'LaTeX-indent-environment-list `(,env* current-indentation) t)
      (add-to-list 'LaTeX-verbatim-environments-local env)
      (add-to-list 'LaTeX-verbatim-environments-local env*)))
  ;; \newmint{foo}{opts} => \foo[key=vals]|code|
  ;; \newmint[macname]{foo}{opts} => \macname[key=vals]|code|
  (dolist (name-lang LaTeX-minted-auto-newmint)
    (let ((lang (if (> (length (car name-lang)) 0)
                    (car name-lang)
                  (cadr name-lang))))
      (add-to-list 'TeX-auto-symbol
                   `(,lang [TeX-arg-key-val (LaTeX-minted-key-val-options)]
                           TeX-arg-verb))
      (add-to-list 'LaTeX-verbatim-macros-with-delims-local lang)
      (when (and (fboundp 'font-latex-add-keywords)
                 (eq TeX-install-font-lock 'font-latex-setup))
        (font-latex-add-keywords `((,lang "[")) 'textual))))
  ;; \newmintinline{foo}{opts} => \fooinline[key=vals]|code| or
  ;;                              \fooinline[key=vals]{code}
  ;; \newmintinline[macname]{foo}{opts} => \macname[key=vals]|code| or
  ;;                                       \macname[key=vals]{code}
  (dolist (name-lang LaTeX-minted-auto-newmintinline)
    (let ((lang (if (> (length (car name-lang)) 0)
                    (car name-lang)
                  (concat (cadr name-lang) "inline"))))
      (add-to-list 'TeX-auto-symbol
                   `(,lang [TeX-arg-key-val (LaTeX-minted-key-val-options)]
                           TeX-arg-verb-delim-or-brace))
      (add-to-list 'LaTeX-verbatim-macros-with-delims-local lang)
      (add-to-list 'LaTeX-verbatim-macros-with-braces-local lang)
      (when (and (fboundp 'font-latex-add-keywords)
                 (eq TeX-install-font-lock 'font-latex-setup))
        (font-latex-add-keywords `((,lang "[")) 'textual))))
  ;; \newmintedfile{foo}{opts} => \foofile[key=vals]{file-name}
  ;; \newmintedfile[macname]{foo}{opts} => \macname[key=vals]{file-name}
  (dolist (name-lang LaTeX-minted-auto-newmintedfile)
    (let ((lang (if (> (length (car name-lang)) 0)
                    (car name-lang)
                  (concat (cadr name-lang) "file"))))
      (add-to-list 'TeX-auto-symbol
                   `(,lang [TeX-arg-key-val (LaTeX-minted-key-val-options)]
                           TeX-arg-file))))
  (when (and (fboundp 'font-latex-set-syntactic-keywords)
             (eq TeX-install-font-lock 'font-latex-setup))
    ;; Refresh font-locking so that the verbatim envs take effect.
    (font-latex-set-syntactic-keywords)))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-minted-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-minted-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-minted-add-syntactic-keywords-extra (type macro)
  "Add MACRO from minted.sty to `font-latex-syntactic-keywords-extra'.
TYPE is one of the symbols `brace' or `delim' indicating how
verbatim text is enclosed after the macro.  MACRO is a string or
a list of strings."
  (let ((syntax (if (eq type 'brace)
                    '((1 "|") (2 "|"))
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
                     ;; The first mandatory argument
                     "\\(?:{[^}]+}\\)"
                     ;; With 'brace, allow braced sub-groups otherwise
                     ;; we stop matching too early.  With 'delim, copy
                     ;; font-latex.el:
                     (if (eq type 'brace)
                         (concat "\\({\\)"
                                   "\\(?:[^}{]*"
                                     "\\(?:{[^}{]*"
                                       "\\(?:{[^}{]*"
                                         "\\(?:{[^}{]*}[^}{]*\\)*"
                                       "}[^}{]*\\)*"
                                     "}[^}{]*\\)*"
                                   "\\)"
                                 "\\(}\\)")
                       (concat
                        ;; Opening delimiter
                        "\\([^a-z@*\n\f{]\\).*?"
                        ;; Closing delimiter
                        "\\(" (regexp-quote TeX-esc) "*\\)\\(\\1\\)")))))
    (add-to-list 'font-latex-syntactic-keywords-extra (append regexp syntax))))

(TeX-add-style-hook
 "minted"
 (lambda ()

   ;; New symbols
   (TeX-add-symbols
    '("mint"
      [TeX-arg-key-val (LaTeX-minted-key-val-options)]
      (TeX-arg-completing-read (LaTeX-minted-language-list) "Language")
      TeX-arg-verb)
    '("mintinline"
      [TeX-arg-key-val (LaTeX-minted-key-val-options)]
      (TeX-arg-completing-read (LaTeX-minted-language-list) "Language")
      TeX-arg-verb-delim-or-brace)
    '("newminted" ["Environment Name"]
      (TeX-arg-completing-read (LaTeX-minted-language-list) "Language")
      (TeX-arg-key-val (LaTeX-minted-key-val-options)))
    '("newmint" ["Macro Name"]
      (TeX-arg-completing-read (LaTeX-minted-language-list) "Language")
      (TeX-arg-key-val (LaTeX-minted-key-val-options)))
    '("newmintinline" ["Macro Name"]
      (TeX-arg-completing-read (LaTeX-minted-language-list) "Language")
      (TeX-arg-key-val (LaTeX-minted-key-val-options)))
    '("newmintedfile" ["Macro Name"]
      (TeX-arg-completing-read (LaTeX-minted-language-list) "Language")
      (TeX-arg-key-val (LaTeX-minted-key-val-options)))
    ;; 3.3 Formatting source code
    '("inputminted"
      [TeX-arg-key-val (LaTeX-minted-key-val-options)]
      (TeX-arg-completing-read (LaTeX-minted-language-list) "Language")
      TeX-arg-file)
    ;; 3.4 Using different styles
    '("usemintedstyle"
      [TeX-arg-completing-read (LaTeX-minted-language-list) "Language"]
      (TeX-arg-completing-read (LaTeX-minted-style-list) "Style"))
    ;; 5.2 Macro option usage
    '("setminted"
      [TeX-arg-completing-read (LaTeX-minted-language-list) "Language"]
      (TeX-arg-key-val (LaTeX-minted-key-val-options)))
    '("setmintedinline"
      [TeX-arg-completing-read (LaTeX-minted-language-list) "Language"]
      (TeX-arg-key-val (LaTeX-minted-key-val-options))))

   ;; New environments
   (LaTeX-add-environments
    '("minted" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-minted-key-val-options)]
      (TeX-arg-completing-read (LaTeX-minted-language-list) "Language")))

   ;; 4 Floating listings: If option "newfloat" is given, run the
   ;; style hook and use the interface provided by the style,
   ;; otherwise add "listing" manually
   (if (or (LaTeX-provided-package-options-member "minted" "newfloat")
           (LaTeX-provided-package-options-member "minted" "newfloat=true"))
       (progn
         (TeX-run-style-hooks "newfloat")
         (LaTeX-add-newfloat-DeclareFloatingEnvironments
          '("listing" "verbatim")))
     (LaTeX-add-environments '("listing" ["Float Position"]))
     (TeX-add-symbols '("listoflistings")
                      '("listingscaption")
                      '("listoflistingscaption"))
     (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
                  '("listing" current-indentation) t)
     (add-to-list 'LaTeX-label-alist '("listing" . LaTeX-listing-label) t)
     (when (fboundp 'reftex-add-label-environments)
       (reftex-add-label-environments
        '(("listing" ?l "lst:" "~\\ref{%s}" caption nil nil)))))

   ;; Add to the auto parser
   (TeX-auto-add-regexp LaTeX-minted-newminted-regexp)
   (TeX-auto-add-regexp LaTeX-minted-newmint-regexp)
   (TeX-auto-add-regexp LaTeX-minted-newmintinline-regexp)
   (TeX-auto-add-regexp LaTeX-minted-newmintedfile-regexp)

   ;; Filling
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
                '("minted" current-indentation) t)
   (add-to-list 'LaTeX-verbatim-environments-local "minted")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("usemintedstyle"  "[{")
                                ("setminted"       "[{")
                                ("setmintedinline" "[{")
                                ("newminted"       "[{{")
                                ("newmint"         "[{{")
                                ("newmintinline"   "[{{")
                                ("newmintedfile"   "[{{"))
                              'function)
     (font-latex-add-keywords '(("inputminted" "[{{")
                                ("mint"        "[{")
                                ("mintinline"  "[{"))
                              'textual)
     ;; Add \mint & \mintinline to
     ;; `font-latex-syntactic-keywords-extra' and cater for their
     ;; special syntax: \mint[optional]{lang}{verbatim} or
     ;;                 \mint[optional]{lang}|verbatim|
     (LaTeX-minted-add-syntactic-keywords-extra 'brace
                                                '("mint" "mintinline"))
     (LaTeX-minted-add-syntactic-keywords-extra 'delim
                                                '("mint" "mintinline"))
     ;; Tell font-lock about the update.
     (font-latex-set-syntactic-keywords)))
 TeX-dialect)

(defvar LaTeX-minted-package-options '("chapter"     "cache"
                                       "cachedir"    "finalizecache"
                                       "frozencache" "draft"
                                       "final"       "kpsewhich"
                                       "langlinenos" "newfloat"
                                       "outputdir"   "section")
  "Package options for the minted package.")

;;; minted.el ends here
