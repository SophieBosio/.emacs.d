;;; fontspec.el --- AUCTeX style for `fontspec.sty' version 2.7i.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013--2022 Free Software Foundation, Inc.

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

;; This file adds support for `fontspec.sty' version 2.7i.  Starting
;; with `fontspec.sty' v2.4, the order of mandatory font names and
;; optional font features in related macros has changed, i.e. optional
;; argument comes after the mandatory one.  This change is now (April
;; 2017) implemented in this file.  Fontification support retains
;; backward compatibility.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(declare-function LaTeX-color-definecolor-list "color" ())
(declare-function LaTeX-xcolor-definecolor-list "xcolor" ())

(defvar LaTeX-fontspec-font-features
  '(;; I General font selection
    ("Extension" (".otf" ".ttf" ".ttc" ".dfont"))
    ("Path")
    ;; I 4.1 More control over font shape selection
    ("UprightFont")
    ("BoldFont")
    ("ItalicFont")
    ("BoldItalicFont")
    ("SlantedFont")
    ("BoldSlantedFont")
    ("SmallCapsFont")
    ;; I 4.2 Specifically choosing the NFSS family
    ("NFSSFamily")
    ;; I 4.3 Choosing additional NFSS font faces
    ("FontFace")
    ;; III 3. Different features for different font shapes
    ("BoldFeatures")
    ("ItalicFeatures")
    ("BoldItalicFeatures")
    ("SlantedFeatures")
    ("BoldSlantedFeatures")
    ("SmallCapsFeatures")
    ("UprightFeatures")
    ;; III 4. Different features for different font sizes
    ("SizeFeatures")
    ;; III 6. Font independent options
    ("Color")
    ("Scale" ("MatchLowercase" "MatchUppercase"))
    ("WordSpace")
    ("PunctuationSpace")
    ("HyphenChar" ("None"))
    ("OpticalSize")
    ("AutoFakeBold")
    ("AutoFakeSlant")
    ("FakeSlant")
    ("FakeStretch")
    ("FakeBold")
    ("LetterSpace")
    ;; IV.3 OpenType options
    ;; IV.3.1.1 Alternates.  The next 2 are synonyms:
    ("Alternate" ("Random"))
    ("StylisticAlternate" ("Random"))
    ;; IV.3.1.2 Character Variants
    ("CharacterVariant")
    ;; IV 3.1.3 Contextuals
    ("Contextuals" ("Swash"       "SwashOff"       "SwashReset"
                    "Alternate"   "AlternateOff"   "AlternateReset"
                    "WordInitial" "WordInitialOff" "WordInitialReset"
                    "WordFinal"   "WordFinalOff"   "WordFinalReset"
                    "LineFinal"   "LineFinalOff"   "LineFinal"
                    "Inner"       "InnerOff"       "InnerReset"
                    "ResetAll"))
    ;; IV 3.1.4 Diacritics
    ("Diacritics" ("MarkToBase" "MarkToBaseOff" "MarkToBaseReset"
                   "MarkToMark" "MarkToMarkOff" "MarkToMarkReset"
                   "AboveBase"  "AboveBaseOff"  "AboveBaseReset"
                   "BelowBase"  "BelowBaseOff"  "BelowBaseReset"
                   "ResetAll"))
    ;; IV 3.1.5 Fractions
    ("Fractions" ("On" "Off" "Reset" "Alternate"
                  "AlternateOff" "AlternateReset" "ResetAll"))
    ;; IV 3.1.6 Kerning
    ("Kerning" ("On" "Off" "Reset" "ResetAll"
                "Uppercase" "UppercaseOff" "UppercaseReset"))
    ;; IV 3.1.7 Letters
    ("Letters" ("SmallCaps"           "SmallCapsOff"           "SmallCapsReset"
                "PetiteCaps"          "PetiteCapsOff"          "PetiteCapsReset"
                "UppercaseSmallCaps"  "UppercaseSmallCapsOff"  "UppercaseSmallCapsReset"
                "UppercasePetiteCaps" "UppercasePetiteCapsOff" "UppercasePetiteCapsReset"
                "Unicase"             "UnicaseOff"             "UnicaseReset"
                "ResetAll"))
    ;; IV 3.1.8 Ligatures
    ("Ligatures" ("Required"      "RequiredOff"      "RequiredReset"
                  "Common"        "CommonOff"        "CommonReset"
                  "Contextual"    "ContextualOff"    "ContextualReset"
                  "Rare"          "RareOff"          "RareReset"
                  "Discretionary" "DiscretionaryOff" "DiscretionaryReset"
                  "Historic"      "HistoricOff"      "HistoricReset"
                  "TeX"           "TeXOff"           "TeXReset"
                  "ResetAll"))
    ;; IV 3.1.9 Localised Forms
    ("LocalForms" ("On" "Off" "Reset"))
    ;; IV 3.1.10 Numbers
    ("Numbers" ("Uppercase"    "UppercaseOff"    "UppercaseReset"
                "Lowercase"    "LowercaseOff"    "LowercaseReset"
                "Lining"       "LiningOff"       "LiningReset"
                "OldStyle"     "OldStyleOff"     "OldStyleReset"
                "Proportional" "ProportionalOff" "ProportionalReset"
                "Monospaced"   "MonospacedOff"   "MonospacedReset"
                "SlashedZero"  "SlashedZeroOff"  "SlashedZeroReset"
                "Arabic"       "ArabicOff"       "ArabicReset"
                "ResetAll"))
    ;; IV 3.1.11 Ornament
    ("Ornament")
    ;; IV 3.1.12 Style
    ("Style" ("Alternate"      "AlternateOff"      "AlternateReset"
              "Italic"         "ItalicOff"         "ItalicReset"
              "Ruby"           "RubyOff"           "RubyReset"
              "Swash"          "SwashOff"          "SwashReset"
              "Cursive"        "CursiveOff"        "CursiveReset"
              "Historic"       "HistoricOff"       "HistoricReset"
              "Titling"        "TitlingOff"        "TitlingReset"
              "HorizontalKana" "HorizontalKanaOff" "HorizontalKanaReset"
              "VerticalKana"   "VerticalKanaOff"   "VerticalKanaReset"
              "Uppercase"      "UppercaseOff"      "UppercaseReset"
              "ResetAll"))
    ;; IV 3.1.13 Stylistic Set variations.   The next 2 are synonyms:
    ("StylisticSet")
    ("Variant")
    ;; IV 3.1.14 Vertical Position
    ("VerticalPosition" ("Superior"           "SuperiorOff"           "SuperiorReset"
                         "Inferior"           "InferiorOff"           "InferiorReset"
                         "Numerator"          "NumeratorOff"          "NumeratorReset"
                         "Denominator"        "DenominatorOff"        "DenominatorReset"
                         "ScientificInferior" "ScientificInferiorOff" "ScientificInferiorReset"
                         "Ordinal"            "OrdinalOff"            "OrdinalReset"
                         "ResetAll"))
    ;; IV 3.2.1 Annotation
    ("Annotation")
    ;; IV 3.2.2 Character width
    ("CharacterWidth" ("Proportional"          "ProportionalOff"          "ProportionalReset"
                       "Full"                  "FullOff"                  "FullReset"
                       "Half"                  "HalfOff"                  "HalfReset"
                       "Third"                 "ThirdOff"                 "ThirdReset"
                       "Quarter"               "QuarterOff"               "QuarterReset"
                       "AlternateProportional" "AlternateProportionalOff" "AlternateProportionalReset"
                       "AlternateHalf"         "AlternateHalfOff"         "AlternateHalfReset"
                       "ResetAll"))
    ;; IV 3.2.3 CJK shape
    ("CJKShape" ("Traditional"
                 "Simplified"
                 "JIS1978"
                 "JIS1983"
                 "JIS1990"
                 "Expert"
                 "NLC"))
    ;; IV 3.2.4 Vertical typesetting
    ("Vertical" ("RotatedGlyphs"         "RotatedGlyphsOff"         "RotatedGlyphsReset"
                 "AlternatesForRotation" "AlternatesForRotationOff" "AlternatesForRotationReset"
                 "Alternates"            "AlternatesOff"            "AlternatesReset"
                 "KanaAlternates"        "KanaAlternatesOff"        "KanaAlternatesReset"
                 "Kerning"               "KerningOff"               "KerningReset"
                 "AlternateMetrics"      "AlternateMetricsOff"      "AlternateMetricsReset"
                 "HalfMetrics"           "HalfMetricsOff"           "HalfMetricsReset"
                 "ProportionalMetrics"   "ProportionalMetricsOff"   "ProportionalMetricsReset"
                 "ResetAll"))
    ;; VIII 3. Going behind fontspec's back: Offer only an excerpt of
    ;; all possible tags:
    ("RawFeature" ("frac" "lnum" "onum" "pnum" "smcp" "tnum" "zero")))
  "Font features options for macros of the fontspec package.")

(defvar LaTeX-fontspec-font-list nil
  "List of the fonts accessible to fontspec.")

(defun LaTeX-fontspec-arg-font (optional &optional prompt)
  "Prompt for a font name with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.

Customize `LaTeX-fontspec-arg-font-search' in order to decide how
to retrieve the list of fonts."
  (unless LaTeX-fontspec-font-list
    (when (if (eq LaTeX-fontspec-arg-font-search 'ask)
              (not (y-or-n-p "Find font yourself? "))
            LaTeX-fontspec-arg-font-search)
      (message "Searching for fonts...")
      (with-temp-buffer
        (shell-command "luaotfload-tool --list=basename" t)
        ;; Search for the font base names and full names, and add them to
        ;; `LaTeX-fontspec-font-list'.  The list is in the form
        ;;     <base name><TAB><full name><TAB><version>
        (while
            (re-search-forward "^\\([^\n\r\t]*\\)\t\\([^\n\r\t]*\\)\t.*$" nil t)
          (add-to-list 'LaTeX-fontspec-font-list (match-string-no-properties 1))
          (add-to-list 'LaTeX-fontspec-font-list
                       (match-string-no-properties 2))))
      (message "Searching for fonts...done")))
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Font name")
    (or LaTeX-fontspec-font-list LaTeX-fontspec-font-list-default))
   optional))

(defun LaTeX-fontspec-font-features ()
  "Return an updated list of font features.
This function retrieves values from various sources and adds them
to appropriate keys which are eventually prepended to
`LaTeX-fontspec-font-features' shadowing the predefined one."
  (append
   ;; Check for color packages, prefer xcolor over color.  Note that
   ;; we run the function `TeX-style-list' only once and after that we
   ;; use the updated `TeX-active-styles'.
   (when (or (member "xcolor" (TeX-style-list))
             (member "color" TeX-active-styles))
     (let ((colorcmd (if (member "xcolor" TeX-active-styles)
                         #'LaTeX-xcolor-definecolor-list
                       #'LaTeX-color-definecolor-list)))
       `(("Color" ,(mapcar #'car (funcall colorcmd))))))
   ;; If `LaTeX-fontspec-font-list' is set, use it for other `*Font'
   ;; related keys:
   (when LaTeX-fontspec-font-list
     `(("UprightFont"     ,LaTeX-fontspec-font-list)
       ("BoldFont"        ,LaTeX-fontspec-font-list)
       ("ItalicFont"      ,LaTeX-fontspec-font-list)
       ("BoldItalicFont"  ,LaTeX-fontspec-font-list)
       ("SlantedFont"     ,LaTeX-fontspec-font-list)
       ("BoldSlantedFont" ,LaTeX-fontspec-font-list)
       ("SmallCapsFont"   ,LaTeX-fontspec-font-list)))
   ;; This is a LuaTeX only feature.  VI. 1 Different font
   ;; technologies and shapers
   (when (eq TeX-engine 'luatex)
     '(("Renderer" ("Harfbuzz" "OpenType" "AAT" "Graphite"))))
   ;; This is a XeTeX only feature.  VII. 1 Different font
   ;; technologies
   (when (eq TeX-engine 'xetex)
     '(("Renderer" ("OpenType" "AAT" "Graphite"))))
   ;; Predefined features:
   LaTeX-fontspec-font-features))

;; Setup for \newfontfamily and \newfontface:
(TeX-auto-add-type "fontspec-newfontcmd" "LaTeX")

(defvar LaTeX-fontspec-newfontcmd-regexp
  '("\\\\newfontfa\\(?:ce\\|mily\\)[ \t\n\r%]*\\\\\\([a-zA-Z]+\\)"
    1 LaTeX-auto-fontspec-newfontcmd)
  "Matches new macros defined with \\newfontface and \\newfontfamily.")

(defun LaTeX-fontspec-auto-prepare ()
  "Clear `LaTeX-auto-fontspec-newfontcmd' before parsing."
  (setq LaTeX-auto-fontspec-newfontcmd nil))

(defun LaTeX-fontspec-auto-cleanup ()
  "Process parsed elements for fontspec package."
  (dolist (mac (mapcar #'car (LaTeX-fontspec-newfontcmd-list)))
    ;; Add macro to list of known macros
    (TeX-add-symbols mac)
    ;; Cater for fontification
    (when (and (featurep 'font-latex)
               (eq TeX-install-font-lock 'font-latex-setup))
      (font-latex-add-keywords `((,mac ""))
                               'type-declaration))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-fontspec-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-fontspec-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "fontspec"
 (lambda ()

   (TeX-check-engine-add-engines 'luatex 'xetex)
   (TeX-run-style-hooks "expl3" "xparse")

   ;; Add fontspec to the parser.
   (TeX-auto-add-regexp LaTeX-fontspec-newfontcmd-regexp)

   (TeX-add-symbols
    ;; 4.3 Commands for old-style and lining numbers: \oldstylenums is
    ;; already provided by LaTeX, so just add \liningnums here
    '("liningnums" "Numbers")

    ;; 4.5 Emphasis and nested emphasis
    ;; \emshape seems to be an internal macro
    "emshape"
    '("emfontdeclare" t)
    "emreset"

    ;; 4.6 Strong emphasis
    '("strong" t)
    '("strongfontdeclare" t)
    "strongreset"

    ;; 5 Font selection
    '("fontspec"
      LaTeX-fontspec-arg-font
      [TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"])

    ;; Default font families
    '("setmainfont"
      (LaTeX-fontspec-arg-font "Main font name")
      [TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"])
    '("setsansfont"
      (LaTeX-fontspec-arg-font "Sans font name")
      [TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"])
    '("setmonofont"
      (LaTeX-fontspec-arg-font "Mono font name")
      [TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"])

    ;; 5.3 Querying whether a font exists
    '("IfFontExistsTF" LaTeX-fontspec-arg-font 2)

    ;; 6 commands to select font families
    '("newfontfamily" TeX-arg-define-macro
      LaTeX-fontspec-arg-font
      [TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"])

    '("newfontface" TeX-arg-define-macro
      LaTeX-fontspec-arg-font
      [TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"])

    ;; 6.4 Math(s) fonts
    '("setmathrm" "Font name" [ "Font features" ])
    '("setmathsf" "Font name" [ "Font features" ])
    '("setmathtt" "Font name" [ "Font features" ])
    '("setboldmathrm" "Font name" [ "Font features" ])

    ;; 8 Default settings
    '("defaultfontfeatures" [ LaTeX-fontspec-arg-font ]
      (TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"))
    '("defaultfontfeatures+" [ LaTeX-fontspec-arg-font ]
      (TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"))

    ;; 10 Working with the currently selected features
    '("IfFontFeatureActiveTF"
      [TeX-arg-key-val (LaTeX-fontspec-font-features) "Font feature"] 2)

    ;; Changing the currently selected features
    '("addfontfeatures"
      (TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"))

    ;; 23 Defining new features
    '("newAATfeature"
      (TeX-arg-completing-read (LaTeX-fontspec-font-features)
                               "Existing feature")
      "New option" 2)

    '("newopentypefeature"
      (TeX-arg-completing-read (LaTeX-fontspec-font-features)
                               "Existing feature")
      "New option" t)

    '("newfontfeature" "New feature" t)

    ;; 24 Defining new scripts and languages
    '("newfontscript" "Script name" "OpenType tag")
    '("newfontlanguage" "Language name" "OpenType tag")

    ;; 26 Renaming existing features & options
    '("aliasfontfeature"
      (TeX-arg-completing-read (LaTeX-fontspec-font-features)
                               "Existing feature")
      "New name")

    `("aliasfontfeatureoption"
      (TeX-arg-completing-read (LaTeX-fontspec-font-features) "Feature")
      (TeX-arg-completing-read
       ,(lambda ()
          (save-excursion
            (re-search-backward "\\\\aliasfontfeatureoption{\\([^}]+\\)}"
                                (line-beginning-position) t))
          (cadr (assoc (match-string-no-properties 1)
                       (LaTeX-fontspec-font-features)))))
      "New Name") )

   (LaTeX-add-environments
    ;; 4.6 Strong emphasis
    '("strong"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fontspec"    "[{[")
                                ("setmainfont" "[{[")
                                ("setsansfont" "[{[")
                                ("setmonofont" "[{[")
                                ("newfontfamily" "\\[{[")
                                ("newfontface"   "\\[{[")
                                ("setmathrm" "[{[")
                                ("setmathsf" "[{[")
                                ("setmathtt" "[{[")
                                ("setboldmathrm" "[{[")
                                ("defaultfontfeatures" "+[{")
                                ("addfontfeature"  "{")
                                ("addfontfeatures" "{")
                                ("newfontscript"   "{{")
                                ("newfontlanguage" "{{")
                                ("emfontdeclare"   "{")
                                ("strongfontdeclare"  "{")
                                ("newAATfeature"      "{{{{")
                                ("newopentypefeature" "{{{")
                                ("newfontfeature"     "{{")
                                ("aliasfontfeature"   "{{")
                                ("aliasfontfeatureoption" "{{{"))
                              'function)
     (font-latex-add-keywords '(("liningnums"    "{"))
                              'type-command)
     (font-latex-add-keywords '(("strong"    "{"))
                              'bold-command)))
 TeX-dialect)

(defvar LaTeX-fontspec-package-options
  '("tuenc" "euenc" "math" "no-math" "config" "no-config" "quiet" "silent")
  "Package options for the fontspec package.")

;;; fontspec.el ends here
