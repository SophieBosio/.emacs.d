;;; siunitx.el --- AUCTeX style for `siunitx.sty' version 3.3.36.  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2022  Free Software Foundation, Inc.

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

;; This file adds support for `siunitx.sty' version 3.0.36 from
;; 2021/22/18.

;;; Code:

(require 'tex) ;Indispensable when compiling the call to `TeX-auto-add-type'.
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(declare-function LaTeX-color-definecolor-list "color" ())
(declare-function LaTeX-xcolor-definecolor-list "xcolor" ())

(TeX-auto-add-type "siunitx-unit" "LaTeX")

;; Self Parsing -- see (info "(auctex)Hacking the Parser").
;; `\\(?:\\[[^]]*\\]\\)?' matches possible options (actually used only
;; by `DeclareSIUnit' macro), wrapped in `[...]'.
(defvar LaTeX-siunitx-regexp
  `(,(concat "\\\\DeclareSI\\(Unit\\|Prefix\\|Power\\|Qualifier\\)"
             "[ \t\n\r]*"
             ;; The optional argument
             "\\(?:\\[[^]]*\\]\\)?"
             "[ \t\n\r]*"
             ;; First mandatory argument
             "{?\\\\\\([A-Za-z]+\\)}?"
             "[ \t\n\r]*"
             ;; Second mandatory argument needed for '\DeclareSIPower':
             "{?\\\\\\([A-Za-z]+\\)}?")
    (2 3 1) LaTeX-auto-siunitx-unit)
  "Matches new siunitx unit, prefix, power, and qualifier definitions.")

(defun LaTeX-siunitx-prepare ()
  "Clear `LaTex-auto-siunitx-unit' before use."
  (setq LaTeX-auto-siunitx-unit nil))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-siunitx-prepare t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defvar LaTeX-siunitx-unit-history nil
  "History of units in siunitx.")

(defun LaTeX-siunitx-unit-list-parsed ()
  "Return a list of units incl. the user defined ones.
This function should be preferred over the function
`LaTeX-siunitx-unit-list' since it knows about the 2 macros
defined with '\\DeclareSIPower'."
  (let (result)
    (dolist (unit (LaTeX-siunitx-unit-list) result)
      (push (car unit) result)
      (when (and (> (safe-length unit) 1)
                 (string-equal (nth 2 unit) "Power"))
        (push (cadr unit) result)))))

(defun LaTeX-arg-siunitx-unit (optional &optional prompt initial-input
                                        definition prefix)
  "Prompt for siunitx units, prefixes, powers, and qualifiers.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If INITIAL-INPUT is non-nil, insert it in the minibuffer
initially, with point positioned at the end.  If DEFINITION is
non-nil, add the chosen unit to the list of defined units.  If
PREFIX is non-nil, insert it before the given input."
  ;; Remove <SPC> key binding from map used in `TeX-completing-read-multiple'
  ;; with `require-match' set to `nil' (it's `crm-local-completion-map' if
  ;; `completing-read-multiple' is bound, `minibuffer-local-completion-map'
  ;; otherwise) and set completion separator to the TeX escape character.
  (let* ((crm-local-completion-map
          (remove (assoc 32 crm-local-completion-map) crm-local-completion-map))
         (minibuffer-local-completion-map
          (remove (assoc 32 minibuffer-local-completion-map)
                  minibuffer-local-completion-map))
         (crm-separator (regexp-quote TeX-esc))
         (unit (mapconcat #'identity
                          (TeX-completing-read-multiple
                           (TeX-argument-prompt optional prompt "Unit: " t)
                           (LaTeX-siunitx-unit-list-parsed)
                           nil nil initial-input
                           'LaTeX-siunitx-unit-history)
                          TeX-esc)))
    (if (and definition (not (string-equal "" unit)))
        (LaTeX-add-siunitx-units unit))
    (TeX-argument-insert unit optional prefix)))

(defun LaTeX-arg-define-siunitx-unit (optional &optional prompt)
  "Prompt for a LaTeX siunitx unit, prefix, power, and qualifier.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (LaTeX-arg-siunitx-unit optional
                          (unless prompt (concat "Unit: " TeX-esc))
                          nil t TeX-esc))

(defvar LaTeX-siunitx-package-options
  '(;; Table 10: Print options
    ("color")
    ("mode"                       ("match" "math" "text"))
    ("number-color")
    ("number-mode"                ("match" "math" "text"))
    ("propagate-math-font"        ("true" "false"))
    ("reset-math-version"         ("true" "false"))
    ("reset-text-family"          ("true" "false"))
    ("reset-text-series"          ("true" "false"))
    ("reset-text-shape"           ("true" "false"))
    ("text-family-to-math"        ("true" "false"))
    ("text-font-command")
    ("text-series-to-math"        ("true" "false"))
    ("unit-color")
    ("unit-mode"                  ("match" "math" "text"))
    ;; Table 11: Options for number parsing
    ("evaluate-expression"        ("true" "false"))
    ("expression")
    ("input-close-uncertainty")
    ("input-comparators")
    ("input-decimal-markers")
    ("input-digits")
    ("input-exponent-markers")
    ("input-ignore")
    ("input-open-uncertainty")
    ("input-signs")
    ("input-uncertainty-signs")
    ("parse-numbers"              ("true" "false"))
    ("retain-explicit-plus"       ("true" "false"))
    ("retain-zero-uncertainty"    ("true" "false"))
    ;; Table 12: Number post-processing options
    ("drop-exponent"              ("true" "false"))
    ("drop-uncertainty"           ("true" "false"))
    ("drop-zero-decimal"          ("true" "false"))
    ("exponent-mode"              ("input" "fixed" "engineering" "scientific"))
    ("fixed-exponent")
    ("minimum-integer-digits")
    ("minimum-decimal-digits")
    ("round-half"                 ("up" "even"))
    ("round-minimum")
    ("round-mode"                 ("off" "figures" "places" "uncertainty"))
    ("round-pad"                  ("true" "false"))
    ("round-precision")
    ;; Table 13: Output options for numbers
    ("bracket-negative-numbers"   ("true" "false"))
    ("exponent-base")
    ("exponent-product")
    ("group-digits"               ("all" "none" "decimal" "integer"))
    ("group-minimum-digits")
    ("group-separator")
    ("negative-color")
    ("output-close-uncertainty")
    ("output-decimal-marker")
    ("output-exponent-marker")
    ("output-open-uncertainty")
    ("print-implicit-plus"        ("true" "false"))
    ("print-unity-mantissa"       ("true" "false"))
    ("print-zero-exponent"        ("true" "false"))
    ("tight-spacing"              ("true" "false"))
    ("uncertainty-mode"           ("compact" "full" "compact-marker"))
    ("uncertainty-separator")
    ;; Table 14: Output options for lists, products and ranges of
    ;; numbers and quantities
    ("list-exponents"             ("individual" "combine-bracket" "combine"))
    ("list-final-separator")
    ("list-pair-separator")
    ("list-separator")
    ("list-units"                 ("repeat" "bracket" "single"))
    ("product-exponents"          ("individual" "combine-bracket" "combine"))
    ("product-mode"               ("symbol" "phrase"))
    ("product-phrase")
    ("product-symbol")
    ("product-units"              ("repeat" "bracket" "single"))
    ("range-exponents"            ("individual" "combine-bracket" "combine"))
    ("range-phrase")
    ("range-units"                ("repeat" "bracket" "single"))
    ;; Table 15: Options for complex numbers
    ("complex-root-position"      ("after-number" "before-number"))
    ("output-complex-root")
    ("input-complex-root")
    ;; Table 16: Angle options
    ("angle-mode"                 ("input" "arc" "decimal"))
    ("angle-symbol-degree")
    ("angle-symbol-minute")
    ("angle-symbol-over-decimal"  ("true" "false"))
    ("angle-symbol-second")
    ("angle-separator")
    ("fill-angle-degrees"         ("true" "false"))
    ("fill-angle-minutes"         ("true" "false"))
    ("fill-angle-seconds"         ("true" "false"))
    ("number-angle-product")
    ;; Table 17: Unit creation options
    ("free-standing-units"        ("true" "false"))
    ("overwrite-command"          ("true" "false"))
    ("space-before-unit"          ("true" "false"))
    ("unit-optional-argument"     ("true" "false"))
    ("use-xspace"                 ("true" "false"))
    ;; Table 18: Unit output options
    ("bracket-unit-denominator"   ("true" "false"))
    ("forbid-literal-units"       ("true" "false"))
    ("fraction-command")
    ("inter-unit-product")
    ("parse-units"                ("true" "false"))
    ("per-mode"                   ("power" "fraction" "symbol"
                                   "repeated-symbol"  "symbol-or-fraction"))
    ("per-symbol")
    ("qualifier-mode"             ("subscript" "brackets"
                                   "combine"   "phrase"))
    ("qualifier-phrase")
    ("sticky-per"                 ("true" "false"))
    ("unit-font-command")
    ;; Table 19: Options for quantities
    ("allow-number-unit-breaks"   ("true" "false"))
    ("extract-mass-in-kilograms"  ("true" "false"))
    ("prefix-mode"                ("input" "combine-exponent"
                                   "extract-exponent"))
    ("quantity-product")
    ("separate-uncertainty-units")
    ;; Table 20: Options for tabular material
    ("table-align-comparator"     ("true" "false"))
    ("table-align-exponent"       ("true" "false"))
    ("table-align-text-after"     ("true" "false"))
    ("table-align-text-before"    ("true" "false"))
    ("table-align-uncertainty"    ("true" "false"))
    ("table-alignment"            ("center" "left" "right" "none"))
    ("table-alignment-mode"       ("format" "marker" "none"))
    ("table-auto-round"           ("true" "false"))
    ("table-column-width")
    ("table-fixed-width"          ("true" "false"))
    ("table-format")
    ("table-number-alignment"     ("center" "left" "right"))
    ("table-text-alignment"       ("center" "left" "right"))
    ;; 4.13 Locale options
    ("locale"                     ("FR" "DE" "UK" "US" "ZA")))
  "Package options for the siunitx package.")

(defun LaTeX-siunitx-key-val-options ()
  "Return an updated list of key=vals from siunitx package."
  (append
   (when (or (member "xcolor" (TeX-style-list))
             (member "color" TeX-active-styles))
     (let* ((colorcmd (if (member "xcolor" TeX-active-styles)
                          #'LaTeX-xcolor-definecolor-list
                        #'LaTeX-color-definecolor-list))
            (colors  (mapcar #'car (funcall colorcmd)))
            (keys '("color"
                    "number-color"
                    "unit-color"
                    "negative-color"))
            result)
       (dolist (key keys result)
         (push (list key colors) result))))
   LaTeX-siunitx-package-options))

(TeX-add-style-hook
 "siunitx"
 (lambda ()

   (TeX-auto-add-regexp LaTeX-siunitx-regexp)

   (TeX-add-symbols
    '("sisetup" (TeX-arg-key-val (LaTeX-siunitx-key-val-options)))

    ;; 3.1 Numbers
    '("num"        [TeX-arg-key-val (LaTeX-siunitx-key-val-options)] "Number")
    '("numlist"    [TeX-arg-key-val (LaTeX-siunitx-key-val-options)] "Numbers")
    '("numproduct" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)] "Numbers")
    '("numrange"   [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      "Number 1" "Number 2")

    ;; 3.2 Angles
    '("ang" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)] "Angle")

    ;; 3.3 Units
    ;; For 'qty' and 'units', see 8 Compatibility with other packages
    '("qtylist"    [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      "Numbers" LaTeX-arg-siunitx-unit)
    '("qtyproduct" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      "Numbers" LaTeX-arg-siunitx-unit)
    '("qtyrange"   [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      "Number 1" "Number 2" LaTeX-arg-siunitx-unit)

    ;; 3.4 Complex numbers and quantities
    '("complexnum" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      "Number")
    '("complexqty" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      "Number" LaTeX-arg-siunitx-unit)

    ;; 3.7 Creating new macros
    '("DeclareSIUnit" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      (LaTeX-arg-define-siunitx-unit) "Symbol")

    '("DeclareSIPrefix" (LaTeX-arg-define-siunitx-unit "Prefix")
      "Symbol" "Powers of 10")

    '("DeclareSIPower"
      (LaTeX-arg-define-siunitx-unit "Symbol before")
      (LaTeX-arg-define-siunitx-unit "Symbol after")
      "Power")

    '("DeclareSIQualifier" (LaTeX-arg-define-siunitx-unit "Qualifier") "Symbol")

    ;; 3.8 Tabular material
    '("tablenum" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)] "Number")

    ;; 5 Upgrading from version 2
    ;; The next set of macros are still available in siunitx.sty v3
    ;; but are not recommended for use in new documents.  We provide
    ;; them in this file anyway since they are also needed when other
    ;; packages like physics or units are loaded:
    '("si" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)] LaTeX-arg-siunitx-unit)
    '("SI" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      "Value" ["Pre-unit"] LaTeX-arg-siunitx-unit)
    '("SIlist" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      "Values" LaTeX-arg-siunitx-unit)
    '("SIrange" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
      "Value 1" "Value 2" LaTeX-arg-siunitx-unit))

   ;; 8 Compatibility with other packages
   ;; Avoid clash with other packages which define macros with the
   ;; same name:
   (let ((styles (TeX-style-list)))
     (unless (member "physics" styles)
       (TeX-add-symbols
        '("qty"  [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
          "Number" LaTeX-arg-siunitx-unit)))
     (unless (member "units" styles)
       (TeX-add-symbols
        '("unit" [TeX-arg-key-val (LaTeX-siunitx-key-val-options)]
          LaTeX-arg-siunitx-unit))))

   ;; The unit macros
   (LaTeX-add-siunitx-units
    ;; Table 1: SI base units.
    "ampere"
    "candela"
    "kelvin"
    "kilogram"
    "meter"
    "metre"
    "second"
    ;; Table 2: Coherent derived units in the SI with special names
    ;; and symbols
    "becquerel"
    ;; "celsius"
    "degreeCelsius"
    "coulomb"
    "farad"
    "gray"
    "hertz"
    "henry"
    "joule"
    "lumen"
    "katal"
    "lux"
    "newton"
    "ohm"
    "pascal"
    "radian"
    "siemens"
    "sievert"
    "steradian"
    "tesla"
    "volt"
    "watt"
    "weber"
    ;; Table 3: Non-SI units accepted for use with the International
    ;; System of Units
    "astronomicalunit"
    "bel"
    "dalton"
    "day"
    "decibel"
    "degree"
    "electronvolt"
    "hectare"
    "hour"
    "liter"
    "litre"
    "arcminute"
    "minute"
    "arcsecond"
    "neper"
    "tonne"
    ;; 3.5 The unit macros
    "percent"
    "square"
    "squared"
    "cubic"
    "cubed"
    "tothe"
    "raiseto"
    "per"
    "of"
    "highlight" ; Defined by siunitx.sty
    ;; Table 4: SI prefixes
    "yocto"
    "zepto"
    "atto"
    "femto"
    "pico"
    "nano"
    "micro"
    "milli"
    "centi"
    "deci"
    "deca"
    "deka"
    "hecto"
    "kilo"
    "mega"
    "giga"
    "tera"
    "peta"
    "exa"
    "zetta"
    "yotta")

   ;; 3.6 Unit abbreviations are always defined:
   (LaTeX-add-siunitx-units
    "fg"    "pg"    "ng"    "ug"    "mg"    "g"    "kg"
    "pm"    "nm"    "um"    "mm"    "cm"    "dm"   "m"  "km"
    "as"    "fs"    "ps"    "ns"    "us"    "ms"   "s"
    "fmol"  "pmol"  "nmol"  "umol"  "mmol"  "mol"  "kmol"
    "pA"    "nA"    "uA"    "mA"    "A"     "kA"
    "ul"    "ml"    "l"     "hl"    "uL"    "mL"   "L"  "hL"
    "mHz"   "Hz"    "kHz"   "MHz"   "GHz"   "THz"
    "mN"    "N"     "kN"    "MN"
    "Pa"    "kPa"   "MPa"   "GPa"
    "mohm"  "kohm"  "Mohm"
    "pV"    "nV"    "uV"    "mV"    "V"     "kV"
    "uW"    "mW"    "W"     "kW"    "MW"    "GW"
    "J"     "uJ"    "mJ"    "kJ"
    "eV"    "meV"   "keV"   "MeV"   "GeV"   "TeV"  "kWh"
    "F"     "fF"    "pF"    "nF"    "uF"
    "H"     "mH"    "uH"
    "K"     "dB"
    "kibi"  "mebi"  "gibi"  "tebi"  "pebi"
    "exbi"  "zebi"  "yobi"  "bit"   "byte")

   ;; \cancel is only available when cancel.sty is loaded:
   (when (member "cancel" (TeX-style-list))
     (LaTeX-add-siunitx-units "cancel"))

   ;; FIXME: 'siunitx.sty' adds only one new column specification
   ;; letter 'S' in v3 and 's' is removed.  We keep 's' for older
   ;; documents and remove it sometimes later.
   (set (make-local-variable 'LaTeX-array-column-letters)
        (concat LaTeX-array-column-letters "S" "s"))

   (TeX-run-style-hooks "l3keys2e"
                        "array"
                        "amstext"
                        "xparse"
                        "expl3")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("num"                 "[{")
                                ("numlist"             "[{")
                                ("numproduct"          "[{")
                                ("numrange"            "[{{")
                                ("ang"                 "[{")
                                ("qtylist"             "[{{")
                                ("qtyrange"            "[{{{")
                                ("complexnum"          "[{")
                                ("complexqty"          "[{")
                                ("DeclareSIUnit"       "[|{\\{")
                                ("DeclareSIPrefix"     "|{\\{{")
                                ("DeclareSIPower"      "|{\\|{\\{")
                                ("DeclareSIQualifier"  "|{\\{")
                                ("tablenum"            "[{")
                                ("highlight"           "{")
                                ("sisetup"             "{")
                                ;; These macros are deprecated in v3 but
                                ;; still available:
                                ("si"                  "[{")
                                ("SI"                  "[{[{")
                                ("SIlist"              "[{{")
                                ("SIrange"             "[{{{"))
                              'function)
     (let ((styles (TeX-style-list)))
       (unless (member "physics" styles)
         (font-latex-add-keywords '(("qty"  "[{{"))
                                  'function))
       (unless (member "units" styles)
         (font-latex-add-keywords '(("unit" "[{"))
                                  'function)))))
 TeX-dialect)

(defun LaTeX-siunitx-package-options nil
  "Prompt for package options for the siunitx package."
  (TeX-read-key-val t (append
                       ;; 'table-column-type' is a preamble only:
                       '(("table-column-type"))
                       LaTeX-siunitx-package-options)))

;; siunitx.el ends here
