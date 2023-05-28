;;; physics.el --- AUCTeX style for `physics' (v1.3).  -*- lexical-binding: t; -*-

;; Copyright (C) 2022--2023 Free Software Foundation, Inc.

;; Author: Ikumi Keita <ikumikeita@jcom.home.ne.jp>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-12-20
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

;; This file adds support for `physics.sty' (v1.3) from 2012/12/12
;; `physics.sty' is part of TeX Live.

;;; Code:

(require 'tex)
(require 'latex)

;; List of supported braces by macros in physics package
;;     | () | [] | || | {} |null| big
;; ----+----+----+----+----+--- +----
;; qty | +  | +  | +  | +  | !  | +
;; abs | !  | !  | !  | @  | !  | +
;; eval| *  | *  | !  | -  | !  | !
;; grad| +  | +  |    | @  | =  | !
;; sin | +  | ^  |    | @  | =  | !
;; exp | +  | +  |    | +  |    | !
;; tr  | +  | +  |    | +  |    | !
;; erf |    |    |    | @  | =  | !
;; Res | +  | +  |    | +  |    | !
;; pv  |    |    |    | @  | =  | !
;; Re  |    |    |    | +  |    | !
;; dd  | +  | ^  |    | @  |    | !
;; bra |    |    |    | +  | =  | !
;; mqty| +  | +  | +  | @  | !  | !
;;mqty*| +  |    |    | @  | !  | !
;;
;; + ... supported
;; ! ... error or wrong result
;; @ ... works, but missing "{}" in the output
;; * ... supported, but the right brace must be "|"
;; - ... works, with only right "|" in the output
;; = ... same as {}
;; ^ ... interpreted as power optional argument
;;
;; Special note
;;  + \sin[2]x and \sin[2]{x} aren't supported.
;;  + "\dd{x}" and "\dd x" give different output.
;;  + mqty* is only meaningful when followed by "(...)".
;;  + as opposed to the document, \tr and \trace aren't equivalent.
;;  + dv accepts one or two arguments except optional argument.
;;    o 1 arg      ... {derivative variable}
;;    o 2 args     ... {function to derive}{derivative variable}
;;    o 1 arg + () ... {derivative variable}(function to derive)
;;  + pdv accepts one, two or three arguments except optional argument.
;;    o 1 arg      ... {derivative variable}
;;    o 2 args     ... {function to derive}{derivative variable}
;;    o 3 args     ... {func. to der.}{der. var.1}{der. var.2}
;;    o 1 arg + () ... {derivative variable}(function to derive)
;;    - 2 args + ()... NA
;;    - 3 args + ()... NA

(defun TeX-arg-physics-big (_optional)
  "Prompt for various \\big specification and insert it without braces.
\\Big, \\bigg and \\Bigg are also allowed."
  (TeX-arg-completing-read t '("big" "Big" "bigg" "Bigg")
                           "bigness specification: " t TeX-esc "" "" nil t))

(defun TeX-physics--arg-any-braces (paren bracket vert brace
                                          &optional default close)
  "Template to query and insert various braces.
Boolean arguments PAREN, BRACKET, VERT and BRACE mean to support (...),
[...], |...| and {...}, respectively.
Optional argument DEFAULT specifies default open brace.
Non-nil CLOSE specifies close brace unconditionally."
  (let* ((candidates
          (let (lst)
            (if paren   (push '("(" . ")") lst))
            (if bracket (push (cons LaTeX-optop LaTeX-optcl) lst))
            (if vert    (push '("|" . "|") lst))
            (if brace   (push (cons TeX-grop TeX-grcl) lst))
            lst))
         (open (completing-read
                (concat "Which brace?"
                        (cond
                         (default
                           (format " (default \"%s\")" default))
                         (brace
                          " (RET to omit)")
                         (t
                          ""))
                        ": ")
                candidates nil t nil nil default)))
    (or close
        (setq close (cdr (assoc open candidates))))
    ;; When `default' is nil and the user gave empty answer,
    ;;  - If `brace' is nil, use "{}".
    ;;  - Otherwise do nothing.
    (if (and (= 0 (length open))
             (not brace))
        (setq open TeX-grop
              close TeX-grcl))
    (when (< 0 (length open))
      (setq TeX-arg-opening-brace open
            TeX-arg-closing-brace close)
      (let ((content
             (if (TeX-active-mark)
                 (prog1
                     (buffer-substring-no-properties (point) (mark))
                   (delete-region (point) (mark)))
               "")))
        (TeX-argument-insert content nil)))))

(defun TeX-arg-physics-qty (_optional)
  "Query and insert (), [], || or {}.
Default ()."
  (TeX-physics--arg-any-braces t t t t "("))

(defun TeX-arg-physics-eval (_optional)
  "Query and insert (| or [|.
When the user declined, supplement {} instead."
  (TeX-physics--arg-any-braces t t nil nil nil "|"))

(defun TeX-arg-physics-grad (_optional)
  "Query and insert () or [].
When the user declined, supplement {} instead."
  (TeX-physics--arg-any-braces t t nil nil))

(defun TeX-arg-physics-exp (_optional)
  "Query and insert (), [] or {}."
  (TeX-physics--arg-any-braces t t nil t))

(defun TeX-arg-physics-dd (_optional)
  "Query and insert () or {}."
  (TeX-physics--arg-any-braces t nil nil t))

(defun TeX-arg-physics-mqty (_optional)
  "Query and insert (), [] or ||.
When the user declined, supplement {} instead."
  (TeX-physics--arg-any-braces t t t nil))

(defun TeX-physics--arg-single-brace (open close &optional force)
  "Template to query and insert single flavor of braces.
If optional argument FORCE is non-nil, don't ask and always insert."
  (when (or force
            (y-or-n-p (format "Use \"%s%s\"? " open close)))
    (setq TeX-arg-opening-brace open
          TeX-arg-closing-brace close)
    (let ((content (if (TeX-active-mark)
                       (prog1
                           (buffer-substring-no-properties (point) (mark))
                         (delete-region (point) (mark)))
                     "")))
      (TeX-argument-insert content nil))))

(defun TeX-arg-physics-trig (_optional)
  "Query and insert ().
First query optional argument for power.  If that is non-empty,
use () unconditionally."
  ;; XXX: Should we respect `TeX-insert-macro-default-style'?
  (let ((power (TeX-read-string "(Optional) Power: ")))
    (let ((TeX-arg-opening-brace LaTeX-optop)
          (TeX-arg-closing-brace LaTeX-optcl))
      (TeX-argument-insert power t))
    (TeX-physics--arg-single-brace "(" ")" (< 0 (length power)))))

(defun TeX-arg-physics-ReIm (_optional)
  "Query and insert {}."
  (TeX-physics--arg-single-brace TeX-grop TeX-grcl))

(defun TeX-arg-physics-mqty* (_optional)
  "Insert ()."
  (TeX-physics--arg-single-brace "(" ")" t))

(defun TeX-physics--arg-1-or-2-arg (prompt1 prompt2)
  "Template to query and insert one or two arguments."
  (let* ((arg1 (TeX-read-string (concat prompt1 ": ")))
         (arg2 (TeX-read-string (concat prompt2 " (RET to omit): "))))
    (TeX-argument-insert arg1 nil)
    (if (< 0 (length arg2))
        (TeX-argument-insert arg2 nil))))

(defun TeX-arg-physics-braket (_optional)
  (TeX-physics--arg-1-or-2-arg "Bra content" "Ket content"))

(defun TeX-arg-physics-ketbra (_optional)
  (TeX-physics--arg-1-or-2-arg "Ket content" "Bra content"))

(defun TeX-arg-physics-expval (_optional)
  (TeX-physics--arg-1-or-2-arg "Observable" "State"))

(defun TeX-arg-physics-derivative (_optional &optional partial)
  "Query and insert one or two arguments for derivative.
The user can choose whether to wrap the function to derive with ().
In addition, query and insert optional power argument.

If optional argument PARTIAL is non-nil, three arguments are also
supported for partial derivative.
When three arguments are specified, don't ask optional power argument."
  (let* ((func (TeX-read-string
                "\
Function to derive (RET to omit, SPC RET to have empty placeholder): "))
         (paren (and (< 0 (length func))
                     (y-or-n-p "Use \"()\" to wrap the function? ")))
         (var1 (TeX-read-string "Derivative variable: "))
         (var2 (and partial (not paren) (< 0 (length func))
                    (TeX-read-string
                     "Derivative variable 2nd (RET to omit): "))))
    (unless var2
      ;; XXX: Should we respect `TeX-insert-macro-default-style'?
      (TeX-arg-string t "Power" nil nil nil LaTeX-optop LaTeX-optcl))
    (when (and (< 0 (length func))
               (not paren))
      (if (equal func " ")
          (setq func ""))
      (TeX-argument-insert func nil))
    (TeX-argument-insert var1 nil)
    (if (< 0 (length var2))
        (TeX-argument-insert var2 nil))
    (when paren
        (setq TeX-arg-opening-brace "("
              TeX-arg-closing-brace ")")
        (if (equal func " ")
            (setq func ""))
        (TeX-argument-insert func nil))))

(TeX-add-style-hook
 "physics"
 (lambda ()
   ;; physics requires amsmath and xparse.
   (TeX-run-style-hooks "amsmath" "xparse")

   (TeX-add-symbols
    ;; 2.1 Automatic bracing
    '("quantity" [TeX-arg-physics-big] TeX-arg-physics-qty)
    '("qty" [TeX-arg-physics-big] TeX-arg-physics-qty)
    '("pqty" [TeX-arg-physics-big] t) '("bqty" [TeX-arg-physics-big] t)
    '("vqtry" [TeX-arg-physics-big] t) '("Bqty" [TeX-arg-physics-big] t)
    '("absolutevalue" [TeX-arg-physics-big] t)
    '("absolutevalue*" t)
    '("abs" [TeX-arg-physics-big] t) '("abs*" t)
    '("norm" [TeX-arg-physics-big] t)
    '("norm*" t)
    '("evaluated" TeX-arg-physics-eval)
    '("evaluated*" TeX-arg-physics-eval)
    '("eval" TeX-arg-physics-eval) '("eval*" TeX-arg-physics-eval)
    '("order" [TeX-arg-physics-big] t)
    '("order*" t)
    '("commutator" [TeX-arg-physics-big] 2)
    '("commutator*" 2)
    '("comm" [TeX-arg-physics-big] 2) '("comm*" 2)
    '("anticommutator" [TeX-arg-physics-big] 2)
    '("anticommutator*" 2)
    '("acomm" [TeX-arg-physics-big] 2) '("acomm*" 2)
    '("poissonbracket" [TeX-arg-physics-big] 2)
    '("poissonbracket*" 2)
    '("pb" [TeX-arg-physics-big] 2) '("pb*" 2)
    ;; 2.2 Vector notation
    '("vectorbold" t) '("vectorbold*" t)
    '("vb" t) '("vb*" t)
    '("vectorarrow" t) '("vectorarrow*" t)
    '("va" t) '("va*" t)
    '("vectorunit" t) '("vectorunit*" t)
    '("vu" t) '("vu*" t)
    "dotproduct" "vdot"
    "crossproduct" "cross"
    "cp"
    '("gradient" TeX-arg-physics-grad)
    '("grad" TeX-arg-physics-grad)
    '("divergence" TeX-arg-physics-grad)
    '("div" TeX-arg-physics-grad)
    "divisionsymbol"
    '("curl" TeX-arg-physics-grad)
    '("laplacian" TeX-arg-physics-grad)
   ;; 2.3 Operators
    '("sin" TeX-arg-physics-trig)
    '("sinh" TeX-arg-physics-trig)
    '("arcsin" TeX-arg-physics-trig)
    '("asin" TeX-arg-physics-trig)
    '("cos" TeX-arg-physics-trig)
    '("cosh" TeX-arg-physics-trig)
    '("arccos" TeX-arg-physics-trig)
    '("acos" TeX-arg-physics-trig)
    '("tan" TeX-arg-physics-trig)
    '("tanh" TeX-arg-physics-trig)
    '("arctan" TeX-arg-physics-trig)
    '("atan" TeX-arg-physics-trig)
    '("csc" TeX-arg-physics-trig)
    '("csch" TeX-arg-physics-trig)
    '("arccsc" TeX-arg-physics-trig)
    '("acsc" TeX-arg-physics-trig)
    "sine"      "hypsine"      "arcsine"      "asine"
    "cosine"    "hypcosine"    "arccosine"    "acosine"
    "tangent"   "hyptangent"   "arctangent"   "atangent"
    "cosecant"  "hypcosecant"  "arccosecant"  "acosecant"
    "secant"    "hypsecant"    "arcsecant"    "asecant"
    "cotangent" "hypcotangent" "arccotangent" "acotangent"
    '("exp" TeX-arg-physics-exp)
    '("log" TeX-arg-physics-exp)
    '("ln" TeX-arg-physics-exp)
    '("det" TeX-arg-physics-exp)
    '("Pr" TeX-arg-physics-exp)
    "exponential" "logarithm" "naturallogarithm" "determinant" "Probability"
    '("tr" TeX-arg-physics-exp)
    '("Tr" TeX-arg-physics-exp)
    "rank" "erf"
    '("Res" TeX-arg-physics-exp)
    '("principalvalue" t)
    '("pv" t) '("PV" t)
    '("Re" TeX-arg-physics-ReIm)
    '("Im" TeX-arg-physics-ReIm)
    "real" "imaginary"
    ;; 2.4 Quick quad text
    '("qqtext" t)
    '("qqtext*" t)
    '("qq" t) '("qq*" t)
    "qcomma" "qcomma*" "qc" "qc*" "qcc" "qcc*"
    "qif" "qif*" "qthen" "qthen*" "qelse" "qelse*" "qotherwise" "qotherwise*"
    "qunless" "qunless*" "qgiven" "qgiven*" "qusing" "qusing*"
    "qassume" "qassume*" "qsince" "qsince*" "qlet" "qlet*" "qfor" "qfor*"
    "qall" "qall*" "qeven" "qeven*" "qodd" "qodd*" "qinteger" "qinteger*"
    "qand" "qand*" "qor" "qor*" "qas" "qas*" "qin" "qin*"
    ;; 2.5 Derivatives
    '("differential" ["Power"] TeX-arg-physics-dd)
    '("dd" ["Power"] TeX-arg-physics-dd)
    '("derivative" TeX-arg-physics-derivative)
    '("derivative*" TeX-arg-physics-derivative)
    '("dv" TeX-arg-physics-derivative)
    '("dv*" TeX-arg-physics-derivative)
    '("partialderivative" (TeX-arg-physics-derivative t))
    '("partialderivative*" (TeX-arg-physics-derivative t))
    '("pderivative" (TeX-arg-physics-derivative t))
    '("pderivative*" (TeX-arg-physics-derivative t))
    '("pdv" (TeX-arg-physics-derivative t))
    '("pdv*" (TeX-arg-physics-derivative t))
    '("variation" ["Power"] TeX-arg-physics-dd)
    '("var" ["Power"] TeX-arg-physics-dd)
    '("functionalderivative" TeX-arg-physics-derivative)
    '("fdv" TeX-arg-physics-derivative)
    ;; 2.6 Dirac bra-ket notation
    '("bra" t)
    '("bra*" t)
    '("ket" t)
    '("ket*" t)
    '("innerproduct" TeX-arg-physics-braket)
    '("innerproduct*" TeX-arg-physics-braket)
    '("braket" TeX-arg-physics-braket)
    '("braket*" TeX-arg-physics-braket)
    '("ip" TeX-arg-physics-braket)
    '("outerproduct" TeX-arg-physics-ketbra)
    '("outerproduct*" TeX-arg-physics-ketbra)
    '("dyad" TeX-arg-physics-ketbra)
    '("dyad*" TeX-arg-physics-ketbra)
    '("ketbra" TeX-arg-physics-ketbra)
    '("ketbra*" TeX-arg-physics-ketbra)
    '("op" TeX-arg-physics-ketbra)
    '("op*" TeX-arg-physics-ketbra)
    '("expectationvalue" TeX-arg-physics-expval)
    '("expectationvalue*" TeX-arg-physics-expval)
    '("expval" TeX-arg-physics-expval)
    '("expval*" TeX-arg-physics-expval)
    '("ev" TeX-arg-physics-expval)
    '("ev*" TeX-arg-physics-expval)
    '("matrixelement" 3)
    '("matrixelement*" 3)
    '("matrixelement**" 3)
    '("matrixel" 3) '("matrixel*" 3) '("matrixel**" 3)
    '("mel" 3) '("mel*" 3) '("mel**" 3)
    ;; 2.7 Matrix macros
    '("matrixquantity" TeX-arg-physics-mqty)
    '("matrixquantity*" TeX-arg-physics-mqty*)
    '("mqty" TeX-arg-physics-mqty)
    '("mqty*" TeX-arg-physics-mqty*)
    '("pmqty" t) '("Pmqty" t) '("bmqty" t) '("vmqty" t)
    '("smallmatrixquantity" TeX-arg-physics-mqty)
    '("smallmatrixquantity*" TeX-arg-physics-mqty*)
    '("smqty" TeX-arg-physics-mqty)
    '("smqty*" TeX-arg-physics-mqty*)
    '("spmqty" t) '("sPmqty" t) '("sbmqty" t) '("svmqty" t)
    '("matrixdeterminant" t)
    '("mdet" t) '("smdet" t)
    '("identitymatrix" "Size")
    '("imat" "Size")
    '("xmatrix" "Element" "Rows" "Cols")
    '("xmatrix*" "Element" "Rows" "Cols")
    '("xmat" "Element" "Rows" "Cols") '("xmat*" "Element" "Rows" "Cols")
    '("zeromatrix" "Rows" "Cols")
    '("zmat" "Rows" "Cols")
    '("paulimatrix" "0,1,2,3 or x,y,z")
    '("pmat" "0,1,2,3 or x,y,z")
    '("diagonalmatrix" ["Filler"] t)
    '("dmat" ["Filler"] t)
    '("antidiagonalmatrix" ["Filler"] t)
    '("admatrix" ["Filler"] t)))
 TeX-dialect)

(defvar LaTeX-physics-package-options
  '("bolddel" "arrowdel" "trig" "notrig" "uprightdiff" "italicdiff")
  "Package options for the physics package.")

;;; physics.el ends here
