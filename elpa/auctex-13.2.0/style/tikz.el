;;; tikz.el --- AUCTeX style for `tikz.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2022  Free Software Foundation, Inc.

;; Author: Matthew Leach <matthew@mattleach.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2016-22-03
;; Keywords: tex tikz

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

;; This file adds some support for `tikz.sty'

;;; Code:

(require 'tex)
(require 'latex)

;; Silence compiler
(declare-function ConTeXt-add-environments "context"
                  (&rest environments))

(defconst TeX-TikZ-point-function-map
  '(("Rect Point" TeX-TikZ-arg-rect-point)
    ("Polar Point" TeX-TikZ-arg-polar-point)
    ("Named Point" TeX-TikZ-arg-named-point))
  "An alist of point specification types and their functions.")

(defconst TeX-TikZ-relative-point-function-map
  (apply #'append (mapcar
                   (lambda (point-map)
                     (let ((key (car point-map))
                           (value (cadr point-map)))
                       `((,(concat "+" key) ,value "+")
                         (,(concat "++" key) ,value "++"))))
                   TeX-TikZ-point-function-map))
  "`TeX-TikZ-point-function-map' with \"+\" and \"++\" as a prefix.")

(defconst TeX-TikZ-path-connector-function-map
  '(("--" identity)
    ("|-" identity)
    ( "-|" identity)
    ("sin" identity)
    ("cos" identity))
  "An alist of path connectors.")

(defconst TeX-TikZ-draw-arg-function-map
  `(,@TeX-TikZ-point-function-map
    ,@TeX-TikZ-relative-point-function-map
    ,@TeX-TikZ-path-connector-function-map
    ("Node" TeX-TikZ-arg-node)
    ("Circle" TeX-TikZ-arg-circle)
    ("Arc" TeX-TikZ-arg-arc)
    ("Parabola" TeX-TikZ-arg-parabola)
    ("Grid" TeX-TikZ-arg-grid))
  "An alist of argument names and functions for TikZ's \\draw.")

(defun TeX-TikZ-get-opt-arg-string (arg &optional open close)
  "Return a string for optional arguments.
If ARG is nil or \"\", return \"\".  Otherwise return \"OPEN ARG
CLOSE\".  If OPEN and CLOSE are nil, set them to `LaTeX-optop'
and `LaTeX-optcl' respectively."
  (unless (or open close)
    (setq open LaTeX-optop)
    (setq close LaTeX-optcl))
  (if (and arg (> (length arg) 0))
      (concat open arg close)
    ""))

(defun TeX-TikZ-arg-rect-point (_ignored &optional prefix)
  "Prompt the user for a point on the Cartesian plane.
Ask the user for an X and Y coordinate, and return the string
\"(X,Y)\"."
  (let ((x (TeX-read-string (TeX-argument-prompt nil nil "X-coordinate")))
        (y (TeX-read-string (TeX-argument-prompt nil nil "Y-coordinate"))))
   (concat " " prefix "(" x ", " y") ")))

(defun TeX-TikZ-arg-polar-point (_ignored &optional prefix)
  "Prompt the user for a point on the polar plane.
Ask the user for r and theta values, and return the string
\"(THETA:R)\"."
  (let ((r (TeX-read-string (TeX-argument-prompt nil nil "R")))
        (theta (TeX-read-string (TeX-argument-prompt nil nil "Theta"))))
   (concat " " prefix "(" theta ":" r ") ")))

(defun TeX-TikZ-arg-options (optional)
  "Prompt the user for options to a TikZ macro.
If OPTIONAL is nil, always return `LaTeX-optop' and
`LaTeX-optcl', even if the user doesn't provide any input."
  (let ((options (TeX-read-string (TeX-argument-prompt optional nil "Options" ))))
    (if optional
        (TeX-TikZ-get-opt-arg-string options)
      (concat LaTeX-optop options LaTeX-optcl))))

(defun TeX-TikZ-arg-name (optional)
  "Prompt the user for a TikZ name.
If OPTIONAL is nil, always return \"()\", even if the user
doesn't provide any input."
  (let ((name (TeX-read-string (TeX-argument-prompt optional nil "Name" ))))
    (if optional
        (TeX-TikZ-get-opt-arg-string name "(" ")")
      (concat "(" name ")"))))

(defun TeX-TikZ-arg-label (optional)
  "Prompt the user for TikZ label.
If OPTIONAL is nil always return `TeX-grop' and `TeX-grcl', even
if the user doesn't provide any input."
  (let ((label (TeX-read-string (TeX-argument-prompt optional nil "Label" ))))
    (if optional
        (TeX-TikZ-get-opt-arg-string label TeX-grop TeX-grcl)
      (concat TeX-grop label TeX-grcl))))

(defun TeX-TikZ-arg-node (_ignored)
  "Prompt the user for the deatils of a node.
Ask the user for the name and text for a node and return the
string \"node[OPTIONS](NAME){TEXT}\"."
  (let ((options (TeX-TikZ-arg-options t))
        (name (TeX-TikZ-arg-name t))
        (label (TeX-TikZ-arg-label nil)))
    (concat "node" options name label " ")))

(defun TeX-TikZ-get-arg-type (types prompt)
  "Prompt the user for an argument type.
TYPES is a list of possible types that the user can specify.  Use
PROMPT as the prompt for input."
  (let ((completion-ignore-case t))
    (completing-read prompt types nil t)))

(defun TeX-TikZ-single-macro-arg (function-alist prompt &optional optional)
  "Prompt the user for a single argument to compose a TikZ macro.
FUNCTION-ALIST is a mapping of argument-types to functions.  The
user is prompted for the argument type, the chosen function is
then called and the value returned.  PROMPT is used as the prompt
for the argument type.  When OPTIONAL is non-nil, add \"\" to
FUNCTION-ALIST with a mapping to `identity', permitting an
optional input."
  (let* ((selected-argument-type (TeX-TikZ-get-arg-type function-alist prompt))
         (fn-alist-with-optional-elm (if optional
                                         `(,@function-alist ("" identity))
                                       function-alist))
         (selected-mapping (assoc selected-argument-type
                                  fn-alist-with-optional-elm)))

    ;; Build the funcall we wish to evaluate.  This will be the function
    ;; to be called (the second element in the assoc element),
    ;; followed by the type name (the first element), followed by any
    ;; other elements in the list as extra arguments.
    (apply
     (cadr selected-mapping)
     (car selected-mapping)
     (cddr selected-mapping))))


(defun TeX-TikZ-macro-arg (function-alist)
  "Prompt the user for arguments to compose a TikZ macro.
FUNCTION-ALIST is a mapping of argument-types to functions.  The
user is repeatedly prompted for the next argument-type; they can
choose form the cars in FUNCTION-ALIST and the appropriate
function is then called.  If the user enters \"\", then the macro
is finished."
  (let* ((options (TeX-TikZ-arg-options t))
         (prompt "Next argument type (RET to finish): ")
         (string-to-insert (TeX-TikZ-single-macro-arg function-alist prompt t)))

    ;; Insert the macro options.
    (insert options " ")

    ;; Iteratively prompt the user for TikZ's arguments until "" is
    ;; returned.
    (while (not (string= string-to-insert ""))
      (insert string-to-insert)
      (setq string-to-insert
            (TeX-TikZ-single-macro-arg function-alist prompt t)))

    ;; Finish the macro.
    (insert ";")))

(defun TeX-TikZ-find-named-points ()
  "Find TiKZ named points in current enviroment.
Begin by finding the span of the current TikZ enviroment and then
searching within that span to find all named-points and return
them as a list of strings, dropping the \\='()\\='."
  ;; FIXME: This function depends on `LaTeX-find-matching-begin' and
  ;; `LaTeX-find-matching-end', so it doesn't work for ConTeXt and
  ;; plain TeX.  In addition, it isn't compatible with the TikZ code
  ;; following \tikz.
  (let* ((env-end (save-excursion
                    (LaTeX-find-matching-end)
                     (point)))
         (matches))
    ;; TODO: Handle cases where we are in a nested environment, \scope
    ;; for example.
    (save-excursion
      (LaTeX-find-matching-begin)
      (save-match-data
        (while (re-search-forward TeX-TikZ-point-name-regexp env-end t)
          (push (match-string 1) matches))))
    matches))

(defun TeX-TikZ-arg-named-point (_ignored &optional prefix)
  "Prompt the user for the name of a previous named-point."
  (let ((point-name (completing-read "Point name: "
                                     (TeX-TikZ-find-named-points))))
    (concat " " prefix "(" point-name ") ")))

(defun TeX-TikZ-arg-circle (_ignored)
  "Prompt the user for the arguments to the circle command."
  (let ((options (TeX-TikZ-arg-options t)))
    (concat "circle" options)))

(defun TeX-TikZ-arg-arc (_ignored)
  "Prompt the user for the arguments to the arc command."
  (let ((options (TeX-TikZ-arg-options t)))
    (concat "arc" options)))

(defun TeX-TikZ-arg-bend (optional)
  "Prompt the user for a bend argument.
If OPTIONAL is non-nil and the user doesn't provide a point,
return \"\"."
  (let ((point
         (TeX-TikZ-single-macro-arg TeX-TikZ-point-function-map
                                    (TeX-argument-prompt optional nil "Bend point")
                                    optional)))
    (if (string= point "")
        point
      (concat " bend" point))))

(defun TeX-TikZ-arg-parabola (_ignored)
  "Prompt the user for the arguments to the parabola command."
  (let ((options (TeX-TikZ-arg-options t))
        (bend (TeX-TikZ-arg-bend t)))
       (concat "parabola" options bend)))

(defun TeX-TikZ-arg-grid (_ignored)
  "Prompt the user for the arguments to the grid command."
  (let ((options (TeX-TikZ-arg-options t)))
    (concat "grid" options)))

(defun TeX-TikZ-draw-arg (_ignored)
  "Prompt the user for the arguments to a TikZ draw macro."
  (TeX-TikZ-macro-arg TeX-TikZ-draw-arg-function-map))

(defun TeX-TikZ-coordinate-arg (_ignored)
  "Prompt the user for the arguments to a TikZ coordinate macro."
  (let ((options (TeX-TikZ-arg-options t))
        (name (TeX-TikZ-arg-name nil))
        (point (TeX-TikZ-single-macro-arg TeX-TikZ-point-function-map
                                          "Coordinate point type: ")))
    (insert options " " name " at" point ";")))

(defun TeX-TikZ-node-arg (_ignored)
  "Prompt the user for the arguments to a TikZ node macro."
  (let ((options (TeX-TikZ-arg-options t))
        (name (TeX-TikZ-arg-name t))
        (point (TeX-TikZ-single-macro-arg TeX-TikZ-point-function-map
                                          "Node point type: "))
        (label (TeX-TikZ-arg-label nil)))
    (insert options " " name  " at" point label ";")))

;; TODO: Add similar support for plain TeX.
(defun TeX-TikZ-env-scope (_ignored)
  "Ask the user for TikZ option and insert it with surrounding \"[]\".
If the user provides empty input, insert \"[]\" anyway and put the
point inside it."
  (let ((option (TeX-TikZ-arg-options nil)))
    (insert option)
    (if (string= option "[]")
        (set-marker TeX-exit-mark (1- (point))))))

(TeX-add-style-hook
 "tikz"
 (lambda ()
   (TeX-add-symbols
    '("draw" (TeX-TikZ-draw-arg))
    '("coordinate" (TeX-TikZ-coordinate-arg))
    '("node" (TeX-TikZ-node-arg))
    '("tikz" ["TikZ option"])
    '("tikzset" "TikZ option")
    ;; FIXME:
    ;; 1. usetikzlibrary isn't much useful without completion support
    ;;    for available libraries.
    ;; 2. ConTeXt users may prefer [...] over {...} as the argument.
    '("usetikzlibrary" t)
    ;; XXX: Maybe we should create pgffor.el and factor out this entry
    ;; into it.
    '("foreach" (TeX-arg-literal " ") (TeX-arg-free "Variable(s)")
      (TeX-arg-literal " ") ["Foreach option"]
      (TeX-arg-literal " in ") "Value list (Use \"...\" for range)"
      (TeX-arg-literal " ") t))))

;; LaTeX/docTeX specific stuff
(TeX-add-style-hook
 "tikz"
 (lambda ()
   (LaTeX-add-environments
    '("tikzpicture" ["TikZ option"])
    '("scope" LaTeX-env-args TeX-TikZ-env-scope))
   ;; tikz.sty loads pgfcore.sty, which loads packages graphicx,
   ;; keyval and xcolor, too.
   (TeX-run-style-hooks "pgf" "graphicx" "keyval" "xcolor"))
 :latex)

;; ConTeXt specific stuff
(TeX-add-style-hook
 "tikz"
 (lambda ()
   (ConTeXt-add-environments
    '("tikzpicture" ["TikZ option"])
    '("scope" ConTeXt-env-args TeX-TikZ-env-scope)))
 :context)

;; plain TeX specific stuff
(TeX-add-style-hook
 "tikz"
 (lambda ()
   (TeX-add-symbols
    '("tikzpicture" ["TikZ option"])
    "endtikzpicture"
    '("scope" ["TikZ option"])
    "endscope"))
 :plain-tex)

;;; tikz.el ends here
