;;; report.el - Special code for report style.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)
(require 'latex)

(defvar LaTeX-report-class-options
  '("a4paper" "a5paper" "b5paper" "letterpaper" "legalpaper" "executivepaper"
    "landscape" "10pt" "11pt" "12pt" "oneside" "twoside" "draft" "final"
    "titlepage" "notitlepage" "openright" "openany" "onecolumn" "twocolumn"
    "leqno" "fleqn" "openbib")
  "Package options for the report class.")

(TeX-add-style-hook
 "report"
 (lambda ()
   (LaTeX-largest-level-set "chapter")
   (LaTeX-add-counters "part" "chapter" "section" "subsection" "subsubsection"
                       "paragraph" "subparagraph" "figure" "table")
   (LaTeX-add-pagestyles "headings" "myheadings")
   (LaTeX-add-environments "abstract"))
 TeX-dialect)

;;; report.el ends here
