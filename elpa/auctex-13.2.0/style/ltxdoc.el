;;; ltxdoc.el --- AUCTeX style for `ltxdoc.cls'  -*- lexical-binding: t; -*-

;; Copyright (C) 2004--2022 Free Software Foundation, Inc.

;; Author: Frank Küster <frank@kuesterei.ch>
;; Maintainer: auctex-devel@gnu.org
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

;; This file adds support for `ltxdoc.cls' v2.1d, dated 2021/12/07.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(defvar LaTeX-article-class-options)

(TeX-add-style-hook
 "ltxdoc"
 (lambda ()

   ;; ltxdoc.cls loads shortvrb.sty and sets '|' as a shorthand.  We
   ;; append it to a local version of `LaTeX-shortvrb-chars' before
   ;; running the style hook for 'shortvrb' which is done inside
   ;; 'doc.el':
   (add-to-list (make-local-variable 'LaTeX-shortvrb-chars) ?| t)

   (TeX-run-style-hooks "doc" "ltx-base" "article")

   (TeX-add-symbols
    '("cmd" TeX-arg-macro)
    '("cs" (TeX-arg-completing-read (TeX-symbol-list) "Macro"))
    '("marg" "Mandatory argument")
    '("oarg" "Optional argument")
    '("parg" "Picture mode argument")

    `("DocInclude"
      ,(lambda (optional)
         (let ((file (file-relative-name
                      (read-file-name
                       (TeX-argument-prompt optional nil "File to include")
                       nil nil nil nil
                       (lambda (x)
                         (or (file-directory-p x)
                             (string-match "\\.\\(fdd\\|dtx\\)\\'" x))))
                      (TeX-master-directory))))
           (TeX-argument-insert file optional)))))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("cmd"  "{")
                                ("cs"   "{")
                                ("marg" "{")
                                ("oarg" "{")
                                ("parg" "{"))
                              'textual)
     (font-latex-add-keywords '("DocInclude" "{")
                              'reference)))
 TeX-dialect)

(defvar LaTeX-ltxdoc-class-options
  (progn
    (TeX-load-style "article")
    (append (remove "a5paper" LaTeX-article-class-options)
            '("nocfg" "doc2")))
  "Class options for the ltxdoc class.
All options are passed to article class, \"a5paper\" is disabled
by ltxdoc.  Therefore it is also removed here.")

;; Local Variables:
;; coding: utf-8
;; End:

;;; ltxdoc.el ends here
