;;; fuz-extra.el --- Extra convenient utils for Fuz  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/cireu/fuz.el

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provided a set of util functions which considered useful
;; but not the core part of `fuz'.

;;; Code:

(require 'inline)

(eval-when-compile
  (require 'pcase))

(defun fuz-highlighter (indices face str)
  "Put face on each position in INDICES of STR.

Sign: (-> (Listof Long) Sym Str Str)"
  (with-temp-buffer
    (insert (propertize str 'read-only nil))
    (goto-char (point-min))
    (dolist (it indices)
      (add-text-properties
       (+ 1 it) (+ 2 it) `(face ,face)))
    (buffer-string)))

(define-inline fuz-sort-with-key! (list comp-fn key)
  "Sort LIST with COMP-FN, transfrom elem in LIST with KEY before comparison."
  (inline-letevals (key comp-fn)
    (inline-quote
     (sort ,list (lambda (e1 e2)
                   (funcall ,comp-fn
                            (funcall ,key e1)
                            (funcall ,key e2)))))))

(defun fuz-memo-function (fn &optional test size)
  "Memoize the FN.

Sign: (All (I O) (->* ((-> I O)) ((U 'eq 'eql 'equal) Int) (-> I O)))

TEST can be one of `eq', `eql', `equal', which used as cache hash's test-fn,
if wasn't provided, use `equal'.
If SIZE was provided, make the hash-table that with initial size SIZE."
  (let* ((arglist `(:test ,(or test #'equal) ,@(if size (list :size size))))
         (cache (apply #'make-hash-table arglist))
         (not-found-sym (make-symbol "not-found")))
    (lambda (input)
      (let ((val (gethash input cache not-found-sym)))
        (if (eq val not-found-sym)
            (puthash input (funcall fn input) cache)
          val)))))

(provide 'fuz-extra)

;; Local Variables:
;; coding: utf-8
;; End:

;;; fuz-extra.el ends here
