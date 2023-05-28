;;; fuz.el --- Fast and precise fuzzy scoring/matching utils  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/cireu/fuz.el
;; Version: 1.4.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp

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

;; `fuz' provided a set of basic fuzzy match and indices searching function.
;; The core part of algorithm is powered by Rust, with the help of emacs
;; dynamic modules.

;; Public APIs:
;; - `fuz-fuzzy-match-skim'
;; - `fuz-fuzzy-match-clangd'
;; - `fuz-calc-score-clangd'
;; - `fuz-calc-score-skim'
;; - `fuz-find-indices-clangd'
;; - `fuz-find-indices-skim'

;;; Code:

(eval-when-compile
  (require 'subr-x)

  ;; Backward compatibility for Emacs 25
  (unless (>= emacs-major-version 26)
    (unless (fboundp 'if-let*) (defalias 'if-let* #'if-let))))

(require 'fuz-core nil t)
(require 'fuz-extra)

(declare-function fuz-core-calc-score-clangd "fuz-core")
(declare-function fuz-core-calc-score-skim "fuz-core")
(declare-function fuz-core-find-indices-clangd "fuz-core")
(declare-function fuz-core-find-indices-skim "fuz-core")

;;; Export function aliases

(defalias 'fuz-calc-score-clangd #'fuz-core-calc-score-clangd)
(defalias 'fuz-calc-score-skim #'fuz-core-calc-score-skim)
(defalias 'fuz-find-indices-clangd #'fuz-core-find-indices-clangd)
(defalias 'fuz-find-indices-skim #'fuz-core-find-indices-skim)

;;; Utils

(defsubst fuz-fuzzy-match-skim (pattern str)
  "Match STR against PATTERN, using skim's algorithm.

Sign: (-> Str Str (Option (Listof Long)))

Return (SCORE . (INDICES)) if matched, otherwise return `nil'."
  (if-let* ((total-score (fuz-calc-score-skim pattern str)))
      (cons total-score (fuz-find-indices-skim pattern str))
    nil))

(defsubst fuz-fuzzy-match-clangd (pattern str)
  "Match STR against PATTERN, using clangd's algorithm.

Sign: (-> Str Str (Option (Listof Long)))

Return (SCORE . (INDICES)) if matched, otherwise return `nil'."
  (if-let* ((total-score (fuz-calc-score-clangd pattern str)))
      (cons total-score (fuz-find-indices-clangd pattern str))
    nil))

;;; Bootstrap helper

(defun fuz-build-and-load-dymod ()
  "Build and load dyamic module."
  (interactive)
  (unless (executable-find "cargo")
    (error "Rust package manager \"cargo\" not found!"))
  (let* ((default-directory (file-name-directory (locate-library "fuz")))
         (dll-name (cl-case system-type
                     ((windows-nt ms-dos cygwin) "fuz_core.dll")
                     (darwin "libfuz_core.dylib")
                     (t "libfuz_core.so")))
         (target-name (cl-case system-type
                        ((windows-nt ms-dos cygwin) "fuz-core.dll")
                        (t "fuz-core.so")))
         (dll-path (expand-file-name (format "target/release/%s" dll-name)))
         (target-path (expand-file-name target-name))
         (buf (generate-new-buffer "*fuz compilation*"))
         (move-file-fn (cl-case system-type
                         ;; Need root permission to make symlink on Windows 10
                         (windows-nt #'copy-file)
                         (t #'make-symbolic-link))))
    (message "Compiling the dynamic module of `fuz', please wait.")
    (pop-to-buffer buf)
    (let ((errno (call-process "cargo" nil buf t "build" "--release")))
      (if (= errno 0)
          (progn
            (funcall move-file-fn dll-path target-path)
            (load target-path nil t)
            (message "Successfully build dynamic module."))
        (error "Failed to compile dynamic modules, check buffer \"%s\" for detailed information."
               (buffer-name buf))))))

(provide 'fuz)

;; Local Variables:
;; coding: utf-8
;; End:

;;; fuz.el ends here
