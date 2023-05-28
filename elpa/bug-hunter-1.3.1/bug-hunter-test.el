;;; bug-hunter-test.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: https://github.com/Malabarba/elisp-bug-hunter
;; Version: 1.3.1
;; Keywords: lisp
;; Package-Requires: ((seq "1.3") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(unless (bound-and-true-p package--initialized)  ;; FIXME: Too lax!
  (setq
   package-user-dir (expand-file-name
                     (format ".cask/%s/elpa" emacs-version)
                     (file-name-directory load-file-name)))

  (package-initialize))

(require 'ert)
;; (require 'cl-lib)
(require 'bug-hunter)
;; (fset 'bug-hunter--report #'ignore)
;; (fset 'bug-hunter--report-end #'ignore)

(ert-deftest bug-hunter-test ()
  (should
   (equal [(void-variable not-defined) 5 2 not-defined]
          (bug-hunter-hunt
           '(((setq test 1) 3 0)
             ((setq test 2) 4 1)
             (not-defined 5 2))
           nil)))
  (should
   (equal [(assertion-triggered t) 2 11 (setq test2 2)]
          (bug-hunter-hunt
           '(((setq test0 0) 0 9)
             ((setq test1 1) 1 10)
             ((setq test2 2) 2 11))
           '(ignore-errors (> test2 test1))))))

(ert-deftest bug-hunter-test-nobug ()
  (should-error (bug-hunter-hunt
                 '(((setq test 1) 0 1)
                   ((setq test 2) 0 1))
                 nil)))

(ert-deftest bug-hunter-test-volcano ()
  (should-error
   (bug-hunter-hunt nil 'not-defined)))

(ert-deftest bug-hunter-test-interactive ()
  (cl-letf (((symbol-function #'y-or-n-p) #'ignore)
            ((symbol-function #'read-char-choice) #'ignore))
    (should-error (bug-hunter-hunt
                   '(((kill-emacs) 0 1))
                   'interactive))))

(ert-deftest bug-hunter-looong-hunt ()
  (let* ((size 30)
         (forms (make-list size '((setq dummy 1) 12 90))))
    (dotimes (n size)
      (setcar (elt forms (- size n 1)) 'not-defined)
      (should
       (equal [(void-variable not-defined) 12 90 not-defined]
              (bug-hunter-hunt forms nil)))))
  (let* ((size 8)
         (forms (make-list size '(setq dummy 1))))
    (dotimes (n size)
      (let ((pos (- size n 1)))
        (setf (elt forms pos) 'not-defined)
        (should
         (equal (vector pos '(bug-caught void-variable not-defined))
                (bug-hunter--bisect-start forms nil)))))))

(ert-deftest bug-hunter-reader-error-test ()
  (let ((file (expand-file-name "bug-hunter-test-dummy-file"
                                default-directory)))
    (with-temp-file file
      (insert "(setq useless 1)\n#\n(setq useless 1)\n"))
    (should
     (equal (bug-hunter-file file nil)
            [(invalid-read-syntax "#") 2 0]))
    (should
     (equal '(bug-caught (invalid-read-syntax "#") 2 0)
            (bug-hunter--read-contents file)))
    (with-temp-file file
      (insert "(setq useless 1)\n)\n(setq useless 1)\n"))
    (should
     (equal '(bug-caught (invalid-read-syntax ")") 2 0)
            (bug-hunter--read-contents file)))
    (with-temp-file file
      (insert "(setq useless 1)\n(\n(setq useless 1)\n"))
    (should
     (equal '(bug-caught (end-of-file) 2 0)
            (bug-hunter--read-contents file)))))


(provide 'bug-hunter-test)
;;; bug-hunter-test.el ends here
