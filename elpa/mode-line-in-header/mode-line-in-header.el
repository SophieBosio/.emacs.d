;;; mode-line-in-header.el --- Minor mode to display the mode line in the header of the current buffer
;; Version: 0.0.20140228

;; Copyright (C) 2015  Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: mode-line
;; Package-Version: 0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor mode displays the mode line in the header of the current
;; buffer.

;; This mode is simply a cleaned up version of one of the snippets on
;; [[http://bzg.fr/emacs-strip-tease.html][#Emacs, naked]].

;;; Usage:

;; (mode-line-in-header 1)

;;; Code:

(defcustom header-line-format nil
  "Analog to `mode-line-format', contains a string, symbol, or
list describing the mode line to display in the header of the
current buffer."
  :type 'string
  :group 'editing-basics)

;;;###autoload
(define-minor-mode mode-line-in-header
  "Minor mode to display the mode line in the header of the current buffer."
  :init-value nil
  :lighter " mode-header"
  :global nil
  :group 'editing-basics
  (if (not header-line-format)
      (setq header-line-format mode-line-format
            mode-line-format   nil)
    (setq mode-line-format header-line-format
          header-line-format nil))
  (set-window-buffer nil (current-buffer)))

;;;###autoload
(define-globalized-minor-mode
  global-mode-line-in-header
  mode-line-in-header
  (lambda () (mode-line-in-header 1)))

(provide 'mode-line-in-header)

;;; mode-line-in-header.el ends here
