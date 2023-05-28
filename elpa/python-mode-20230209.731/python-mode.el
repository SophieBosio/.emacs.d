;;; python-mode.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*- 

;; Version: 6.3.1

;; URL: https://gitlab.com/groups/python-mode-devs

;; Package-Requires: ((emacs "24"))

;; Author: 2015-2023 https://gitlab.com/groups/python-mode-devs
;;         2003-2014 https://launchpad.net/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Includes a minor mode for handling a Python/IPython shell, and can
;; take advantage of Pymacs when installed.

;; See documentation in README.org, README.DEVEL.org

;; Please report bugs at
;; https://gitlab.com/python-mode-devs/python-mode/issues

;; available commands are documented in directory "doc" as
;; commands-python-mode.org

;; As for ‘py-add-abbrev’:
;; Similar to ‘add-mode-abbrev’, but uses
;; ‘py-partial-expression’ before point for expansion to
;; store, not ‘word’.  Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; ‘py-expression’ composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; ‘py-partial-expression’ beginns with a "(", which is
;; not taken as proposal.

;;; Code:

(require 'ansi-color)
(ignore-errors (require 'subr-x))
(require 'cc-cmds)
(require 'comint)
(require 'compile)
(require 'custom)
(require 'ert)
(require 'flymake)
(require 'hippie-exp)
(require 'hideshow)
(require 'json)
(require 'shell)
(require 'thingatpt)
(require 'which-func)
(require 'tramp)
(require 'tramp-sh)
(require 'org-loaddefs)
(unless (functionp 'mapcan)
  (require 'cl-extra)
  ;; mapcan doesn't exist in Emacs 25
  (defalias 'mapcan 'cl-mapcan)
  )

;; (require 'org)

(defgroup python-mode nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "py-")

(defconst py-version "6.3.1")

(defvar py-install-directory nil
  "Make sure it exists.")

(defcustom py-install-directory nil
  "Directory where python-mode.el and it's subdirectories should be installed.

Needed for completion and other environment stuff only."

  :type 'string
  :tag "py-install-directory"
  :group 'python-mode)

(or
 py-install-directory
 (and (buffer-live-p (ignore-errors (set-buffer (get-buffer "python--mode.el"))))
      (setq py-install-directory (ignore-errors (file-name-directory (buffer-file-name (get-buffer  "python-mode.el"))))))
 (and (buffer-live-p (ignore-errors (set-buffer (get-buffer "python-components-mode.el"))))
      (setq py-install-directory (ignore-errors (file-name-directory (buffer-file-name (get-buffer  "python-components-mode.el")))))))

(defcustom py-font-lock-defaults-p t
  "If fontification is not required, avoiding it might speed up things."

  :type 'boolean
  :tag "py-font-lock-defaults-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-pythonpath ""
  "Define $PYTHONPATH here, if needed.

Emacs doesn't read .bashrc"

  :type 'string
  :tag "py-pythonpath"
  :group 'python-mode)

(defcustom python-mode-modeline-display "Py"
  "String to display in Emacs modeline."

  :type 'string
  :tag "python-mode-modeline-display"
  :group 'python-mode)

(defcustom py-python2-modeline-display "Py2"
  "String to display in Emacs modeline."

  :type 'string
  :tag "python2-mode-modeline-display"
  :group 'python-mode)

(defcustom py-python3-modeline-display "Py3"
  "String to display in Emacs modeline."

  :type 'string
  :tag "python3-mode-modeline-display"
  :group 'python-mode)

(defcustom py-ipython-modeline-display "IPy"
  "String to display in Emacs modeline."

  :type 'string
  :tag "ipython-modeline-display"
  :group 'python-mode)

(defcustom py-jython-modeline-display "Jy"
  "String to display in Emacs modeline."

  :type 'string
  :tag "jython-modeline-display"
  :group 'python-mode)

(defcustom py-extensions "py-extensions.el"
  "File where extensions to python-mode.el should be installed.

Used by virtualenv support."

  :type 'string
  :tag "py-extensions"
  :group 'python-mode)

(defcustom info-lookup-mode "python"
  "Which Python documentation should be queried.

Make sure it's accessible from Emacs by \\<emacs-lisp-mode-map> \\[info] ...
See INSTALL-INFO-FILES for help."

  :type 'string
  :tag "info-lookup-mode"
  :group 'python-mode)

(defcustom py-fast-process-p nil
  "Use ‘py-fast-process’.

Commands prefixed \"py-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Results arrive in output buffer, which is not in comint-mode"

  :type 'boolean
  :tag "py-fast-process-p"
  :group 'python-mode
  :safe 'booleanp)

;; credits to python.el
(defcustom py-shell-compilation-regexp-alist
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "‘compilation-error-regexp-alist’ for ‘py-shell’."
  :type '(alist string)
  :tag "py-shell-compilation-regexp-alist"
  :group 'python-mode)

(defcustom py-shift-require-transient-mark-mode-p t
  "If py-shift commands require variable ‘transient-mark-mode’ set to t.

Default is t"

  :type 'boolean
  :tag "py-shift-require-transient-mark-mode-p"
  :group 'python-mode
  :safe 'booleanp)

(defvar py-fast-output-buffer "*Python Fast*"
  "Internally used. ‘buffer-name’ for fast-processes.")

(defvar py-this-result nil
  "Internally used, store return-value.")

(defconst py-coding-re
  "\\(# *coding[ \t]*=\\|#[ \t]*\-*\-[ \t]*coding:\\|#[ \t]*encoding:\\)[ \t]*\\([[:graph:]+]\\)"
 "Fetch the coding cookie maybe.")

(defcustom py-comment-auto-fill-p nil
  "When non-nil, fill comments.

Defaut is nil"

  :type 'boolean
  :tag "py-comment-auto-fill-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-sexp-use-expression-p nil
  "If non-nil, ‘forward-sexp’ will call ‘py-forward-expression’.

Respective ‘backward-sexp’ will call ‘py-backward-expression’
Default is t"
  :type 'boolean
  :tag "py-sexp-use-expression-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-session-p t
  "If commands would use an existing process.

Default is t"

  :type 'boolean
  :tag "py-session-p"
  :group 'python-mode
  :safe 'booleanp)

(defvar py-chars-before " \t\n\r\f"
  "Used by ‘py--string-strip’.")

(defvar py-chars-after " \t\n\r\f"
    "Used by ‘py--string-strip’.")

(unless (functionp 'file-local-name)
  (defun file-local-name (file)
    "Return the local name component of FILE.
This function removes from FILE the specification of the remote host
and the method of accessing the host, leaving only the part that
identifies FILE locally on the remote system.
The returned file name can be used directly as argument of
‘process-file’, ‘start-file-process’, or ‘shell-command’."
    (or (file-remote-p file 'localname) file)))

(defun py---emacs-version-greater-23 ()
  "Return ‘t’ if emacs major version is above 23"
  (< 23 (string-to-number (car (split-string emacs-version "\\.")))))

;; (format "execfile(r'%s')\n" file)
(defun py-execute-file-command (filename)
  "Return the command using FILENAME."
  (format "exec(compile(open(r'%s').read(), r'%s', 'exec')) # PYTHON-MODE\n" filename filename)
  )

(defun py--beginning-of-buffer-p ()
  "Returns position, if cursor is at the beginning of buffer.
Return nil otherwise. "
  (when (bobp)(point)))

;;  (setq strip-chars-before  "[ \t\r\n]*")
(defun py--string-strip (str &optional chars-before chars-after)
  "Return a copy of STR, CHARS removed.
`CHARS-BEFORE' and `CHARS-AFTER' default is \"[ \t\r\n]*\",
i.e. spaces, tabs, carriage returns, newlines and newpages."
  (let ((s-c-b (or chars-before
                   py-chars-before))
        (s-c-a (or chars-after
                   py-chars-after))
        (erg str))
    (setq erg (replace-regexp-in-string  s-c-b "" erg))
    (setq erg (replace-regexp-in-string  s-c-a "" erg))
    erg))

(defun py-toggle-session-p (&optional arg)
  "Switch boolean variable ‘py-session-p’.

With optional ARG message state switched to"
  (interactive "p")
  (setq py-session-p (not py-session-p))
  (when arg (message "py-session-p: %s" py-session-p)))

(defcustom py-max-help-buffer-p nil
  "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil.  In ‘help-buffer’, \"q\" will close it."

  :type 'boolean
  :tag "py-max-help-buffer-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-highlight-error-source-p nil
  "Respective code in source-buffer will be highlighted.

Default is nil.

\\<python-mode-map> ‘py-remove-overlays-at-point’ removes that highlighting."
  :type 'boolean
  :tag "py-highlight-error-source-p"
  :group 'python-mode)

(defcustom py-set-pager-cat-p nil
  "If the shell environment variable $PAGER should set to ‘cat’.

Avoids lp:783828,
 \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module ‘os’"

  :type 'boolean
  :tag "py-set-pager-cat-p"
  :group 'python-mode)

(defcustom py-empty-line-closes-p nil
  "When non-nil, dedent after empty line following block.

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil"

  :type 'boolean
  :tag "py-empty-line-closes-p"
  :group 'python-mode)

(defcustom py-prompt-on-changed-p t
  "Ask for save before a changed buffer is sent to interpreter.

Default is t"

  :type 'boolean
  :tag "py-prompt-on-changed-p"
  :group 'python-mode)

(defcustom py-dedicated-process-p nil
  "If commands executing code use a dedicated shell.

Default is nil

When non-nil and ‘py-session-p’, an existing
dedicated process is re-used instead of default
 - which allows executing stuff in parallel."
  :type 'boolean
  :tag "py-dedicated-process-p"
  :group 'python-mode)

(defcustom py-store-result-p nil
  "Put resulting string of `py-execute-...' into ‘kill-ring’.

Default is nil"

  :type 'boolean
  :tag "py-dedicated-process-p"
  :group 'python-mode)

(defvar py-shell--font-lock-buffer "*PSFLB*"
  "May contain the ‘py-buffer-name’ currently fontified." )

(defvar py-return-result-p nil
  "Internally used.

When non-nil, return resulting string of `py-execute-...'.
Imports will use it with nil.
Default is nil")

(defun py-toggle-py-return-result-p ()
  "Toggle value of ‘py-return-result-p’."
  (interactive)
  (setq py-return-result-p (not py-return-result-p))
  (when (called-interactively-p 'interactive) (message "py-return-result-p: %s" py-return-result-p)))

(defcustom py--execute-use-temp-file-p nil
 "Assume execution at a remote machine.

 where write-access is not given."

 :type 'boolean
 :tag "py--execute-use-temp-file-p"
 :group 'python-mode)

(defvar py--match-paren-forward-p nil
  "Internally used by ‘py-match-paren’.")

(defvar py-new-session-p t
  "Internally used.  See lp:1393882.

Restart ‘py-shell’ once with new Emacs/‘python-mode’.")

(defcustom py-electric-close-active-p nil
  "Close completion buffer if no longer needed.

Works around a bug in ‘choose-completion’.
Default is nil"
  :type 'boolean
  :tag "py-electric-close-active-p"
  :group 'python-mode)

(defcustom py-hide-show-minor-mode-p nil
  "If hide-show minor-mode should be on, default is nil."

  :type 'boolean
  :tag "py-hide-show-minor-mode-p"
  :group 'python-mode)

(defcustom py-do-completion-p t
  "Permits disabling all python-mode native completion.

Default is ‘t’.
See #144, how to disable process spawn for autocompletion"

  :type 'boolean
  :tag "py-do-completion-p"
  :group 'python-mode)

(defcustom py-load-skeletons-p nil
  "If skeleton definitions should be loaded, default is nil.

If non-nil and variable ‘abbrev-mode’ on, block-skeletons will inserted.
Pressing \"if<SPACE>\" for example will prompt for the if-condition."

  :type 'boolean
  :tag "py-load-skeletons-p"
  :group 'python-mode)

(defcustom py-if-name-main-permission-p t
  "Allow execution of code inside blocks started.

by \"if __name__== '__main__':\".
Default is non-nil"

  :type 'boolean
  :tag "py-if-name-main-permission-p"
  :group 'python-mode)

(defcustom py-use-font-lock-doc-face-p nil
  "If documention string inside of def or class get ‘font-lock-doc-face’.

‘font-lock-doc-face’ inherits ‘font-lock-string-face’.
Call \\<emacs-lisp-mode-map> \\[customize-face] in order to have a effect."

  :type 'boolean
  :tag "py-use-font-lock-doc-face-p"
  :group 'python-mode)

(defcustom py-empty-comment-line-separates-paragraph-p t
  "Consider paragraph start/end lines with nothing inside but comment sign.

Default is  non-nil"
  :type 'boolean
  :tag "py-empty-comment-line-separates-paragraph-p"
  :group 'python-mode)

(defcustom py-indent-honors-inline-comment nil
  "If non-nil, indents to column of inlined comment start.
Default is nil."
  :type 'boolean
  :tag "py-indent-honors-inline-comment"
  :group 'python-mode)

(defcustom py-auto-fill-mode nil
  "If ‘python-mode’ should set ‘fill-column’.

according to values
in ‘py-comment-fill-column’ and ‘py-docstring-fill-column’.
Default is  nil"

  :type 'boolean
  :tag "py-auto-fill-mode"
  :group 'python-mode)

(defcustom py-error-markup-delay 4
  "Seconds error's are highlighted in exception buffer."

  :type 'integer
  :tag "py-error-markup-delay"
  :group 'python-mode)

(defcustom py-fast-completion-delay 0.1
  "Used by ‘py-fast-send-string’."

  :type 'float
  :tag "py-fast-completion-delay"
  :group 'python-mode)

(defcustom py-new-shell-delay
    (if (eq system-type 'windows-nt)
      2.0
    1.0)

  "If a new comint buffer is connected to Python.
Commands like completion might need some delay."

  :type 'float
  :tag "py-new-shell-delay"
  :group 'python-mode)

(defcustom py-autofill-timer-delay 1
  "Delay when idle."
  :type 'integer
  :tag "py-autofill-timer-delay"
  :group 'python-mode)

(defcustom py-docstring-fill-column 72
  "Value of ‘fill-column’ to use when filling a docstring.
Any non-integer value means do not use a different value of
‘fill-column’ when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current ‘fill-column’" t))
  :tag "py-docstring-fill-column"
  :group 'python-mode)

(defcustom py-comment-fill-column 79
  "Value of ‘fill-column’ to use when filling a comment.
Any non-integer value means do not use a different value of
‘fill-column’ when filling docstrings."
  :type '(choice (integer)
		 (const :tag "Use the current ‘fill-column’" t))
  :tag "py-comment-fill-column"
  :group 'python-mode)

(defcustom py-fontify-shell-buffer-p nil
  "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If t, related vars like ‘comment-start’ will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives"

  :type 'boolean
  :tag "py-fontify-shell-buffer-p"
  :group 'python-mode)

(defvar py-modeline-display ""
  "Internally used.")

(defcustom py-modeline-display-full-path-p nil
  "If the full PATH/TO/PYTHON be in modeline.

Default is nil. Note: when ‘py-python-command’ is
specified with path, it's shown as an acronym in
‘buffer-name’ already."

  :type 'boolean
  :tag "py-modeline-display-full-path-p"
  :group 'python-mode)

(defcustom py-modeline-acronym-display-home-p nil
  "If the modeline acronym should contain chars indicating the home-directory.

Default is nil"
  :type 'boolean
  :tag "py-modeline-acronym-display-home-p"
  :group 'python-mode)

(defun py-autopair-check ()
  "Check, if ‘autopair-mode’ is available.

Give some hints, if not."
  (interactive)
  (if (featurep 'autopair)
      't
    (progn
      (message "py-autopair-check: %s" "Don't see autopair.el. Make sure, it's installed. If not, maybe see source: URL: http://autopair.googlecode.com")
      nil)))

(defvar highlight-indent-active nil)
(defvar autopair-mode nil)

(defvar-local py--editbeg nil
  "Internally used by ‘py-edit-docstring’ and others")

(defvar-local py--editend nil
  "Internally used by ‘py-edit-docstring’ and others")

(defvar py--oldbuf nil
  "Internally used by ‘py-edit-docstring’.")

(defvar py-edit-buffer "Edit docstring"
  "Name of the temporary buffer to use when editing.")

(defvar py--edit-register nil)

(defvar py-result nil
  "Internally used.  May store result from Python process.

See var ‘py-return-result-p’ and command ‘py-toggle-py-return-result-p’")

(defvar py-error nil
  "Takes the error-messages from Python process.")

(defvar py-python-completions "*Python Completions*"
  "Buffer name for Python-shell completions, internally used.")

(defvar py-ipython-completions "*IPython Completions*"
  "Buffer name for IPython-shell completions, internally used.")

(defcustom py-timer-close-completions-p t
  "If ‘py-timer-close-completion-buffer’ should run, default is non-nil."

  :type 'boolean
  :tag "py-timer-close-completions-p"
  :group 'python-mode)

(defcustom py-autopair-mode nil
  "If ‘python-mode’ calls (autopair-mode-on)

Default is nil
Load ‘autopair-mode’ written by Joao Tavora <joaotavora [at] gmail.com>
URL: http://autopair.googlecode.com"
  :type 'boolean
  :tag "py-autopair-mode"
  :group 'python-mode)

(defcustom py-indent-no-completion-p nil
  "If completion function should insert a TAB when no completion found.

Default is nil"
  :type 'boolean
  :tag "py-indent-no-completion-p"
  :group 'python-mode)

(defcustom py-company-pycomplete-p nil
  "Load company-pycomplete stuff.  Default is  nil."

  :type 'boolean
  :tag "py-company-pycomplete-p"
  :group 'python-mode)

(defvar py-last-position nil
    "Used by ‘py-help-at-point’.

Avoid repeated call at identic pos.")

(defvar py-auto-completion-mode-p nil
  "Internally used by ‘py-auto-completion-mode’.")

(defvar py-complete-last-modified nil
  "Internally used by ‘py-auto-completion-mode’.")

(defvar py--auto-complete-timer nil
  "Internally used by ‘py-auto-completion-mode’.")

(defvar py-auto-completion-buffer nil
  "Internally used by ‘py-auto-completion-mode’.")

(defvar py--auto-complete-timer-delay 1
  "Seconds Emacs must be idle to trigger auto-completion.

See ‘py-auto-completion-mode’")

(defcustom py-auto-complete-p nil
  "Run python-mode's built-in auto-completion via ‘py-complete-function’.

Default is  nil."

  :type 'boolean
  :tag "py-auto-complete-p"
  :group 'python-mode)

(defcustom py-tab-shifts-region-p nil
  "If t, TAB will indent/cycle the region, not just the current line.

Default is  nil
See also ‘py-tab-indents-region-p’"

  :type 'boolean
  :tag "py-tab-shifts-region-p"
  :group 'python-mode)

(defcustom py-tab-indents-region-p nil
  "When t and first TAB doesn't shift, ‘indent-region’ is called.

Default is  nil
See also ‘py-tab-shifts-region-p’"

  :type 'boolean
  :tag "py-tab-indents-region-p"
  :group 'python-mode)

(defcustom py-block-comment-prefix-p t
  "If py-comment inserts ‘py-block-comment-prefix’.

Default is t"

  :type 'boolean
  :tag "py-block-comment-prefix-p"
  :group 'python-mode)

(defcustom py-org-cycle-p nil
  "When non-nil, command ‘org-cycle’ is available at shift-TAB, <backtab>.

Default is nil."
  :type 'boolean
  :tag "py-org-cycle-p"
  :group 'python-mode)

(defcustom py-set-complete-keymap-p  nil
  "If ‘py-complete-initialize’.

Sets up enviroment for Pymacs based py-complete.
 Should load it's keys into ‘python-mode-map’
Default is nil.
See also resp. edit ‘py-complete-set-keymap’"

  :type 'boolean
  :tag "py-set-complete-keymap-p"
  :group 'python-mode)

(defcustom py-outline-minor-mode-p t
  "If outline minor-mode should be on, default is t."
  :type 'boolean
  :tag "py-outline-minor-mode-p"
  :group 'python-mode)

(defvar py-guess-py-install-directory-p nil
  "If in cases, ‘py-install-directory’ isn't set,  ‘py-set-load-path’ guess it.")

(defcustom py-guess-py-install-directory-p nil
  "If in cases, ‘py-install-directory’ isn't set, ‘py-set-load-path’ guesses it."
  :type 'boolean
  :tag "py-guess-py-install-directory-p"
  :group 'python-mode)

(defcustom py-load-pymacs-p nil
  "If Pymacs related stuff should be loaded. Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"
  :type 'boolean
  :tag "py-load-pymacs-p"
  :group 'python-mode)

(defcustom py-verbose-p nil
  "If functions should report results.

Default is nil."
  :type 'boolean
  :tag "py-verbose-p"
  :group 'python-mode)

(defcustom py-sexp-function nil
  "Called instead of ‘forward-sexp’, ‘backward-sexp’.

Default is nil."

  :type '(choice

          (const :tag "default" nil)
          (const :tag "py-forward-partial-expression" py-forward-partial-expression)
          (const :tag "py-forward-expression" py-forward-expression))
  :tag "py-sexp-function"
  :group 'python-mode)

(defcustom py-close-provides-newline t
  "If a newline is inserted, when line after block isn't empty.

Default is non-nil.
When non-nil, ‘py-forward-def’ and related will work faster"
  :type 'boolean
  :tag "py-close-provides-newline"
  :group 'python-mode)

(defcustom py-dedent-keep-relative-column t
  "If point should follow dedent or kind of electric move to end of line.

Default is t - keep relative position."
  :type 'boolean
  :tag "py-dedent-keep-relative-column"
  :group 'python-mode)

(defcustom py-indent-list-style 'line-up-with-first-element
  "Sets the basic indentation style of lists.

The term ‘list’ here is seen from Emacs Lisp editing purpose.
A list symbolic expression means everything delimited by
brackets, parentheses or braces.

Setting here might be ignored in case of canonical indent.

‘line-up-with-first-element’ indents to 1+ column
of opening delimiter

def foo (a,
         b):

but ‘one-level-to-beginning-of-statement’ in case of EOL at list-start

def foo (
    a,
    b):

‘one-level-to-beginning-of-statement’ adds
‘py-indent-offset’ to beginning

def long_function_name(
    var_one, var_two, var_three,
    var_four):
    print(var_one)

‘one-level-from-first-element’ adds ‘py-indent-offset’ from first element
def foo():
    if (foo &&
            baz):
        bar()"
  :type '(choice
          (const :tag "line-up-with-first-element" line-up-with-first-element)
          (const :tag "one-level-to-beginning-of-statement" one-level-to-beginning-of-statement)
          (const :tag "one-level-from-first-element" one-level-from-first-element)
          )
  :tag "py-indent-list-style"
  :group 'python-mode)
(make-variable-buffer-local 'py-indent-list-style)

(defcustom py-closing-list-dedents-bos nil
  "When non-nil, indent lists closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = [
    1, 2, 3,
    4, 5, 6
]

result = some_function_that_takes_arguments(
    \\='a\\=', \\='b\\=', \\='c\\=',
    \\='d\\=', \\='e\\=', \\='f\\='
)

Default is nil, i.e.

my_list = [
    1, 2, 3,
    4, 5, 6
    ]

result = some_function_that_takes_arguments(
    \\='a\\=', \\='b\\=', \\='c\\=',
    \\='d\\=', \\='e\\=', \\='f\\='
    )

Examples from PEP8
URL: https://www.python.org/dev/peps/pep-0008/#indentation"
  :type 'boolean
  :tag "py-closing-list-dedents-bos"
  :group 'python-mode)

(defvar py-imenu-max-items 99)
(defcustom py-imenu-max-items 99
 "Python-mode specific ‘imenu-max-items’."
 :type 'number
 :tag "py-imenu-max-items"
 :group 'python-mode)

(defcustom py-closing-list-space 1
  "Number of chars, closing parenthesis outdent from opening, default is 1."
  :type 'number
  :tag "py-closing-list-space"
  :group 'python-mode)

(defcustom py-max-specpdl-size 99
  "Heuristic exit.
e
Limiting number of recursive calls by ‘py-forward-statement’ and related.
Default is ‘max-specpdl-size’.

This threshold is just an approximation.  It might set far higher maybe.

See lp:1235375. In case code is not to navigate due to errors,
command ‘which-function-mode’ and others might make Emacs hang.

Rather exit than."

  :type 'number
  :tag "py-max-specpdl-size"
  :group 'python-mode)

(defcustom py-closing-list-keeps-space nil
  "If non-nil, closing parenthesis dedents onto column of opening.
Adds ‘py-closing-list-space’.
Default is nil."
  :type 'boolean
  :tag "py-closing-list-keeps-space"
  :group 'python-mode)

(defcustom py-electric-colon-active-p nil
  "‘py-electric-colon’ feature.

Default is nil.  See lp:837065 for discussions.
See also ‘py-electric-colon-bobl-only’"
  :type 'boolean
  :tag "py-electric-colon-active-p"
  :group 'python-mode)

(defcustom py-electric-colon-bobl-only t

  "When inserting a colon, do not indent lines unless at beginning of block.

See lp:1207405 resp. ‘py-electric-colon-active-p’"

  :type 'boolean
  :tag "py-electric-colon-bobl-only"
  :group 'python-mode)

(defcustom py-electric-yank-active-p nil
  "When non-nil, ‘yank’ will be followed by an ‘indent-according-to-mode’.

Default is nil"
  :type 'boolean
  :tag "py-electric-yank-active-p"
  :group 'python-mode)

(defcustom py-electric-colon-greedy-p nil
  "If ‘py-electric-colon’ should indent to the outmost reasonable level.

If nil, default, it will not move from at any reasonable level."
  :type 'boolean
  :tag "py-electric-colon-greedy-p"
  :group 'python-mode)

(defcustom py-electric-colon-newline-and-indent-p nil
  "If non-nil, ‘py-electric-colon’ will call ‘newline-and-indent’.

Default is nil."
  :type 'boolean
  :tag "py-electric-colon-newline-and-indent-p"
  :group 'python-mode)

(defcustom py-electric-comment-p nil
  "If \"#\" should call ‘py-electric-comment’. Default is nil."
  :type 'boolean
  :tag "py-electric-comment-p"
  :group 'python-mode)

(defcustom py-electric-comment-add-space-p nil
  "If ‘py-electric-comment’ should add a space.  Default is nil."
  :type 'boolean
  :tag "py-electric-comment-add-space-p"
  :group 'python-mode)

(defcustom py-mark-decorators nil
  "If decorators should be marked too.

Default is nil.

Also used by navigation"
  :type 'boolean
  :tag "py-mark-decorators"
  :group 'python-mode)

(defcustom py-defun-use-top-level-p nil
 "If ‘beginning-of-defun’, ‘end-of-defun’ calls function ‘top-level’ form.

Default is nil.

beginning-of defun, ‘end-of-defun’ forms use
commands ‘py-backward-top-level’, ‘py-forward-top-level’

‘mark-defun’ marks function ‘top-level’ form at point etc."

 :type 'boolean
  :tag "py-defun-use-top-level-p"
 :group 'python-mode)

(defcustom py-tab-indent t
  "Non-nil means TAB in Python mode calls ‘py-indent-line’."
  :type 'boolean
  :tag "py-tab-indent"
  :group 'python-mode)

(defcustom py-return-key 'py-newline-and-indent
  "Which command <return> should call."
  :type '(choice

          (const :tag "default" py-newline-and-indent)
          (const :tag "newline" newline)
          (const :tag "py-newline-and-indent" py-newline-and-indent)
          (const :tag "py-newline-and-dedent" py-newline-and-dedent)
          )
  :tag "py-return-key"
  :group 'python-mode)

(defcustom py-complete-function 'py-fast-complete
  "When set, enforces function todo completion, default is ‘py-fast-complete’.

Might not affect IPython, as ‘py-shell-complete’ is the only known working here.
Normally ‘python-mode’ knows best which function to use."
  :type '(choice

          (const :tag "default" nil)
          (const :tag "Pymacs and company based py-complete" py-complete)
          (const :tag "py-shell-complete" py-shell-complete)
          (const :tag "py-indent-or-complete" py-indent-or-complete)
	  (const :tag "py-fast-complete" py-fast-complete)
          )
  :tag "py-complete-function"
  :group 'python-mode)

(defcustom py-encoding-string " # -*- coding: utf-8 -*-"
  "Default string specifying encoding of a Python file."
  :type 'string
  :tag "py-encoding-string"
  :group 'python-mode)

(defcustom py-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file."
  :type 'string
  :tag "py-shebang-startstring"
  :group 'python-mode)

(defcustom py-flake8-command ""
  "Which command to call flake8.

If empty, ‘python-mode’ will guess some"
  :type 'string
  :tag "py-flake8-command"
  :group 'python-mode)

(defcustom py-flake8-command-args ""
  "Arguments used by flake8.

Default is the empty string."
  :type 'string
  :tag "py-flake8-command-args"
  :group 'python-mode)

(defvar py-flake8-history nil
  "Used by flake8, resp. `py-flake8-command'.

Default is nil.")

(defcustom py-message-executing-temporary-file t
  "If execute functions using a temporary file should message it.

Default is t.
Messaging increments the prompt counter of IPython shell."
  :type 'boolean
  :tag "py-message-executing-temporary-file"
  :group 'python-mode)

(defcustom py-execute-no-temp-p nil
  "Seems Emacs-24.3 provided a way executing stuff without temporary files."
  :type 'boolean
  :tag "py-execute-no-temp-p"
  :group 'python-mode)

(defcustom py-lhs-inbound-indent 1
  "When line starts a multiline-assignment.

How many colums indent more than opening bracket, brace or parenthesis."
  :type 'integer
  :tag "py-lhs-inbound-indent"
  :group 'python-mode)

(defcustom py-continuation-offset 2
  "Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line."
  :type 'integer
  :tag "py-continuation-offset"
  :group 'python-mode)

(defcustom py-indent-tabs-mode nil
  "Python-mode starts ‘indent-tabs-mode’ with the value specified here.

Default is nil."
  :type 'boolean
  :tag "py-indent-tabs-mode"
  :group 'python-mode)

(defcustom py-smart-indentation nil
  "Guess ‘py-indent-offset’.  Default is nil.

Setting it to t seems useful only in cases where customizing
‘py-indent-offset’ is no option - for example because the
indentation step is unknown or differs inside the code.

When this variable is non-nil, ‘py-indent-offset’ is guessed from existing code.

Which might slow down the proceeding."

  :type 'boolean
  :tag "py-smart-indentation"
  :group 'python-mode)

(defcustom py-block-comment-prefix "##"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where ‘x’ is not a blank or a tab, and
 `...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :tag "py-block-comment-prefix"
  :group 'python-mode)

(defcustom py-indent-offset 4
  "Amount of offset per level of indentation.
`\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :tag "py-indent-offset"
  :group 'python-mode)
(make-variable-buffer-local 'py-indent-offset)

(defcustom py-backslashed-lines-indent-offset 5
  "Amount of offset per level of indentation of backslashed.
No semantic indent,  which diff to ‘py-indent-offset’ indicates"
  :type 'integer
  :tag "py-backslashed-lines-indent-offset"
  :group 'python-mode)

(defcustom py-shell-completion-native-output-timeout 5.0
  "Time in seconds to wait for completion output before giving up."
  :version "25.1"
  :type 'float
  :tag "py-shell-completion-native-output-timeout"
  :group 'python-mode)

(defcustom py-shell-completion-native-try-output-timeout 1.0
  "Time in seconds to wait for *trying* native completion output."
  :version "25.1"
  :type 'float
  :tag "py-shell-completion-native-try-output-timeout"
  :group 'python-mode)

(defvar py-shell--first-prompt-received-output-buffer nil)
(defvar py-shell--first-prompt-received nil)

(defcustom py-shell-first-prompt-hook nil
  "Hook run upon first (non-pdb) shell prompt detection.
This is the place for shell setup functions that need to wait for
output.  Since the first prompt is ensured, this helps the
current process to not hang while waiting.  This is useful to
safely attach setup code for long-running processes that
eventually provide a shell."
  :version "25.1"
  :type 'hook
  :tag "py-shell-first-prompt-hook"
  :group 'python-mode)

(defvar py-shell--parent-buffer nil)

(defvar py-shell--package-depth 10)

(defcustom py-indent-comments t
  "When t, comment lines are indented."
  :type 'boolean
  :tag "py-indent-comments"
  :group 'python-mode)

(defcustom py-uncomment-indents-p nil
  "When non-nil, after uncomment indent lines."
  :type 'boolean
  :tag "py-uncomment-indents-p"
  :group 'python-mode)

(defcustom py-separator-char "/"
  "The character, which separates the system file-path components.

Precedes guessing when not empty, returned by function ‘py-separator-char’."
  :type 'string
  :tag "py-separator-char"
  :group 'python-mode)

(defvar py-separator-char "/"
  "Values set by defcustom only will not be seen in batch-mode.")

(and
 ;; used as a string finally
 ;; kept a character not to break existing customizations
 (characterp py-separator-char)(setq py-separator-char (char-to-string py-separator-char)))

(defcustom py-custom-temp-directory ""
  "If set, will take precedence over guessed values from ‘py-temp-directory’.

Default is the empty string."
  :type 'string
  :tag "py-custom-temp-directory"
  :group 'python-mode)

(defcustom py-beep-if-tab-change t
  "Ring the bell if ‘tab-width’ is changed.
If a comment of the form

                           \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) ‘tab-width’ does not
equal <number>, ‘tab-width’ is set to <number>, a message saying so is
displayed in the echo area, and if ‘py-beep-if-tab-change’ is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
  :tag "py-beep-if-tab-change"
  :group 'python-mode)

(defcustom py-jump-on-exception t
  "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
  :tag "py-jump-on-exception"
  :group 'python-mode)

(defcustom py-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :tag "py-ask-about-save"
  :group 'python-mode)

(defcustom py-delete-function 'delete-char
  "Function called by ‘py-electric-delete’ when deleting forwards."
  :type 'function
  :tag "py-delete-function"
  :group 'python-mode)

(defcustom py-import-check-point-max
  20000
  "Max number of characters to search Java-ish import statement.

When ‘python-mode’ tries to calculate the shell
-- either a CPython or a Jython shell --
it looks at the so-called ‘shebang’.
If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
  :tag "py-import-check-point-max
"
  :group 'python-mode)

;; (setq py-shells
;; (list
;; ""
;; 'ipython
;; 'ipython2.7
;; 'ipython3
;; 'jython
;; 'python
;; 'python2
;; 'python3
;; 'pypy
;; ))

(defcustom py-known-shells
  (list
   "ipython"
   "ipython2.7"
   "ipython3"
   "jython"
   "python"
   "python2"
   "python3"
   "pypy"
   )
  "A list of available shells instrumented for commands.
Expects its executables installed

Edit for your needs."
  :type '(repeat string)
  :tag "py-shells"
  :group 'python-mode)

(defcustom py-known-shells-extended-commands
  (list "ipython"
	"python"
	"python3"
	"pypy"
	)
  "A list of shells for finer grained commands.
like ‘py-execute-statement-ipython’
Expects its executables installed

Edit for your needs."
  :type '(repeat string)
  :tag "py-shells"
  :group 'python-mode)

(defun py-install-named-shells-fix-doc (ele)
  "Internally used by ‘py-load-named-shells’.

Argument ELE: a shell name, a string."
  (cond ((string-match "^i" ele)
	 (concat "I" (capitalize (substring ele 1))))
	((string-match "^pypy" ele)
	 "PyPy")
	(t (capitalize ele))))

(defcustom py-jython-packages
  '("java" "javax")
  "Imported packages that imply ‘jython-mode’."
  :type '(repeat string)
  :tag "py-jython-packages
"
  :group 'python-mode)

(defcustom py-current-defun-show t
  "If ‘py-current-defun’ should jump to the definition.

Highlights it while waiting PY-WHICH-FUNC-DELAY seconds.
Afterwards returning to previous position.

Default is t."

  :type 'boolean
  :tag "py-current-defun-show"
  :group 'python-mode)

(defcustom py-current-defun-delay 2
  "‘py-current-defun’ waits PY-WHICH-FUNC-DELAY seconds.

Before returning to previous position."

  :type 'number
  :tag "py-current-defun-delay"
  :group 'python-mode)

(defcustom py-python-send-delay 1
  "Seconds to wait for output, used by `py--send-...' functions.

See also ‘py-ipython-send-delay’"

  :type 'number
  :tag "py-python-send-delay"
  :group 'python-mode)

(defcustom py-python3-send-delay 1
  "Seconds to wait for output, used by `py--send-...' functions.

See also ‘py-ipython-send-delay’"

  :type 'number
  :tag "py-python3-send-delay"
  :group 'python-mode)

(defcustom py-ipython-send-delay 1
  "Seconds to wait for output, used by `py--send-...' functions.

See also ‘py-python-send-delay’"

  :type 'number
  :tag "py-ipython-send-delay"
  :group 'python-mode)

(defcustom py-master-file nil
  "Execute the named master file instead of the buffer's file.

Default is nil.
With relative path variable ‘default-directory’ is prepended.

Beside you may set this variable in the file's local
variable section, e.g.:

                           # Local Variables:
                           # py-master-file: \"master.py\"
                           # End:"
  :type 'string
  :tag "py-master-file"
  :group 'python-mode)
(make-variable-buffer-local 'py-master-file)

(defcustom py-pychecker-command "pychecker"
  "Shell command used to run Pychecker."
  :type 'string
  :tag "py-pychecker-command"
  :group 'python-mode)

(defcustom py-pychecker-command-args "--stdlib"
  "String arguments to be passed to pychecker."
  :type 'string
  :tag "py-pychecker-command-args"
  :group 'python-mode)

(defcustom py-pyflakes-command "pyflakes"
  "Shell command used to run Pyflakes."
  :type 'string
  :tag "py-pyflakes-command"
  :group 'python-mode)

(defcustom py-pyflakes-command-args ""
  "String arguments to be passed to pyflakes.

Default is \"\""
  :type 'string
  :tag "py-pyflakes-command-args"
  :group 'python-mode)

(defcustom py-pep8-command "pep8"
  "Shell command used to run pep8."
  :type 'string
  :tag "py-pep8-command"
  :group 'python-mode)

(defcustom py-pep8-command-args ""
  "String arguments to be passed to pylint.

Default is \"\""
  :type 'string
  :tag "py-pep8-command-args"
  :group 'python-mode)

(defcustom py-pyflakespep8-command (concat py-install-directory "/pyflakespep8.py")
  "Shell command used to run `pyflakespep8'."
  :type 'string
  :tag "py-pyflakespep8-command"
  :group 'python-mode)

(defcustom py-pyflakespep8-command-args ""
  "String arguments to be passed to pyflakespep8.

Default is \"\""
  :type 'string
  :tag "py-pyflakespep8-command-args"
  :group 'python-mode)

(defcustom py-pylint-command "pylint"
  "Shell command used to run Pylint."
  :type 'string
  :tag "py-pylint-command"
  :group 'python-mode)

(defcustom py-pylint-command-args '("--errors-only")
  "String arguments to be passed to pylint.

Default is \"--errors-only\""
  :type '(repeat string)
  :tag "py-pylint-command-args"
  :group 'python-mode)

(defvar py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ *"
  "Recognize the prompt.")

(defcustom py-shell-input-prompt-1-regexp ">>> "
  "A regular expression to match the input prompt of the shell."
  :type 'regexp
  :tag "py-shell-input-prompt-1-regexp"
  :group 'python-mode)

(defcustom py-shell-input-prompt-2-regexp "[.][.][.]:? "
  "A regular expression to match the input prompt.

Applies to the shell after the first line of input."
  :type 'string
  :tag "py-shell-input-prompt-2-regexp"
  :group 'python-mode)

(defvar py-shell-ipython-input-prompt-1-regexp "In \\[[0-9]+\\]: "
  "Regular Expression matching input prompt of python shell.
It should not contain a caret (^) at the beginning.")

(defvar py-shell-ipython-input-prompt-2-regexp "   \\.\\.\\.: "
  "Regular Expression matching second level input prompt of python shell.
It should not contain a caret (^) at the beginning.")

(defcustom py-shell-input-prompt-2-regexps
  '(">>> " "\\.\\.\\. "                 ; Python
    "In \\[[0-9]+\\]: "                 ; IPython
    "   \\.\\.\\.: "                    ; IPython
    ;; Using ipdb outside IPython may fail to cleanup and leave static
    ;; IPython prompts activated, this adds some safeguard for that.
    "In : " "\\.\\.\\.: ")
  "List of regular expressions matching input prompts."
  :type '(repeat string)
  :version "24.4"
  :tag "py-shell-input-prompt-2-regexps"
  :group 'python-mode)

(defcustom py-shell-input-prompt-regexps
  '(">>> " "\\.\\.\\. "                 ; Python
    "In \\[[0-9]+\\]: "                 ; IPython
    "   \\.\\.\\.: "                    ; IPython
    ;; Using ipdb outside IPython may fail to cleanup and leave static
    ;; IPython prompts activated, this adds some safeguard for that.
    "In : " "\\.\\.\\.: ")
  "List of regular expressions matching input prompts."
  :type '(repeat regexp)
  :version "24.4"
  :tag "py-shell-input-prompt-regexps"
  :group 'python-mode)

(defvar py-ipython-output-prompt-re "^Out\\[[0-9]+\\]: "
  "A regular expression to match the output prompt of IPython.")

(defcustom py-shell-output-prompt-regexps
  '(""                                  ; Python
    "Out\\[[0-9]+\\]: "                 ; IPython
    "Out :")                            ; ipdb safeguard
  "List of regular expressions matching output prompts."
  :type '(repeat string)
  :version "24.4"
  :tag "py-shell-output-prompt-regexps"
  :group 'python-mode)

(defvar py-pydbtrack-input-prompt "^[(]*ipydb[>)]+ "
  "Recognize the pydb-prompt.")
;; (setq py-pdbtrack-input-prompt "^[(< \t]*[Ii]?[Pp]y?db[>)]*.*")

(defvar py-ipython-input-prompt-re "In \\[?[0-9 ]*\\]?: *\\|^[ ]\\{3\\}[.]\\{3,\\}: *"
  "A regular expression to match the IPython input prompt.")

(defvar py-shell-prompt-regexp
  (concat "\\("
	  (mapconcat 'identity
		     (delq nil
			   (list
			    py-shell-input-prompt-1-regexp
			    py-shell-input-prompt-2-regexp
			    py-ipython-input-prompt-re
			    py-ipython-output-prompt-re
			    py-pdbtrack-input-prompt
			    py-pydbtrack-input-prompt
			    "[.]\\{3,\\}:? *"
			    ))
		     "\\|")
	  "\\)")
  "Internally used by ‘py-fast-filter’.
‘ansi-color-filter-apply’ might return
Result: \"\\nIn [10]:    ....:    ....:    ....: 1\\n\\nIn [11]: \"")

(defvar py-fast-filter-re
  (concat "\\("
	  (mapconcat 'identity
		     (delq nil
			   (list
			    py-shell-input-prompt-1-regexp
			    py-shell-input-prompt-2-regexp
			    py-ipython-input-prompt-re
			    py-ipython-output-prompt-re
			    py-pdbtrack-input-prompt
			    py-pydbtrack-input-prompt
			    "[.]\\{3,\\}:? *"
			    ))
		     "\\|")
	  "\\)")
  "Internally used by ‘py-fast-filter’.
‘ansi-color-filter-apply’ might return
Result: \"\\nIn [10]:    ....:    ....:    ....: 1\\n\\nIn [11]: \"")

(defcustom py-shell-prompt-detect-p nil
  "Non-nil enables autodetection of interpreter prompts."
  :type 'boolean
  :safe 'booleanp
  :version "24.4"
  :tag "py-shell-prompt-detect-p"
  :group 'python-mode)

(defcustom py-shell-prompt-read-only t
  "If non-nil, the python prompt is read only.

Setting this variable will only effect new shells."
  :type 'boolean
  :tag "py-shell-prompt-read-only"
  :group 'python-mode)

(setq py-fast-filter-re
  (concat "\\("
	  (mapconcat 'identity
		     (delq nil
			   (list
			    py-shell-input-prompt-1-regexp
			    py-shell-input-prompt-2-regexp
			    py-ipython-input-prompt-re
			    py-ipython-output-prompt-re
			    py-pdbtrack-input-prompt
			    py-pydbtrack-input-prompt
			    "[.]\\{3,\\}:? *"
			    ))
		     "\\|")
	  "\\)"))

(defcustom py-honor-IPYTHONDIR-p nil
  "When non-nil ipython-history file is constructed by $IPYTHONDIR.

Default is nil.
Otherwise value of ‘py-ipython-history’ is used."
  :type 'boolean
  :tag "py-honor-IPYTHONDIR-p"
  :group 'python-mode)

(defcustom py-ipython-history "~/.ipython/history"
  "Ipython-history default file.

Used when `py-honor-IPYTHONDIR-p' is nil - th default"

  :type 'string
  :tag "py-ipython-history"
  :group 'python-mode)

(defcustom py-honor-PYTHONHISTORY-p nil
  "When non-nil python-history file is set by $PYTHONHISTORY.

Default is nil.
Otherwise value of ‘py-python-history’ is used."
  :type 'boolean
  :tag "py-honor-PYTHONHISTORY-p"
  :group 'python-mode)

(defcustom py-python-history "~/.python_history"
  "Python-history default file.

Used when `py-honor-PYTHONHISTORY-p' is nil (default)."

  :type 'string
  :tag "py-python-history"
  :group 'python-mode)

(defcustom py-switch-buffers-on-execute-p nil
  "When non-nil switch to the Python output buffer.

If ‘py-keep-windows-configuration’ is t, this will take precedence
over setting here."

  :type 'boolean
  :tag "py-switch-buffers-on-execute-p"
  :group 'python-mode)
;; made buffer-local as pdb might need t in all circumstances
(make-variable-buffer-local 'py-switch-buffers-on-execute-p)

(defcustom py-split-window-on-execute 'just-two
  "When non-nil split windows.

Default is just-two - when code is send to interpreter.
Splits screen into source-code buffer and current ‘py-shell’ result.
Other buffer will be hidden that way.

When set to t, ‘python-mode’ tries to reuse existing windows
and will split only if needed.

With \\='always, results will displayed in a new window.

Both t and ‘always’ is experimental still.

For the moment: If a multitude of python-shells/buffers should be
visible, open them manually and set ‘py-keep-windows-configuration’ to t.

See also ‘py-keep-windows-configuration’"
  :type `(choice
          (const :tag "default" just-two)
	  (const :tag "reuse" t)
          (const :tag "no split" nil)
	  (const :tag "just-two" just-two)
          (const :tag "always" always))
  :tag "py-split-window-on-execute"
  :group 'python-mode)

;; (defun py-toggle-py-split-window-on-execute ()
;;   "Toggle between customized value and nil."
;;   (interactive)
;;   (setq py-split-window-on-execute (not py-split-window-on-execute))
;;   (when (called-interactively-p 'interactive)
;;     (message "py-split-window-on-execute: %s" py-split-window-on-execute)
;;     py-split-window-on-execute))

(defcustom py-split-window-on-execute-threshold 3
  "Maximal number of displayed windows.

Honored, when ‘py-split-window-on-execute’ is t, i.e. \"reuse\".
Don't split when max number of displayed windows is reached."
  :type 'number
  :tag "py-split-window-on-execute-threshold"
  :group 'python-mode)

(defcustom py-split-windows-on-execute-function 'split-window-vertically
  "How window should get splitted to display results of py-execute-... functions."
  :type '(choice (const :tag "split-window-vertically" split-window-vertically)
                 (const :tag "split-window-horizontally" split-window-horizontally)
                 )
  :tag "py-split-windows-on-execute-function"
  :group 'python-mode)

(defcustom py-shell-fontify-p 'input
  "Fontify current input in Python shell. Default is input.

INPUT will leave output unfontified.

At any case only current input gets fontified."
  :type '(choice (const :tag "Default" all)
                 (const :tag "Input" input)
		 (const :tag "Nil" nil)
                 )
  :tag "py-shell-fontify-p"
  :group 'python-mode)

(defcustom py-hide-show-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with"     "match"  "case")
  "Keywords composing visible heads."
  :type '(repeat string)
  :tag "py-hide-show-keywords
"
  :group 'python-mode)

(defcustom py-hide-show-hide-docstrings t
  "Controls if doc strings can be hidden by hide-show."
  :type 'boolean
  :tag "py-hide-show-hide-docstrings"
  :group 'python-mode)

(defcustom py-hide-comments-when-hiding-all t
  "Hide the comments too when you do an ‘hs-hide-all’."
  :type 'boolean
  :tag "py-hide-comments-when-hiding-all"
  :group 'python-mode)

(defcustom py-outline-mode-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with"     "match"  "case")
  "Keywords composing visible heads."
  :type '(repeat string)
  :tag "py-outline-mode-keywords
"
  :group 'python-mode)

(defcustom python-mode-hook nil
  "Hook run when entering Python mode."

  :type 'hook
  :tag "python-mode-hook"
  :group 'python-mode
  )

;; (defcustom py-shell-name
;;   (if (eq system-type 'windows-nt)
;;       "C:/Python27/python"
;;     "python")

;;   "A PATH/TO/EXECUTABLE or default value ‘py-shell’ may look for.

;; If no shell is specified by command.

;; On Windows default is C:/Python27/python
;; --there is no garantee it exists, please check your system--

;; Else python"
;;   :type 'string
;;   :tag "py-shell-name
;; "
;;   :group 'python-mode)

(defcustom py-python-command
  (if (eq system-type 'windows-nt)
      ;; "C:\\Python27\\python.exe"
      "python"
   ;; "C:/Python33/Lib/site-packages/IPython"
    "python")

  "Make sure directory in in the PATH-variable.

Windows: edit in \"Advanced System Settings/Environment Variables\"
Commonly \"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
  :tag "py-python-command
"
  :group 'python-mode)

(defvar py-shell-name py-python-command)
;; (defvaralias 'py-shell-name 'py-python-command)

(defcustom py-python-command-args '("-i")
  "String arguments to be used when starting a Python shell."
  :type '(repeat string)
  :tag "py-python-command-args"
  :group 'python-mode)

(defcustom py-python2-command
  (if (eq system-type 'windows-nt)
      "C:\\Python27\\python"
    ;; "python2"
    "python2")

  "Make sure, the directory where python.exe resides in in the PATH-variable.

Windows: If needed, edit in
\"Advanced System Settings/Environment Variables\"
Commonly
\"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
  :tag "py-python2-command
"
  :group 'python-mode)

(defcustom py-python2-command-args '("-i")
  "String arguments to be used when starting a Python shell."
  :type '(repeat string)
  :tag "py-python2-command-args"
  :group 'python-mode)

;; "/usr/bin/python3"
(defcustom py-python3-command
  (if (eq system-type 'windows-nt)
    "C:/Python33/python"
    "python3")

  "A PATH/TO/EXECUTABLE or default value ‘py-shell’ may look for.

Unless shell is specified by command.

On Windows see C:/Python3/python.exe
--there is no garantee it exists, please check your system--

At GNU systems see /usr/bin/python3"

  :type 'string
  :tag "py-python3-command
"
  :group 'python-mode)

(defcustom py-python3-command-args '("-i")
  "String arguments to be used when starting a Python3 shell."
  :type '(repeat string)
  :tag "py-python3-command-args"
  :group 'python-mode)

(defcustom py-ipython-command
  (if (eq system-type 'windows-nt)
      ;; "ipython"
    "C:\\Python27\\python"
    ;; "C:/Python33/Lib/site-packages/IPython"
    ;; "/usr/bin/ipython"
    "ipython")

  "A PATH/TO/EXECUTABLE or default value.

`M-x IPython RET' may look for,
Unless IPython-shell is specified by command.

On Windows default is \"C:\\\\Python27\\\\python.exe\"
While with Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython.exe\"

Else /usr/bin/ipython"

  :type 'string
  :tag "py-ipython-command
"
  :group 'python-mode)

(defcustom py-ipython-command-args
  (if (eq system-type 'windows-nt)
      '("-i" "C:\\Python27\\Scripts\\ipython-script.py")
    ;; --simple-prompt seems to exist from IPython 5.
    (if (string-match "^[0-4]" (shell-command-to-string (concat "ipython" " -V")))
	'("--pylab" "--automagic")
      '("--pylab" "--automagic" "--simple-prompt")))
  "String arguments to be used when starting a IPython shell.

At Windows make sure ipython-script.py is PATH.
Also setting PATH/TO/SCRIPT here should work, for example;
C:\\Python27\\Scripts\\ipython-script.py
With Anaconda the following is known to work:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython-script-py\""
  :type '(repeat string)
  :tag "py-ipython-command-args"
  :group 'python-mode)

(defcustom py-jython-command
  (if (eq system-type 'windows-nt)
      '("jython")
    '("/usr/bin/jython"))

  "A PATH/TO/EXECUTABLE or default value.
`M-x Jython RET' may look for, if no Jython-shell is specified by command.

Not known to work at windows
Default /usr/bin/jython"

  :type '(repeat string)
  :tag "py-jython-command
"
  :group 'python-mode)

(defcustom py-jython-command-args '("-i")
  "String arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :tag "py-jython-command-args"
  :group 'python-mode)

(defcustom py-shell-toggle-1 py-python2-command
  "A PATH/TO/EXECUTABLE or default value used by ‘py-toggle-shell’."
  :type 'string
  :tag "py-shell-toggle-1"
  :group 'python-mode)

(defcustom py-shell-toggle-2 py-python3-command
  "A PATH/TO/EXECUTABLE or default value used by ‘py-toggle-shell’."
  :type 'string
  :tag "py-shell-toggle-2"
  :group 'python-mode)

(defcustom py--imenu-create-index-p nil
  "Non-nil means Python mode creates and displays an index menu.

Of functions and global variables."
  :type 'boolean
  :tag "py--imenu-create-index-p"
  :group 'python-mode)

(defvar py-history-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'\\|'''/tmp/"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters.")

(defcustom py-match-paren-mode nil
  "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets ‘py-match-paren-key’ in ‘python-mode-map’.
Customize ‘py-match-paren-key’ which key to use."
  :type 'boolean
  :tag "py-match-paren-mode"
  :group 'python-mode)

(defcustom py-match-paren-key "%"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where ‘x’ is not a blank or a tab, and
                               `...' is arbitrary).
However, this string should not end in whitespace."
  :type 'string
  :tag "py-match-paren-key"
  :group 'python-mode)

(defcustom py-kill-empty-line t
  "If t, ‘py-indent-forward-line’ kills empty lines."
  :type 'boolean
  :tag "py-kill-empty-line"
  :group 'python-mode)

(defcustom py-imenu-show-method-args-p nil
  "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
  :tag "py-imenu-show-method-args-p"
  :group 'python-mode)

(defcustom py-use-local-default nil
  "If t, ‘py-shell’ will use ‘py-shell-local-path’.

Alternative to default Python.

Making switch between several virtualenv's easier,‘python-mode’ should
deliver an installer, named-shells pointing to virtualenv's will be available."
  :type 'boolean
  :tag "py-use-local-default"
  :group 'python-mode)

(defcustom py-edit-only-p nil
  "Don't check for installed Python executables.

Default is nil.

See bug report at launchpad, lp:944093."
  :type 'boolean
  :tag "py-edit-only-p"
  :group 'python-mode)

(defcustom py-force-py-shell-name-p nil
  "When t, execution specified in ‘py-shell-name’ is enforced.

Possibly shebang doesn't take precedence."

  :type 'boolean
  :tag "py-force-py-shell-name-p"
  :group 'python-mode)

(defcustom python-mode-v5-behavior-p nil
  "Execute region through ‘shell-command-on-region’.

As v5 did it - lp:990079.
This might fail with certain chars - see UnicodeEncodeError lp:550661"

  :type 'boolean
  :tag "python-mode-v5-behavior-p"
  :group 'python-mode)

(defun py-toggle-python-mode-v5-behavior ()
  "Switch the values of `python-mode-v5-behavior-p'."
  (interactive)
  (setq python-mode-v5-behavior-p (not python-mode-v5-behavior-p))
  (when (called-interactively-p 'interactive)
    (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p)))

(defun py-toggle-py-verbose-p ()
  "Switch the values of ‘py-verbose-p’.

Default is nil.
If on, messages value of ‘py-result’ for instance."
  (interactive)
  (setq py-verbose-p (not py-verbose-p))
  (when (called-interactively-p 'interactive)
    (message "py-verbose-p: %s" py-verbose-p)))

(defcustom py-trailing-whitespace-smart-delete-p nil
  "Default is nil.

When t, ‘python-mode’ calls
\(add-hook \\='before-save-hook \\='delete-trailing-whitespace nil \\='local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected"
  :type 'boolean
  :tag "py-trailing-whitespace-smart-delete-p"
  :group 'python-mode)

(defcustom py-newline-delete-trailing-whitespace-p t
  "Delete trailing whitespace maybe left by ‘py-newline-and-indent’.

Default is t. See lp:1100892"
  :type 'boolean
  :tag "py-newline-delete-trailing-whitespace-p"
  :group 'python-mode)

(defcustom py--warn-tmp-files-left-p nil
  "Warn, when ‘py-temp-directory’ contains files susceptible being left.

WRT previous Python-mode sessions. See also lp:987534."
  :type 'boolean
  :tag "py--warn-tmp-files-left-p"
  :group 'python-mode)

(defcustom py-complete-ac-sources '(ac-source-pycomplete)
  "List of ‘auto-complete’ sources assigned to ‘ac-sources’.

In ‘py-complete-initialize’.

Default is known to work an Ubuntu 14.10 - having python-
mode, pymacs and auto-complete-el, with the following minimal
Emacs initialization:

\(require \\='pymacs)
\(require \\='auto-complete-config)
\(ac-config-default)"
  :type 'hook
  :tag "py-complete-ac-sources"
  :options '(ac-source-pycomplete ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)
  :group 'python-mode)

(defcustom py-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
  :type 'boolean
  :tag "py-remove-cwd-from-path"
  :group 'python-mode)

(defcustom py-shell-local-path ""
  "‘py-shell’ will use EXECUTABLE indicated here incl. path.

If ‘py-use-local-default’ is non-nil."

  :type 'string
  :tag "py-shell-local-path"
  :group 'python-mode)

(defcustom py-python-edit-version ""
  "When not empty, fontify according to Python version specified.

Default is the empty string, a useful value \"python3\" maybe.

When empty, version is guessed via ‘py-choose-shell’."

  :type 'string
  :tag "py-python-edit-version"
  :group 'python-mode)

(defcustom py-ipython-execute-delay 0.3
  "Delay needed by execute functions when no IPython shell is running."
  :type 'float
  :tag "py-ipython-execute-delay"
  :group 'python-mode)

(defvar py-shell-completion-setup-code
  "try:
    import readline
except ImportError:
    def __COMPLETER_all_completions(text): []
else:
    import rlcompleter
    readline.set_completer(rlcompleter.Completer().complete)
    def __COMPLETER_all_completions(text):
        import sys
        completions = []
        try:
            i = 0
            while True:
                res = readline.get_completer()(text, i)
                if not res: break
                i += 1
                completions.append(res)
        except NameError:
            pass
        return completions"
  "Code used to setup completion in Python processes.")

(defvar py-shell-module-completion-code "';'.join(__COMPLETER_all_completions('''%s'''))"
  "Python code used to get completions separated by semicolons for imports.")

(defvar py-ipython-module-completion-code
  "import IPython
version = IPython.__version__
if \'0.10\' < version:
    from IPython.core.completerlib import module_completion
"
  "For IPython v0.11 or greater.
Use the following as the value of this variable:

';'.join(module_completion('''%s'''))")

(defvar py-ipython-module-completion-string
  "';'.join(module_completion('''%s'''))"
  "See also ‘py-ipython-module-completion-code’.")

(defcustom py--imenu-create-index-function 'py--imenu-index
  "Switch between ‘py--imenu-create-index-new’  and series 5. index-machine."
  :type '(choice
	  (const :tag "'py--imenu-create-index-new, also lists modules variables " py--imenu-create-index-new)

	  (const :tag "py--imenu-create-index, series 5. index-machine" py--imenu-create-index)
	  (const :tag "py--imenu-index, honor type annotations" py--imenu-index)

	  )
  :tag "py--imenu-create-index-function"
  :group 'python-mode)

(defvar py-line-re "^"
  "Used by generated functions." )

(defvar py-input-filter-re "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters.")

(defvar strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING.

See also ‘string-chars-preserve’")

(defvar strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING.

See also ‘string-chars-preserve’")

(defcustom py-docstring-style 'pep-257-nn
  "Implemented styles:

 are DJANGO, ONETWO, PEP-257, PEP-257-NN,SYMMETRIC, and NIL.

A value of NIL won't care about quotes
position and will treat docstrings a normal string, any other
value may result in one of the following docstring styles:

DJANGO:

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

ONETWO:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257-NN:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

SYMMETRIC:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\""
  :type '(choice

          (const :tag "Don't format docstrings" nil)
          (const :tag "Django's coding standards style." django)
          (const :tag "One newline and start and Two at end style." onetwo)
          (const :tag "PEP-257 with 2 newlines at end of string." pep-257)
          (const :tag "PEP-257-nn with 1 newline at end of string." pep-257-nn)
          (const :tag "Symmetric style." symmetric))
  :tag "py-docstring-style"
  :group 'python-mode)

(defcustom py-execute-directory nil
  "Stores the file's default directory-name py-execute-... functions act upon.

Used by Python-shell for output of ‘py-execute-buffer’ and related commands.
See also ‘py-use-current-dir-when-execute-p’"
  :type 'string
  :tag "py-execute-directory"
  :group 'python-mode)

(defcustom py-use-current-dir-when-execute-p t
  "Current directory used for output.

See also ‘py-execute-directory’"
  :type 'boolean
  :tag "py-use-current-dir-when-execute-p"
  :group 'python-mode)

(defcustom py-keep-shell-dir-when-execute-p nil
  "Don't change Python shell's current working directory when sending code.

See also ‘py-execute-directory’"
  :type 'boolean
  :tag "py-keep-shell-dir-when-execute-p"
  :group 'python-mode)

(defcustom py-fileless-buffer-use-default-directory-p t
  "‘default-directory’ sets current working directory of Python output shell.

When ‘py-use-current-dir-when-execute-p’ is non-nil and no buffer-file exists."
  :type 'boolean
  :tag "py-fileless-buffer-use-default-directory-p"
  :group 'python-mode)

(defcustom py-check-command "pychecker --stdlib"
  "Command used to check a Python file."
  :type 'string
  :tag "py-check-command"
  :group 'python-mode)

;; (defvar py-this-abbrevs-changed nil
;;   "Internally used by ‘python-mode-hook’.")

(defvar py-buffer-name nil
  "Internal use.

The buffer last output was sent to.")

(defvar py-orig-buffer-or-file nil
  "Internal use.")

(defcustom py-keep-windows-configuration nil
  "Takes precedence over:

 ‘py-split-window-on-execute’ and ‘py-switch-buffers-on-execute-p’.
See lp:1239498

To suppres window-changes due to error-signaling also.
Set ‘py-keep-windows-configuration’ onto \\'force

Default is nil"

  :type '(choice
          (const :tag "nil" nil)
          (const :tag "t" t)
          (const :tag "force" force))
  :tag "py-keep-windows-configuration"
  :group 'python-mode)

(defvar py-output-buffer "*Python Output*"
      "Used if `python-mode-v5-behavior-p' is t.

Otherwise output buffer is created dynamically according to version process.")

(defcustom py-force-default-output-buffer-p nil
  "Enforce sending output to the default output ‘buffer-name’.

Set by defvar ‘py-output-buffer’
Bug #31 - wrong fontification caused by string-delimiters in output"

  :type 'boolean
  :tag "py-force-default-output-buffer-p"
  :group 'python-mode)

(defcustom py-shell-unbuffered t
  "Should shell output be unbuffered?.
When non-nil, this may prevent delayed and missing output in the
Python shell.  See commentary for details."
  :type 'boolean
  :safe 'booleanp
  :tag "py-shell-unbuffered"
  :group 'python-mode)

(defcustom py-shell-process-environment nil
  "List of overridden environment variables for subprocesses to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.
When this variable is non-nil, values are exported into the
process environment before starting it.  Any variables already
present in the current environment are superseded by variables
set here."
  :type '(repeat string)
  :tag "py-shell-process-environment"
  :group 'python-mode)

(defcustom py-shell-extra-pythonpaths nil
  "List of extra pythonpaths for Python shell.
When this variable is non-nil, values added at the beginning of
the PYTHONPATH before starting processes.  Any values present
here that already exists in PYTHONPATH are moved to the beginning
of the list so that they are prioritized when looking for
modules."
  :type '(repeat string)
  :tag "py-shell-extra-pythonpaths"
  :group 'python-mode)

(defcustom py-shell-exec-path nil
  "List of paths for searching executables.
When this variable is non-nil, values added at the beginning of
the PATH before starting processes.  Any values present here that
already exists in PATH are moved to the beginning of the list so
that they are prioritized when looking for executables."
  :type '(repeat string)
  :tag "py-shell-exec-path"
  :group 'python-mode)

(defcustom py-shell-remote-exec-path nil
  "List of paths to be ensured remotely for searching executables.
When this variable is non-nil, values are exported into remote
hosts PATH before starting processes.  Values defined in
‘py-shell-exec-path’ will take precedence to paths defined
here.  Normally you wont use this variable directly unless you
plan to ensure a particular set of paths to all Python shell
executed through tramp connections."
  :version "25.1"
  :type '(repeat string)
  :tag "py-shell-remote-exec-path"
  :group 'python-mode)

(defcustom py-shell-virtualenv-root nil
  "Path to virtualenv root.
This variable, when set to a string, makes the environment to be
modified such that shells are started within the specified
virtualenv."
  :type '(choice (const nil) string)
  :tag "py-shell-virtualenv-root"
  :group 'python-mode)

(defvar py-shell-completion-native-redirect-buffer
  " *Py completions redirect*"
  "Buffer to be used to redirect output of readline commands.")

(defvar py-shell--block-prompt nil
  "Input block prompt for inferior python shell.
Do not set this variable directly, instead use
‘py-shell-prompt-set-calculated-regexps’.")

(defvar py-shell-output-filter-in-progress nil)
(defvar py-shell-output-filter-buffer nil)

(defvar py-shell--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior python shell.
Do not set this variable directly.

Iff ‘py-shell--prompt-calculated-input-regexp’
or ‘py-shell--prompt-calculated-output-regexp’ are set
‘py-shell-prompt-set-calculated-regexps’ isn't run.")

(defvar py-shell--prompt-calculated-output-regexp nil
  "Calculated output prompt regexp for inferior python shell.

‘py-shell-prompt-set-calculated-regexps’
Do not set this variable directly.

Iff ‘py-shell--prompt-calculated-input-regexp’
or ‘py-shell--prompt-calculated-output-regexp’ are set
‘py-shell-prompt-set-calculated-regexps’ isn't run.")

(defvar py-shell-prompt-output-regexp ""
  "See ‘py-shell-prompt-output-regexps’.")

(defvar py-shell-prompt-output-regexps
  '(""                                  ; Python
    "Out\\[[0-9]+\\]: "                 ; IPython
    "Out :")                            ; ipdb safeguard
  "List of regular expressions matching output prompts.")

(defvar py-underscore-word-syntax-p t
  "This is set later by defcustom, only initial value here.

If underscore chars should be of ‘syntax-class’ ‘word’, not of ‘symbol’.
Underscores in word-class makes ‘forward-word’.
Travels the indentifiers. Default is t.
See also command ‘py-toggle-underscore-word-syntax-p’")

(defvar py-autofill-timer nil)
(defvar py-fill-column-orig fill-column
  "Used to reset fill-column")

;; defvared value isn't updated maybe
(defvar python-mode-message-string
  (if (or (string= "python-mode.el" (buffer-name))
	  (ignore-errors (string-match "python-mode.el" (py--buffer-filename-remote-maybe))))
      "python-mode.el"
    "python-components-mode")
  "Internally used. Reports the ‘python-mode’ branch.")

;; defvared value isn't updated maybe
(setq python-mode-message-string
  (if (or (string= "python-mode.el" (buffer-name))
	  (ignore-errors (string-match "python-mode.el" (py--buffer-filename-remote-maybe))))
      "python-mode.el"
    "python-components-mode"))

(defun py-escaped-p (&optional pos)
  "Return t if char at POS is preceded by an odd number of backslashes. "
  (save-excursion
    (when pos (goto-char pos))
    (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defvar python-mode-syntax-table nil
  "Give punctuation syntax to ASCII that normally has symbol.

Syntax or has word syntax and isn't a letter.")

(setq python-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Give punctuation syntax to ASCII that normally has symbol
        ;; syntax or has word syntax and isn't a letter.
        (let ((symbol (string-to-syntax "_"))
              (sst (standard-syntax-table)))
          (dotimes (i 128)
            (unless (= i ?_)
              (if (equal symbol (aref sst i))
                  (modify-syntax-entry i "." table)))))
        (modify-syntax-entry ?$ "." table)
        (modify-syntax-entry ?% "." table)
        ;; exceptions
        (modify-syntax-entry ?# "<" table)
        (modify-syntax-entry ?\n ">" table)
        (modify-syntax-entry ?' "\"" table)
        (modify-syntax-entry ?` "$" table)
        (if py-underscore-word-syntax-p
            (modify-syntax-entry ?\_ "w" table)
          (modify-syntax-entry ?\_ "_" table))
        table))

(defvar py-shell-mode-syntax-table nil
  "Set from py-shell")

(defvar py-ipython-completion-command-string nil
  "Select command according to IPython version.

Either `py-ipython0.10-completion-command-string'
or `py-ipython0.11-completion-command-string'.

`py-ipython0.11-completion-command-string' also covers version 0.12")

(defvar py-ipython0.10-completion-command-string
  "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions.")

(defvar py-ipython0.11-completion-command-string
  "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions.")

(defvar py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-"
  "Matches encoding string of a Python file.")

(defvar py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]*\\([biptj]+ython[^ \t\n]*\\)"
  "Detecting the shell in head of file.")

(defvar py-temp-directory
  (let ((ok '(lambda (x)
               (and x
                    (setq x (expand-file-name x)) ; always true
                    (file-directory-p x)
                    (file-writable-p x)
                    x)))
        erg)
    (or
     (and (not (string= "" py-custom-temp-directory))
          (if (funcall ok py-custom-temp-directory)
              (setq erg (expand-file-name py-custom-temp-directory))
            (if (file-directory-p (expand-file-name py-custom-temp-directory))
                (error "Py-custom-temp-directory set but not writable")
              (error "Py-custom-temp-directory not an existing directory"))))
     (and (funcall ok (getenv "TMPDIR"))
          (setq erg (getenv "TMPDIR")))
     (and (funcall ok (getenv "TEMP/TMP"))
          (setq erg (getenv "TEMP/TMP")))
     (and (funcall ok "/usr/tmp")
          (setq erg "/usr/tmp"))
     (and (funcall ok "/tmp")
          (setq erg "/tmp"))
     (and (funcall ok "/var/tmp")
          (setq erg "/var/tmp"))
     (and (eq system-type 'darwin)
          (funcall ok "/var/folders")
          (setq erg "/var/folders"))
     (and (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
          (funcall ok (concat "c:" py-separator-char "Users"))
          (setq erg (concat "c:" py-separator-char "Users")))
     ;; (funcall ok ".")
     (error
      "Couldn't find a usable temp directory -- set ‘py-temp-directory’"))
    (when erg (setq py-temp-directory erg)))
  "Directory used for temporary files created by a *Python* process.
By default, guesses the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory.

 ‘py-custom-temp-directory’ will take precedence when setq")

(defvar py-exec-command nil
  "Internally used.")

(defvar py-which-bufname "Python")

(defvar py-pychecker-history nil)

(defvar py-pyflakes-history nil)

(defvar py-pep8-history nil)

(defvar py-pyflakespep8-history nil)

(defvar py-pylint-history nil)

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")

(defvar hs-hide-comments-when-hiding-all t
  "Defined in hideshow.el, silence compiler warnings here.")

(defvar py-shell-complete-debug nil
  "For interal use when debugging, stores completions." )

(defvar py-debug-p nil
  "Activate extra code for analysis and test purpose when non-nil.

Temporary files are not deleted. Other functions might implement
some logging, etc.
For normal operation, leave it set to nil, its default.
Defined with a defvar form to allow testing the loading of new versions.")

(defun py-toggle-py-debug-p ()
  "Toggle value of ‘py-debug-p’."
  (interactive)
  (setq py-debug-p (not py-debug-p))
  (when (called-interactively-p 'interactive) (message "py-debug-p: %s" py-debug-p)))

(defcustom py-shell-complete-p nil
  "Enable native completion.

Set TAB accordingly."

  :type 'boolean
  :tag "py-shell-complete-p"
  :group 'python-mode)
(make-variable-buffer-local 'py-shell-complete-p)

(defcustom py-section-start "# {{"
  "Delimit arbitrary chunks of code."
  :type 'string
  :tag "py-section-start"
  :group 'python-mode)

(defcustom py-section-end "# }}"
  "Delimit arbitrary chunks of code."
  :type 'string
  :tag "py-section-end"
  :group 'python-mode)

(defvar py-section-re py-section-start)

(defvar py-last-window-configuration nil
  "Internal use.

Restore ‘py-restore-window-configuration’.")

(defvar py-exception-buffer nil
  "Will be set internally.

Remember source buffer where error might occur.")

(defvar py-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string.")

(defvar py-star-labelled-re "[ \\t]*[\\*-] +[[:graph:]]"
  "When looking at a star label.")

(defvar py-colon-labelled-re "[ \\t]*[[:graph:]]* *: *[[:graph:]]+"
  "When looking at a colon label.")
;; (setq py-colon-labelled-re "[ \\t]*[[:graph:]]* *: *[[:graph:]]+\\|[ \\t]*[\\*-] +[[:graph:]]")

(defvar py-labelled-re (concat py-colon-labelled-re "\\|" py-star-labelled-re)
  "When looking at label.")

;; "[ \t]+\\c.+"
(defvar py-symbol-re "[ \t]*\\c.+[ \t]*$"
  "Matching lines only containing symbols.")
(setq py-symbol-re "[ \t]*\\c.+[ \t]*")

(defvar py-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "Expression possibly composing a ‘py-expression’.")

(defvar py-expression-skip-chars "^ (=#\t\r\n\f"
  "Chars composing a ‘py-expression’.")

(setq py-expression-skip-chars "^ [{(=#\t\r\n\f")

(defvar py-expression-re "[^ =#\t\r\n\f]+"
  "Expression possibly composing a ‘py-expression’.")

(defcustom py-paragraph-re paragraph-start
  "Allow Python specific ‘paragraph-start’ var."
  :type 'string
  :tag "py-paragraph-re"
  :group 'python-mode)

(defvar py-not-expression-regexp "[ .=#\t\r\n\f)]+"
  "Regexp indicated probably will not compose a ‘py-expression’.")

(defvar py-not-expression-chars " #\t\r\n\f"
  "Chars indicated probably will not compose a ‘py-expression’.")

;; (defvar py-partial-expression-stop-backward-chars "^] .=,\"'()[{}:#\t\r\n\f"
(defvar py-partial-expression-stop-backward-chars "^] .=,\"'()[{}:#\t\r\n\f"
    "Chars indicated which not possibly compose a ‘py-partial-expression’,
stop at it.")
;; (setq py-partial-expression-stop-backward-chars "^] .=,\"'()[{}:#\t\r\n\f")

(defvar py-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")
;; (setq py-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")

(defvar py-partial-expression-re (concat "[" py-partial-expression-stop-backward-chars (substring py-partial-expression-forward-chars 1) "]+"))
(setq py-partial-expression-re (concat "[" py-partial-expression-stop-backward-chars "]+"))

(defvar py-statement-re py-partial-expression-re)
(defvar py-indent-re ".+"
  "This var is introduced for regularity only.")
(setq py-indent-re ".+")

(defvar py-operator-re "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*"
  "Matches most of Python syntactical meaningful characters.

See also ‘py-assignment-re’")

;; (setq py-operator-re "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*")

(defvar py-delimiter-re "\\(\\.[[:alnum:]]\\|,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs.")

(defvar py-line-number-offset 0
  "When an exception occurs as a result of ‘py-execute-region’.

A subsequent ‘py-up-exception’ needs the line number where the region
started, in order to jump to the correct file line.
This variable is set in ‘py-execute-region’ and used in ‘py--jump-to-exception’.")

(defvar py-match-paren-no-use-syntax-pps nil)

(defvar py-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

(defvar py-XXX-tag-face 'py-XXX-tag-face)

(defvar py-pseudo-keyword-face 'py-pseudo-keyword-face)

(defface py-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face method decorators."
  :tag "py-variable-name-face"
  :group 'python-mode)

(defvar py-variable-name-face 'py-variable-name-face)

(defvar py-number-face 'py-number-face)

(defvar py-decorators-face 'py-decorators-face)

(defvar py-object-reference-face 'py-object-reference-face)

(defvar py-builtins-face 'py-builtins-face)

(defvar py-class-name-face 'py-class-name-face)

(defvar py-def-face 'py-def-face)

(defvar py-exception-name-face 'py-exception-name-face)

(defvar py-import-from-face 'py-import-from-face)

(defvar py-def-class-face 'py-def-class-face)

(defvar py-try-if-face 'py-try-if-face)

(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar jython-mode-hook nil
  "Hook called by ‘jython-mode’.
‘jython-mode’ also calls ‘python-mode-hook’.")

(defvar py-shell-hook nil
  "Hook called by ‘py-shell’.")

;; (defvar python-font-lock-keywords nil)

(defvar py-dotted-expression-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?."_" table)
    table)
  "Syntax table used to identify Python dotted expressions.")

(defvar python-default-template "if"
  "Default template to expand by ‘python-expand-template’.
Updated on each expansion.")

(defvar-local py-already-guessed-indent-offset nil
  "Internal use by ‘py-indent-line’.

When ‘this-command’ is ‘eq’ to ‘last-command’, use the guess already computed.")

(defvar py-shell-template "
\(defun NAME (&optional argprompt)
  \"Start an DOCNAME interpreter in another window.

With optional \\\\[universal-argument] user is prompted
for options to pass to the DOCNAME interpreter. \"
  (interactive \"P\")
  (let\* ((py-shell-name \"FULLNAME\"))
    (py-shell argprompt)
    (when (called-interactively-p 'interactive)
      (switch-to-buffer (current-buffer))
      (goto-char (point-max)))))
")

;; Constants
(defconst py-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]"
  "Matches the beginning of a class, method or compound statement.")

(setq py-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]")

(defconst py-finally-re
  "[ \t]*\\_<finally:"
  "Regular expression matching keyword which closes a try-block.")

(defconst py-except-re "[ \t]*\\_<except\\_>"
  "Matches the beginning of a ‘except’ block.")

;; (defconst py-except-re
;;   "[ \t]*\\_<except\\_>[:( \n\t]*"
;;   "Regular expression matching keyword which composes a try-block.")

(defconst py-return-re
  ".*:?[ \t]*\\_<\\(return\\)\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function.")

(defconst py-decorator-re
  "[ \t]*@[^ ]+\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function.")

(defcustom py-outdent-re-raw
  (list
   "case"
   "elif"
   "else"
   "except"
   "finally"
   )
  "Used by ‘py-outdent-re’."
  :type '(repeat string)
  :tag "py-outdent-re-raw"
  :group 'python-mode
  )

(defconst py-outdent-re
  (concat
   "[ \t]*"
   (regexp-opt py-outdent-re-raw 'symbols)
   "[)\t]*")
  "Regular expression matching statements to be dedented one level.")

(defcustom py-no-outdent-re-raw
  (list
   "break"
   "continue"
   "import"
   "pass"
   "raise"
   "return")
  "Uused by ‘py-no-outdent-re’."
  :type '(repeat string)
  :tag "py-no-outdent-re-raw"
  :group 'python-mode)

(defconst py-no-outdent-re
  (concat
   "[ \t]*"
   (regexp-opt py-no-outdent-re-raw 'symbols)
   "[)\t]*$")
"Regular expression matching lines not to augment indent after.

See ‘py-no-outdent-re-raw’ for better readable content")

(defconst py-assignment-re "\\(\\_<\\w+\\_>[[:alnum:]:, \t]*[ \t]*\\)\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)\\(.*\\)"
  "If looking at the beginning of an assignment.")

;; 'name':
(defconst py-dict-re "'\\_<\\w+\\_>':")

(defcustom py-block-re-raw
  (list
   "async def"
   "async for"
   "async with"
   "class"
   "def"
   "for"
   "if"
   "match"
   "try"
   "while"
   "with"
   )
  "Matches the beginning of a compound statement but not it's clause."
  :type '(repeat string)
  :tag "py-block-re-raw"
  :group 'python-mode)

(defconst py-block-re (concat
		       ;; "[ \t]*"
		       (regexp-opt py-block-re-raw 'symbols)
		       "[:( \n\t]"
		       )
  "Matches the beginning of a compound statement.")

(defconst py-minor-block-re-raw (list
				      "async for"
				      "async with"
                                      "case"
				      "except"
				      "for"
				      "if"
                                      "match"
				      "try"
				      "with"
				      )
  "Matches the beginning of an case ‘for’, ‘if’, ‘try’, ‘except’ or ‘with’ block.")

(defconst py-minor-block-re
  (concat
   "[ \t]*"
   (regexp-opt py-minor-block-re-raw 'symbols)
   "[:( \n\t]")

  "Regular expression matching lines not to augment indent after.

See ‘py-minor-block-re-raw’ for better readable content")

(defconst py-try-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of a ‘try’ block.")

(defconst py-case-re "[ \t]*\\_<case\\_>[: \t][^:]*:"
  "Matches a ‘case’ clause.")

(defconst py-match-case-re "[ \t]*\\_<match\\_>[: \t][^:]*:"
  "Matches a ‘case’ clause.")

(defconst py-for-re "[ \t]*\\_<\\(async for\\|for\\)\\_> +[[:alpha:]_][[:alnum:]_]* +in +[[:alpha:]_][[:alnum:]_()]* *[: \n\t]"
  "Matches the beginning of a ‘try’ block.")

(defconst py-if-re "[ \t]*\\_<if\\_> +[^\n\r\f]+ *[: \n\t]"
  "Matches the beginning of an ‘if’ block.")

(defconst py-else-re "[ \t]*\\_<else:[ \n\t]"
  "Matches the beginning of an ‘else’ block.")

(defconst py-elif-re "[ \t]*\\_<\\elif\\_>[( \n\t]"
  "Matches the beginning of a compound if-statement's clause exclusively.")

;; (defconst py-elif-block-re "[ \t]*\\_<elif\\_> +[[:alpha:]_][[:alnum:]_]* *[: \n\t]"
;;   "Matches the beginning of an ‘elif’ block.")

(defconst py-class-re "[ \t]*\\_<\\(class\\)\\_>[ \n\t]"
  "Matches the beginning of a class definition.")

(defconst py-def-or-class-re "[ \t]*\\_<\\(async def\\|class\\|def\\)\\_>[ \n\t]+\\([[:alnum:]_]*\\)"
  "Matches the beginning of a class- or functions definition.

Second group grabs the name")

;; (setq py-def-or-class-re "[ \t]*\\_<\\(async def\\|class\\|def\\)\\_>[ \n\t]")

;; (defconst py-def-re "[ \t]*\\_<\\(async def\\|def\\)\\_>[ \n\t]"
(defconst py-def-re "[ \t]*\\_<\\(def\\|async def\\)\\_>[ \n\t]"
  "Matches the beginning of a functions definition.")

(defcustom py-block-or-clause-re-raw
  (list
   "async for"
   "async with"
   "async def"
   "async class"
   "class"
   "def"
   "elif"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "try"
   "while"
   "with"
   "match"
   "case"
   )
  "Matches the beginning of a compound statement or it's clause."
  :type '(repeat string)
  :tag "py-block-or-clause-re-raw"
  :group 'python-mode)

(defvar py-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  py-block-or-clause-re-raw 'symbols)
   "[( \t]*.*:?")
  "See ‘py-block-or-clause-re-raw’, which it reads.")

(defcustom py-extended-block-or-clause-re-raw
  (list
   "async def"
   "async for"
   "async with"
   "class"
   "def"
   "elif"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "try"
   "while"
   "with"
   "match"
   "case"
   )
  "Matches the beginning of a compound statement or it's clause."
  :type '(repeat string)
  :tag "py-extended-block-or-clause-re-raw"
  :group 'python-mode)

(defconst py-extended-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  py-extended-block-or-clause-re-raw 'symbols)
   "[( \t:]+")
  "See ‘py-block-or-clause-re-raw’, which it reads.")

(defun py--arglist-indent (nesting &optional indent-offset)
  "Internally used by ‘py-compute-indentation’"
  (if
      (and (eq 1 nesting)
           (save-excursion
             (back-to-indentation)
             (looking-at py-extended-block-or-clause-re)))
      (progn
        (back-to-indentation)
        (1+ (+ (current-column) (* 2 (or indent-offset py-indent-offset)))))
    (+ (current-indentation) (or indent-offset py-indent-offset))))

(defconst py-clause-re py-extended-block-or-clause-re
  "See also py-minor-clause re.")

(defcustom py-minor-clause-re-raw
  (list
   "case"
   "elif"
   "else"
   "except"
   "finally"
   )
  "Matches the beginning of a clause."
    :type '(repeat string)
    :tag "py-minor-clause-re-raw"
    :group 'python-mode)

(defconst py-minor-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  py-minor-clause-re-raw 'symbols)
   "[( \t]*.*:")
  "See ‘py-minor-clause-re-raw’, which it reads.")

(defcustom py-top-level-re
  (concat
   "^[a-zA-Z_]"
   (regexp-opt  py-extended-block-or-clause-re-raw)
   "[( \t]*.*:?")
  "A form which starts at zero indent level, but is not a comment."
  :type '(regexp)
  :tag "py-top-level-re"
  :group 'python-mode
  )

(defvar py-comment-re comment-start
  "Needed for normalized processing.")

(defconst py-block-keywords
   (regexp-opt py-block-or-clause-re-raw 'symbols)
  "Matches known keywords opening a block.

Customizing ‘py-block-or-clause-re-raw’  will change values here")

(defconst py-try-clause-re
  (concat
   "[ \t]*\\_<\\("
   (mapconcat 'identity
              (list
               "else"
               "except"
               "finally")
              "\\|")
   "\\)\\_>[( \t]*.*:")
  "Matches the beginning of a compound try-statement's clause.")

(defcustom py-compilation-regexp-alist
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "Fetch errors from Py-shell.
hooked into ‘compilation-error-regexp-alist’"
  :type '(alist string)
  :tag "py-compilation-regexp-alist"
  :group 'python-mode)

(defun py--quote-syntax (n)
  "Put ‘syntax-table’ property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as ‘syntax-ppss’ to be correct and it seems to be OK
  ;; to use it here despite initial worries.) We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.

  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let* ((syntax (parse-partial-sexp (point-min) (point))))
	(when (eq t (nth 3 syntax))	; after unclosed fence
	  (goto-char (nth 8 syntax))	; fence position
	  ;; (skip-chars-forward "uUrR")	; skip any prefix
	  ;; Is it a matching sequence?
	  (if (eq (char-after) (char-after (match-beginning 2)))
	      (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2) ; leading quote (not prefix)
	       (not (match-end 1)))     ; prefix is null
	  (and (= n 1) ; prefix
	       (match-end 1)))          ; non-empty
      (unless (eq 'string (syntax-ppss-context (parse-partial-sexp (point-min) (point))))
	(eval-when-compile (string-to-syntax "|"))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))

(defconst py-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  ;; First avoid a sequence preceded by an odd number of backslashes.
  `((,(concat "\\(?:^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
              "\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\(?4:\"\\)\\(?5:\"\\)\\(?6:\"\\)\\|\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)")
     (1 (py--quote-syntax 1) t t)
     (2 (py--quote-syntax 2) t t)
     (3 (py--quote-syntax 3) t t)
     (6 (py--quote-syntax 1) t t))))

(defconst py--windows-config-register 313465889
  "Internal used by ‘window-configuration-to-register’.")

(put 'py-indent-offset 'safe-local-variable 'integerp)

;; testing
(defvar py-ert-test-default-executables
  (list "python" "python3" "ipython")
  "Serialize tests employing dolist.")

(defcustom py-shell-unfontify-p t
  "Run ‘py--run-unfontify-timer’ unfontifying the shell banner-text.

Default is nil"

  :type 'boolean
  :tag "py-shell-unfontify-p"
  :group 'python-mode)

;; Pdb
;; #62, pdb-track in a shell buffer
(defcustom pdb-track-stack-from-shell-p t
  "If t, track source from shell-buffer.

Default is t.
Add hook \\='comint-output-filter-functions \\='py--pdbtrack-track-stack-file"

  :type 'boolean
  :tag "pdb-track-stack-from-shell-p"
  :group 'python-mode)

(defcustom py-update-gud-pdb-history-p t
  "If pdb should provide suggestions WRT file to check and ‘py-pdb-path’.

Default is t
See lp:963253"
  :type 'boolean
  :tag "py-update-gud-pdb-history-p"
  :group 'python-mode)

(defcustom py-pdb-executable nil
  "Indicate PATH/TO/pdb.

Default is nil
See lp:963253"
  :type 'string
  :tag "py-pdb-executable"
  :group 'python-mode)

(defcustom py-pdb-path
  (if (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
      (quote c:/python27/python\ -i\ c:/python27/Lib/pdb.py)
    '/usr/lib/python2.7/pdb.py)
  "Where to find pdb.py.  Edit this according to your system.
For example \"/usr/lib/python3.4\" might be an option too.

If you ignore the location `M-x py-guess-pdb-path' might display it."
  :type 'variable
  :tag "py-pdb-path"
  :group 'python-mode)

(defvar py-python-ms-pdb-command ""
  "MS-systems might use that.")

(defcustom py-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[>)]+ "
  "Regular expression matching pdb input prompt of Python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :tag "py-shell-prompt-pdb-regexp"
  :group 'python-mode)

(defcustom py-pdbtrack-stacktrace-info-regexp
  "> \\([^\"(<]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression matching stacktrace information.
Used to extract the current line and module being inspected."
  :type 'string
  :safe 'stringp
  :tag "py-pdbtrack-stacktrace-info-regexp"
  :group 'python-mode)

(defvar py-pdbtrack-tracked-buffer nil
  "Variable containing the value of the current tracked buffer.
Never set this variable directly, use
‘py-pdbtrack-set-tracked-buffer’ instead.")

(defvar py-pdbtrack-buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")

(defcustom py-pdbtrack-do-tracking-p t
  "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as ‘gud-mode’ does for debugging C programs with gdb."
  :type 'boolean
  :tag "py-pdbtrack-do-tracking-p"
  :group 'python-mode)
(make-variable-buffer-local 'py-pdbtrack-do-tracking-p)

(defcustom py-pdbtrack-filename-mapping nil
  "Supports mapping file paths when opening file buffers in pdbtrack.
When non-nil this is an alist mapping paths in the Python interpreter
to paths in Emacs."
  :type 'alist
  :tag "py-pdbtrack-filename-mapping"
  :group 'python-mode)

(defcustom py-pdbtrack-minor-mode-string " PDB"
  "String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
  :tag "py-pdbtrack-minor-mode-string"
  :group 'python-mode)

(defconst py-pdbtrack-stack-entry-regexp
   (concat ".*\\("py-shell-input-prompt-1-regexp">\\|"py-ipython-input-prompt-re">\\|>\\) *\\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>()]+\\)()")
  "Regular expression pdbtrack uses to find a stack trace entry.")

(defconst py-pdbtrack-marker-regexp-file-group 2
  "Group position in gud-pydb-marker-regexp that matches the file name.")

(defconst py-pdbtrack-marker-regexp-line-group 3
  "Group position in gud-pydb-marker-regexp that matches the line number.")

(defconst py-pdbtrack-marker-regexp-funcname-group 4
  "Group position in gud-pydb-marker-regexp that matches the function name.")

(defconst py-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

(defvar py-pdbtrack-is-tracking-p nil)

(defvar py--docbeg nil
  "Internally used by ‘py--write-edit’.")

(defvar py--docend nil
  "Internally used by ‘py--write-edit’.")

(defcustom py-completion-setup-code
  "
def __PYTHON_EL_get_completions(text):
    completions = []
    completer = None

    try:
        import readline

        try:
            import __builtin__
        except ImportError:
            # Python 3
            import builtins as __builtin__
        builtins = dir(__builtin__)

        is_ipython = ('__IPYTHON__' in builtins or
                      '__IPYTHON__active' in builtins)
        splits = text.split()
        is_module = splits and splits[0] in ('from', 'import')

        if is_ipython and is_module:
            from IPython.core.completerlib import module_completion
            completions = module_completion(text.strip())
        elif is_ipython and '__IP' in builtins:
            completions = __IP.complete(text)
        elif is_ipython and 'get_ipython' in builtins:
            completions = get_ipython().Completer.all_completions(text)
        else:
            # Try to reuse current completer.
            completer = readline.get_completer()
            if not completer:
                # importing rlcompleter sets the completer, use it as a
                # last resort to avoid breaking customizations.
                import rlcompleter
                completer = readline.get_completer()
            if getattr(completer, 'PYTHON_EL_WRAPPED', False):
                completer.print_mode = False
            i = 0
            while True:
                completion = completer(text, i)
                if not completion:
                    break
                i += 1
                completions.append(completion)
    except:
        pass
    finally:
        if getattr(completer, 'PYTHON_EL_WRAPPED', False):
            completer.print_mode = True
    return completions"
  "Code used to setup completion in inferior Python processes."
  :type 'string
  :tag "py-completion-setup-code"
  :group 'python-mode)

(defcustom py-shell-completion-string-code
  "';'.join(__PYTHON_EL_get_completions('''%s'''))"
  "Python code used to get a string of completions separated by semicolons.
The string passed to the function is the current python name or
the full statement in the case of imports."
  :type 'string
  :tag "py-shell-completion-string-code"
  :group 'python-mode)

(defface py-XXX-tag-face
  '((t (:inherit font-lock-string-face)))
  "XXX\\|TODO\\|FIXME "
  :tag "py-XXX-tag-face"
  :group 'python-mode)

(defface py-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False,
  Ellipsis.

See also ‘py-object-reference-face’"
  :tag "py-pseudo-keyword-face"
  :group 'python-mode)

(defface py-object-reference-face
  '((t (:inherit py-pseudo-keyword-face)))
  "Face when referencing object members from its class resp. method.,
commonly \"cls\" and \"self\""
  :tag "py-object-reference-face"
  :group 'python-mode)

(defface py-number-face
 '((t (:inherit nil)))
  "Highlight numbers."
  :tag "py-number-face"
  :group 'python-mode)

(defface py-try-if-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords."
  :tag "py-try-if-face"
  :group 'python-mode)

(defface py-import-from-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords."
  :tag "py-import-from-face"
  :group 'python-mode)

(defface py-def-class-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords."
  :tag "py-def-class-face"
  :group 'python-mode)

 ;; PEP 318 decorators
(defface py-decorators-face
  '((t (:inherit font-lock-keyword-face)))
  "Face method decorators."
  :tag "py-decorators-face"
  :group 'python-mode)

(defface py-builtins-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for builtins like TypeError, object, open, and exec."
  :tag "py-builtins-face"
  :group 'python-mode)

(defface py-class-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for classes."
  :tag "py-class-name-face"
  :group 'python-mode)

(defface py-def-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for classes."
  :tag "py-class-name-face"
  :group 'python-mode)

(defface py-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for Python exceptions."
  :tag "py-exception-name-face"
  :group 'python-mode)

;; subr-x.el might not exist yet
;; #73, Byte compilation on Emacs 25.3 fails on different trim-right signature

(defsubst py--string-trim-left (strg &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") strg)
      (replace-match "" t t strg)
    strg))

(defsubst py--string-trim-right (strg &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'") strg)
      (replace-match "" t t strg)
    strg))

(defsubst py--string-trim (strg &optional trim-left trim-right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  (py--string-trim-left (py--string-trim-right strg trim-right) trim-left))

(defsubst string-blank-p (strg)
  "Check whether STRING is either empty or only whitespace."
  (string-match-p "\\`[ \t\n\r]*\\'" strg))

(defsubst string-remove-prefix (prefix strg)
  "Remove PREFIX from STRING if present."
  (if (string-prefix-p prefix strg)
      (substring strg (length prefix))
    strg))

(defun py-toggle-imenu-create-index ()
  "Toggle value of ‘py--imenu-create-index-p’."
  (interactive)
  (setq py--imenu-create-index-p (not py--imenu-create-index-p))
  (when (called-interactively-p 'interactive)
    (message "py--imenu-create-index-p: %s" py--imenu-create-index-p)))

(defun py-toggle-shell-completion ()
  "Switch value of buffer-local var ‘py-shell-complete-p’."
  (interactive)
    (setq py-shell-complete-p (not py-shell-complete-p))
    (when (called-interactively-p 'interactive)
      (message "py-shell-complete-p: %s" py-shell-complete-p)))

(defun py--at-raw-string ()
  "If at beginning of a raw-string."
  (and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R))))

(defmacro py-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line."
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped-p))))

(defmacro py-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line."
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped-p))))

(defun py--skip-to-comment-or-semicolon ()
  "Returns position if point was moved."
  (let ((orig (point)))
    (cond ((while (and (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
                       ;; (sit-for 1)
                       (and (nth 8 (parse-partial-sexp (point-min) (point))) (skip-chars-forward "#;" (line-end-position)))))))
    (and (< orig (point))(point))))

(defun py-forward-statement (&optional orig done repeat)
  "Go to the last char of current statement.

ORIG - consider orignial position or point.
DONE - transaktional argument
REPEAT - count and consider repeats"
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (orig (or orig (point)))
	  erg last
	  ;; use by scan-lists
	  forward-sexp-function pps err)
      (setq pps (parse-partial-sexp (point-min) (point)))
      ;; (origline (or origline (py-count-lines)))
      (cond
       ;; which-function-mode, lp:1235375
       ((< py-max-specpdl-size repeat)
	(error "py-forward-statement reached loops max. If no error, customize ‘py-max-specpdl-size’"))
       ;; list
       ((nth 1 pps)
	(if (<= orig (point))
	    (progn
	      (setq orig (point))
	      ;; do not go back at a possible unclosed list
	      (goto-char (nth 1 pps))
	      (if
		  (ignore-errors (forward-list))
		  (progn
		    (when (looking-at ":[ \t]*$")
		      (forward-char 1))
		    (setq done t)
		    (skip-chars-forward "^#" (line-end-position))
		    (skip-chars-backward " \t\r\n\f" (line-beginning-position))
		    (py-forward-statement orig done repeat))
		(setq err (py--record-list-error pps))
		(goto-char orig)))))
       ;; in comment
       ((and comment-start (looking-at (concat " *" comment-start)))
        (py--end-of-comment-intern (point)))
       ;; (goto-char (match-end 0))
       ;; (py-forward-statement orig done repeat))
       ((nth 4 pps)
	(py--end-of-comment-intern (point))
	(py--skip-to-comment-or-semicolon)
	(while (and (eq (char-before (point)) ?\\)
		    (py-escaped-p) (setq last (point)))
	  (forward-line 1) (end-of-line))
	(and last (goto-char last)
	     (forward-line 1)
	     (back-to-indentation))
	;; py-forward-statement-test-3JzvVW
	(unless (or (looking-at (concat " *" comment-start))(eolp))
	  (py-forward-statement orig done repeat)))
       ;; string
       ((looking-at py-string-delim-re)
	(goto-char (match-end 0))
	(py-forward-statement orig done repeat))
       ((nth 3 pps)
	(when (py-end-of-string)
	  (end-of-line)
	  (skip-chars-forward " \t\r\n\f")
	  (setq pps (parse-partial-sexp (point-min) (point)))
	  (unless (and done (not (or (nth 1 pps) (nth 8 pps))) (eolp)) (py-forward-statement orig done repeat))))
       ((py-current-line-backslashed-p)
	(end-of-line)
	(skip-chars-backward " \t\r\n\f" (line-beginning-position))
	(while (and (eq (char-before (point)) ?\\)
		    (py-escaped-p))
	  (forward-line 1)
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
	(unless (eobp)
	  (py-forward-statement orig done repeat)))
       ((eq orig (point))
	(if (eolp)
	    (skip-chars-forward " \t\r\n\f#'\"")
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f" orig))
	;; point at orig due to a trailing whitespace
	(and (eq (point) orig) (skip-chars-forward " \t\r\n\f"))
	;; (setq done t)
	(py-forward-statement orig done repeat))
       ((eq (current-indentation) (current-column))
	(py--skip-to-comment-or-semicolon)
	(setq pps (parse-partial-sexp orig (point)))
	(if (nth 1 pps)
	    (py-forward-statement orig done repeat)
	  (unless done
	    (py-forward-statement orig done repeat))))
       ((and (looking-at "[[:print:]]+$") (not done) (py--skip-to-comment-or-semicolon))
	(py-forward-statement orig done repeat)))
      (unless
	  (or
	   (eq (point) orig)
	   (member (char-before) (list 10 32 9 ?#)))
	(setq erg (point)))
      (if (and py-verbose-p err)
	  (py--message-error err))
      erg)))

(defun py-backward-statement (&optional orig done limit ignore-in-string-p repeat maxindent)
  "Go to the initial line of a simple statement.

For beginning of compound statement use ‘py-backward-block’.
For beginning of clause ‘py-backward-clause’.

‘ignore-in-string-p’ allows moves inside a docstring, used when
computing indents
ORIG - consider orignial position or point.
DONE - transaktional argument
LIMIT - honor limit
IGNORE-IN-STRING-P - also much inside a string
REPEAT - count and consider repeats
Optional MAXINDENT: don't stop if indentation is larger"
  (interactive)
  (save-restriction
    (unless (bobp)
      (let* ((repeat (or (and repeat (1+ repeat)) 0))
	     (orig (or orig (point)))
             (pps (parse-partial-sexp (or limit (point-min))(point)))
             (done done)
             erg)
	;; lp:1382788
	(unless done
	  (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))
 	       (setq pps (parse-partial-sexp (or limit (point-min))(point)))))
        (cond
	 ((< py-max-specpdl-size repeat)
	  (error "Py-forward-statement reached loops max. If no error, customize ‘py-max-specpdl-size’"))
         ((and (bolp) (eolp))
          (skip-chars-backward " \t\r\n\f")
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; inside string
         ((and (nth 3 pps) (not ignore-in-string-p))
	  (setq done t)
	  (goto-char (nth 8 pps))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ((nth 4 pps)
	  (while (ignore-errors (goto-char (nth 8 pps)))
	    (skip-chars-backward " \t\r\n\f")
	    (setq pps (parse-partial-sexp (line-beginning-position) (point))))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((nth 1 pps)
          (goto-char (1- (nth 1 pps)))
	  (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation) (point)))
	    (setq done t))
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((py-preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; at raw-string
	 ;; (and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
	 ((and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
	  (forward-char -1)
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
          (forward-comment -1)
          (while (and (not (bobp)) (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
            (forward-comment -1))
          (unless (bobp)
            (py-backward-statement orig done limit ignore-in-string-p repeat maxindent)))
	 ;; at inline comment
         ((looking-at "[ \t]*#")
	  (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation) (point)))
	    (setq done t))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; at beginning of string
	 ((looking-at py-string-delim-re)
	  (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
	    (setq done t))
	  (back-to-indentation)
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; after end of statement
	 ((and (not done) (eq (char-before) ?\;))
	  (skip-chars-backward ";")
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; travel until indentation or semicolon
	 ((and (not done) (py--skip-to-semicolon-backward))
	  (unless (and maxindent (< maxindent (current-indentation)))
	    (setq done t))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; at current indent
	 ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ((and maxindent (< maxindent (current-indentation)))
	  (forward-line -1)
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent)))
	;; return nil when before comment
	(unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
	  (when (< (point) orig)(setq erg (point))))
	erg))))

(defun py-backward-statement-bol ()
  "Goto beginning of line where statement start.
Returns position reached, if successful, nil otherwise.

See also ‘py-up-statement’"
  (interactive)
  (let* ((orig (point))
         erg)
    (unless (bobp)
      (cond ((bolp)
	     (and (py-backward-statement orig)
		  (progn (beginning-of-line)
			 (setq erg (point)))))
	    (t (setq erg
		     (and
		      (py-backward-statement)
		      (progn (beginning-of-line) (point)))))))
    erg))

(defun py-forward-statement-bol ()
  "Go to the ‘beginning-of-line’ following current statement."
  (interactive)
  (py-forward-statement)
  (py--beginning-of-line-form))

(defun py-beginning-of-statement-p ()
  (interactive)
  (save-restriction
    (eq (point)
    (save-excursion
      (py-forward-statement)
      (py-backward-statement)))))

(defun py-up-statement ()
  "go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise."
  (interactive)
  (if (py--beginning-of-statement-p)
      (py-backward-statement)
    (progn (and (py-backward-statement) (py-backward-statement)))))

(defun py--end-of-statement-p ()
  "Return position, if cursor is at the end of a statement, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-statement)
      (py-forward-statement)
      (when (eq orig (point))
        orig))))

(defun py-down-statement ()
  "Go to the beginning of next statement downwards in buffer.

Corresponds to backward-up-list in Elisp
Return position if statement found, nil otherwise."
  (interactive)
  (let* ((orig (point)))
    (cond ((py--end-of-statement-p)
	   (progn
	     (and
	      (py-forward-statement)
	      (py-backward-statement)
	      (< orig (point))
	      (point))))
	  ((ignore-errors (< orig (and (py-forward-statement) (py-backward-statement))))
	   (point))
	  ((ignore-errors (< orig (and (py-forward-statement) (py-forward-statement)(py-backward-statement))))
	     (point)))))

(defun py--backward-regexp (regexp &optional indent condition orig regexpvalue)
  "Search backward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (unless (py-beginning-of-statement-p) (skip-chars-backward " \t\r\n\f")
	    (py-backward-comment (point)))
    (let* (pps
	   (regexpvalue (or regexpvalue (symbol-value regexp)))
	   (indent (cond ((eq regexp 'py-match-case-re)
                          nil)
                       (t (or indent (current-indentation)))))
	   (condition (or condition '<=))
	   (orig (or orig (point))))
      (if (eq (current-indentation) (current-column))
	  (while (and
		  (not (bobp))
		  ;; # class kugel(object) -> a[1:2]:
		  ;; class kugel(object):
		  ;; (re-search-backward regexpvalue nil 'move 1)
		  ;; (re-search-backward (concat "^ \\{0,"(format "%s" indent) "\\}"regexpvalue) nil 'move 1)
		  (re-search-backward regexpvalue nil 'move 1)
		  ;; (re-search-backward (concat "^" "def") nil 'move 1)
		  ;; re-search-backward not greedy
		  (not (and (looking-back "async *" (line-beginning-position))
			    (goto-char (match-beginning 0))))
		  (or (and
                       (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
                       (goto-char pps))
		      ;; needed by py-backward-clause
                      (and (not (eq (current-column) 0)) indent
		      	   (funcall condition indent (current-indentation))))))
	(back-to-indentation)
	(and
         (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
         (goto-char pps))
	(unless (and (< (point) orig) (looking-at regexpvalue))
	  (py--backward-regexp regexp (current-indentation) condition orig)))
      (unless (or (eq (point) orig)(bobp)) (back-to-indentation))
      (and (looking-at regexpvalue) (not (nth 8 (parse-partial-sexp (point-min) (point))))(point)))))

(defun py--fetch-indent-statement-above (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (back-to-indentation)
    (if (or (looking-at comment-start)(py-beginning-of-statement-p))
        (current-indentation)
      (py-backward-statement)
      (current-indentation))))

(defun py--docstring-p (pos)
  "Check to see if there is a docstring at POS.

If succesful, returns beginning of docstring position in buffer"
  (save-excursion
    (let ((erg
	   (progn
	     (goto-char pos)
	     (and (looking-at "\"\"\"\\|'''")
		  ;; https://github.com/swig/swig/issues/889
		  ;; def foo(rho, x):
		  ;;     r"""Calculate :math:`D^\nu \rho(x)`."""
		  ;;     return True
		  (if (py--at-raw-string)
		      (progn
			(forward-char -1)
			(point))
		    (point))))))
      (when (and erg (py-backward-statement))
	(when (or (bobp) (looking-at py-def-or-class-re)(looking-at "\\_<__[[:alnum:]_]+__\\_>"))
	  erg)))))

(defun py--font-lock-syntactic-face-function (state)
  "STATE expected as result von (parse-partial-sexp (point-min) (point)."
  (if (nth 3 state)
      (if (py--docstring-p (nth 8 state))
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook nil))

(defun py-choose-shell-by-shebang (&optional shebang)
  "Choose shell by looking at #! on the first line.

If SHEBANG is non-nil, returns the shebang as string,
otherwise the Python resp. Jython shell command name."
  (interactive)
  ;; look for an interpreter specified in the first line
  (let* (erg res)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at py-shebang-regexp)
        (if shebang
            (setq erg (match-string-no-properties 0))
          (setq erg (split-string (match-string-no-properties 0) "[#! \t]"))
          (dolist (ele erg)
            (when (string-match "[bijp]+ython" ele)
              (setq res ele))))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" res))
    res))

(defun py--choose-shell-by-import ()
  "Choose CPython or Jython mode based imports.

If a file imports any packages in ‘py-jython-packages’, within
‘py-import-check-point-max’ characters from the start of the file,
return ‘jython’, otherwise return nil."
  (let (mode)
    (save-excursion
      (goto-char (point-min))
      (while (and (not mode)
                  (search-forward-regexp
                   "^\\(\\(from\\)\\|\\(import\\)\\) \\([^ \t\n.]+\\)"
                   py-import-check-point-max t))
        (setq mode (and (member (match-string 4) py-jython-packages)
                        'jython))))
    mode))

(defun py-choose-shell-by-path (&optional separator-char)
  "SEPARATOR-CHAR according to system variable ‘path-separator’.

Select Python executable according to version desplayed in path.
Returns versioned string, nil if nothing appropriate found"
  (interactive)
  (let ((path (py--buffer-filename-remote-maybe))
	(separator-char (or separator-char py-separator-char))
                erg)
    (when (and path separator-char
               (string-match (concat separator-char "[iI]?[pP]ython[0-9.]+" separator-char) path))
      (setq erg (substring path
                           (1+ (string-match (concat separator-char "[iI]?[pP]ython[0-9.]+" separator-char) path)) (1- (match-end 0)))))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-which-python (&optional shell)
  "Return version of Python of current environment, a number.
Optional argument SHELL selected shell."
  (interactive)
  (let* ((cmd (or shell (py-choose-shell)))
	 (treffer (string-match "\\([23]*\\.?[0-9\\.]*\\)$" cmd))
         version erg)
    (if treffer
        ;; if a number if part of python name, assume it's the version
        (setq version (substring-no-properties cmd treffer))
      (setq erg (shell-command-to-string (concat cmd " --version")))
      (setq version (cond ((string-match (concat "\\(on top of Python \\)" "\\([0-9]\\.[0-9]+\\)") erg)
                           (match-string-no-properties 2 erg))
                          ((string-match "\\([0-9]\\.[0-9]+\\)" erg)
                           (substring erg 7 (1- (length erg)))))))
    (when (called-interactively-p 'any)
      (if version
          (when py-verbose-p (message "%s" version))
        (message "%s" "Could not detect Python on your system")))
    (string-to-number version)))

(defun py-python-current-environment ()
  "Return path of current Python installation."
  (interactive)
  (let* ((cmd (py-choose-shell))
         (denv (shell-command-to-string (concat "type " cmd)))
         (erg (substring denv (string-match "/" denv))))
    (when (called-interactively-p 'any)
      (if erg
          (message "%s" erg)
        (message "%s" "Could not detect Python on your system")))
    erg))

 ;; requested by org-mode still
(defalias 'py-toggle-shells 'py-choose-shell)

(defun py--cleanup-process-name (res)
  "Make res ready for use by ‘executable-find’.

Returns RES or substring of RES"
  (if (string-match "<" res)
      (substring res 0 (match-beginning 0))
    res))

(defalias 'py-which-shell 'py-choose-shell)
(defun py-choose-shell (&optional shell)
  "Return an appropriate executable as a string.

Does the following:
 - look for an interpreter with ‘py-choose-shell-by-shebang’
 - examine imports using ‘py--choose-shell-by-import’
 - look if Path/To/File indicates a Python version
 - if not successful, return default value of ‘py-shell-name’

When interactivly called, messages the SHELL name
Return nil, if no executable found."
  (interactive)
  ;; org-babel uses ‘py-toggle-shells’ with arg, just return it
  (or shell
      (let* (res
	     done
	     (erg
	      (cond (py-force-py-shell-name-p
		     (default-value 'py-shell-name))
		    (py-use-local-default
		     (if (not (string= "" py-shell-local-path))
			 (expand-file-name py-shell-local-path)
		       (message "Abort: ‘py-use-local-default’ is set to ‘t’ but ‘py-shell-local-path’ is empty. Maybe call ‘py-toggle-local-default-use’")))
		    ((and (not py-fast-process-p)
			  (comint-check-proc (current-buffer))
			  (setq done t)
			  (string-match "ython" (process-name (get-buffer-process (current-buffer)))))
		     (setq res (process-name (get-buffer-process (current-buffer))))
		     (py--cleanup-process-name res))
		    ((py-choose-shell-by-shebang))
		    ((py--choose-shell-by-import))
		    ((py-choose-shell-by-path))
		    (t (or
			py-python-command
			"python3"))))
	     (cmd (if (or
		       ;; comint-check-proc was succesful
		       done
		       py-edit-only-p)
		      erg
		    (executable-find erg))))
	(if cmd
	    (when (called-interactively-p 'any)
	      (message "%s" cmd))
	  (when (called-interactively-p 'any) (message "%s" "Could not detect Python on your system. Maybe set ‘py-edit-only-p’?")))
	erg)))

(defun py--normalize-directory (directory)
  "Make sure DIRECTORY ends with a file-path separator char.

Returns DIRECTORY"
  (cond ((string-match (concat py-separator-char "$") directory)
         directory)
        ((not (string= "" directory))
         (concat directory py-separator-char))))

(defun py--normalize-pythonpath (pythonpath)
  "Make sure PYTHONPATH ends with a colon.

Returns PYTHONPATH"
  (let ((erg (cond ((string-match (concat path-separator "$") pythonpath)
                    pythonpath)
                   ((not (string= "" pythonpath))
                    (concat pythonpath path-separator))
		   (t pythonpath))))
    erg))

(defun py-install-directory-check ()
  "Do some sanity check for ‘py-install-directory’.

Returns t if successful."
  (interactive)
  (let ((erg (and (boundp 'py-install-directory) (stringp py-install-directory) (< 1 (length py-install-directory)))))
    (when (called-interactively-p 'any) (message "py-install-directory-check: %s" erg))
    erg))

(defun py--buffer-filename-remote-maybe (&optional file-name)
  "Argument FILE-NAME: the value of variable ‘buffer-file-name’."
  (let ((file-name (or file-name
                       (and
                        (ignore-errors (file-readable-p (buffer-file-name)))
                        (buffer-file-name)))))
    (if (and (featurep 'tramp) (tramp-tramp-file-p file-name))
        (tramp-file-name-localname
         (tramp-dissect-file-name file-name))
      file-name)))

(defun py-guess-py-install-directory ()
  "If `(locate-library \"python-mode\")' is not succesful.

Used only, if ‘py-install-directory’ is empty."
  (interactive)
  (cond (;; don't reset if it already exists
	 py-install-directory)
        ;; ((locate-library "python-mode")
	;;  (file-name-directory (locate-library "python-mode")))
	((ignore-errors (string-match "python-mode" (py--buffer-filename-remote-maybe)))
	 (file-name-directory (py--buffer-filename-remote-maybe)))
        (t (if
	       (and (get-buffer "python-mode.el")
		    (set-buffer (get-buffer "python-mode.el"))
		    ;; (setq py-install-directory (ignore-errors (file-name-directory (buffer-file-name (get-buffer  "python-mode.el")))))
		    (buffer-file-name (get-buffer  "python-mode.el")))
	       (setq py-install-directory (file-name-directory (buffer-file-name (get-buffer  "python-mode.el"))))
	     (if
		 (and (get-buffer "python-components-mode.el")
		      (set-buffer (get-buffer "python-components-mode.el"))
		      (buffer-file-name (get-buffer  "python-components-mode.el")))
		 (setq py-install-directory (file-name-directory (buffer-file-name (get-buffer  "python-components-mode.el"))))))
	   )))

(defun py--fetch-pythonpath ()
  "Consider settings of ‘py-pythonpath’."
  (if (string= "" py-pythonpath)
      (getenv "PYTHONPATH")
    (concat (py--normalize-pythonpath (getenv "PYTHONPATH")) py-pythonpath)))

(defun py-load-pymacs ()
  "Load Pymacs as delivered.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"
  (interactive)
  (let ((pyshell (py-choose-shell))
        (path (py--fetch-pythonpath))
        (py-install-directory (cond ((string= "" py-install-directory)
                                     (py-guess-py-install-directory))
                                    (t (py--normalize-directory py-install-directory)))))
    (if (py-install-directory-check)
        (progn
          ;; If Pymacs has not been loaded before, prepend py-install-directory to
          ;; PYTHONPATH, so that the Pymacs delivered with python-mode is used.
          (unless (featurep 'pymacs)
            (setenv "PYTHONPATH" (concat
                                  (expand-file-name py-install-directory)
                                  (if path (concat path-separator path)))))
          (setenv "PYMACS_PYTHON" (if (string-match "IP" pyshell)
                                      "python"
                                    pyshell))
          (require 'pymacs))
      (error "‘py-install-directory’ not set, see INSTALL"))))

(when py-load-pymacs-p (py-load-pymacs))

(when (and py-load-pymacs-p (featurep 'pymacs))
  (defun py-load-pycomplete ()
    "Load Pymacs based pycomplete."
    (interactive)
    (let* ((path (py--fetch-pythonpath))
           (py-install-directory (cond ((string= "" py-install-directory)
                                        (py-guess-py-install-directory))
                                       (t (py--normalize-directory py-install-directory))))
           (pycomplete-directory (concat (expand-file-name py-install-directory) "completion")))
      (if (py-install-directory-check)
          (progn
            ;; If the Pymacs process is already running, augment its path.
            (when (and (get-process "pymacs") (fboundp 'pymacs-exec))
              (pymacs-exec (concat "sys.path.insert(0, '" pycomplete-directory "')")))
            (require 'pymacs)
            (setenv "PYTHONPATH" (concat
                                  pycomplete-directory
                                  (if path (concat path-separator path))))
            (push pycomplete-directory load-path)
            (require 'pycomplete)
            (add-hook 'python-mode-hook 'py-complete-initialize))
        (error "‘py-install-directory’ not set, see INSTALL")))))

(when (functionp 'py-load-pycomplete)
  (py-load-pycomplete))

(defun py-set-load-path ()
  "Include needed subdirs of ‘python-mode’ directory."
  (interactive)
  (let ((install-directory (py--normalize-directory py-install-directory)))
    (if py-install-directory
	(cond ((and (not (string= "" install-directory))(stringp install-directory))
               (push (expand-file-name install-directory) load-path)
               (push (concat (expand-file-name install-directory) "completion")  load-path)
               (push (concat (expand-file-name install-directory) "extensions")  load-path)
               (push (concat (expand-file-name install-directory) "test") load-path)
               )
              (t (error "Please set ‘py-install-directory’, see INSTALL")))
      (error "Please set ‘py-install-directory’, see INSTALL")))
  (when (called-interactively-p 'interactive) (message "%s" load-path)))

(defun py-count-lines (&optional beg end)
  "Count lines in accessible part until current line.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115
Optional argument BEG specify beginning.
Optional argument END specify end."
  (interactive)
  (save-excursion
    (let ((count 0)
	  (beg (or beg (point-min)))
	  (end (or end (point))))
      (save-match-data
	(if (or (eq major-mode 'comint-mode)
		(eq major-mode 'py-shell-mode))
	    (if
		(re-search-backward py-shell-prompt-regexp nil t 1)
		(goto-char (match-end 0))
	      ;; (when py-debug-p (message "%s"  "py-count-lines: Don't see a prompt here"))
	      (goto-char beg))
	  (goto-char beg)))
      (while (and (< (point) end)(not (eobp)) (skip-chars-forward "^\n" end))
        (setq count (1+ count))
        (unless (or (not (< (point) end)) (eobp)) (forward-char 1)
                (setq count (+ count (abs (skip-chars-forward "\n" end))))))
      (when (bolp) (setq count (1+ count)))
      (when (and py-debug-p (called-interactively-p 'any)) (message "%s" count))
      count)))

(defun py--escape-doublequotes (start end)
  "Escape doublequotes in region by START END."
  (let ((end (copy-marker end)))
    (save-excursion
      (goto-char start)
      (while (and (not (eobp)) (< 0 (abs (skip-chars-forward "^\"" end))))
	(when (eq (char-after) ?\")
	  (unless (py-escaped-p)
	    (insert "\\")
	    (forward-char 1)))))))

(defun py--escape-open-paren-col1 (start end)
  "Start from position START until position END."
  (goto-char start)
  (while (re-search-forward "^(" end t 1)
    (insert "\\")
    (end-of-line)))

(and py-company-pycomplete-p (require 'company-pycomplete))

(defcustom py-empty-line-p-chars "^[ \t\r]*$"
  "Empty-line-p-chars."
  :type 'regexp
  :tag "py-empty-line-p-chars"
  :group 'python-mode)

(defcustom py-default-working-directory ""
  "If not empty used by ‘py-set-current-working-directory’."
  :type 'string
  :tag "py-default-working-directory"
  :group 'python-mode)

(defun py-empty-line-p ()
  "Return t if cursor is at an empty line, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at py-empty-line-p-chars)))

(defun py-toggle-closing-list-dedents-bos (&optional arg)
  "Switch boolean variable ‘py-closing-list-dedents-bos’.

With optional ARG message state switched to"
  (interactive "p")
  (setq py-closing-list-dedents-bos (not py-closing-list-dedents-bos))
  (when arg (message "py-closing-list-dedents-bos: %s" py-closing-list-dedents-bos)))

(defun py-comint-delete-output ()
  "Delete all output from interpreter since last input.
Does not delete the prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
	(replacement nil)
	(inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
			  (forward-line 0)
			  (point-marker))))
	(delete-region comint-last-input-end pmark)
	(goto-char (process-mark proc))
	(setq replacement (concat "*** output flushed ***\n"
				  (buffer-substring pmark (point))))
	(delete-region pmark (point))))
    ;; Output message and put back prompt
    (comint-output-filter proc replacement)))

(defun py-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (and (nth 4 pps) (nth 8 pps))))

;;
(defun py-in-string-or-comment-p ()
  "Returns beginning position if inside a string or comment, nil otherwise. "
  (or (nth 8 (parse-partial-sexp (point-min) (point)))
      (when (or (looking-at "\"") (looking-at "[ \t]*#[ \t]*"))
        (point))))

(defvar python-mode-map nil)
(when py-org-cycle-p
  (define-key python-mode-map (kbd "<backtab>") 'org-cycle))

(defun py-forward-buffer ()
  "A complementary form used by auto-generated commands.

Returns position reached if successful"
  (interactive)
  (unless (eobp)
    (goto-char (point-max))))

(defun py-backward-buffer ()
  "A complementary form used by auto-generated commands.

Returns position reached if successful"
  (interactive)
  (unless (bobp)
    (goto-char (point-min))))

(defun py--end-of-comment-intern (pos)
  (while (and (not (eobp))
              (forward-comment 99999)))
  ;; forward-comment fails sometimes
  (and (eq pos (point)) (prog1 (forward-line 1) (back-to-indentation))
       (while (member (char-after) (list  (string-to-char comment-start) 10))(forward-line 1)(back-to-indentation))))

(defun py--beginning-of-line-form ()
  "Internal use: Go to beginning of line following end of form.

Return position."
  (if (eobp)
      (point)
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun py--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of statement after a semicolon.

Returns ‘t’ if point was moved"
  (prog1
      (< 0 (abs (skip-chars-backward "^;" (or limit (line-beginning-position)))))
    (skip-chars-forward " \t" (line-end-position))))

;; (defun py-forward-comment ()
;;   "Go to the end of comment at point."
;;   (let ((orig (point))
;;         last)
;;     (while (and (not (eobp)) (nth 4 (parse-partial-sexp (line-beginning-position) (point))) (setq last (line-end-position)))
;;       (forward-line 1)
;;       (end-of-line))
;;     (when
;;         (< orig last)
;;       (goto-char last)(point))))

(defun py-forward-comment ()
  "Go to the end of commented section at point."
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point)))
        last)
    (while (and (not (eobp))(or (eq (char-after) ?#) (nth 4 pps) (py-empty-line-p)))
      (setq last (line-end-position))
      (forward-line 1)
      (end-of-line))
    (unless (nth 4 (parse-partial-sexp (point-min) (point)))
      (when last (goto-char last)))
    ))

(defun py--forward-string-maybe (&optional start)
  "Go to the end of string.

Expects START position of string
Return position of moved, nil otherwise."
  (let ((orig (point)))
    (when start (goto-char start)
	  (when (looking-at "\"\"\"\\|'''")
	    (goto-char (1- (match-end 0)))
	    (forward-sexp))
	  ;; maybe at the inner fence
	  (when (looking-at "\"\"\\|''")
	    (goto-char (match-end 0)))
	  (and (< orig (point)) (point)))))

(defun py-load-skeletons ()
  "Load skeletons from extensions. "
  (interactive)
  (load (concat py-install-directory "/extensions/python-components-skeletons.el")))

(defun py--kill-emacs-hook ()
  "Delete files in ‘py-file-queue’.
These are Python temporary files awaiting execution."
  (mapc #'(lambda (filename)
            (ignore-errors (delete-file filename)))
        py-file-queue))

(add-hook 'kill-emacs-hook 'py--kill-emacs-hook)

;;  Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))

(defun py--update-lighter (shell)
  "Select lighter for mode-line display"
  (setq py-modeline-display
	(cond
	 ;; ((eq 2 (prefix-numeric-value argprompt))
	 ;; py-python2-command-args)
	 ((string-match "^[^-]+3" shell)
	  py-python3-modeline-display)
	 ((string-match "^[^-]+2" shell)
	  py-python2-modeline-display)
	 ((string-match "^.[Ii]" shell)
	  py-ipython-modeline-display)
	 ((string-match "^.[Jj]" shell)
	  py-jython-modeline-display)
	 (t
	  python-mode-modeline-display))))

;;  bottle.py
;;  py   = sys.version_info
;;  py3k = py >= (3,0,0)
;;  py25 = py <  (2,6,0)
;;  py31 = (3,1,0) <= py < (3,2,0)

;;  sys.version_info[0]
(defun py-python-version (&optional executable verbose)
  "Returns versions number of a Python EXECUTABLE, string.

If no EXECUTABLE given, ‘py-shell-name’ is used.
Interactively output of ‘--version’ is displayed. "
  (interactive)
  (let* ((executable (or executable py-shell-name))
         (erg (py--string-strip (shell-command-to-string (concat executable " --version")))))
    (when (called-interactively-p 'any) (message "%s" erg))
    (unless verbose (setq erg (cadr (split-string erg))))
    erg))

(defun py-version ()
  "Echo the current version of ‘python-mode’ in the minibuffer."
  (interactive)
  (message "Using ‘python-mode’ version %s" py-version))

(declare-function compilation-shell-minor-mode "compile" (&optional arg))

(defun py--warn-tmp-files-left ()
  "Detect and warn about file of form \"py11046IoE\" in py-temp-directory."
  (let ((erg1 (file-readable-p (concat py-temp-directory py-separator-char (car (directory-files  py-temp-directory nil "py[[:alnum:]]+$"))))))
    (when erg1
      (message "py--warn-tmp-files-left: %s ?" (concat py-temp-directory py-separator-char (car (directory-files  py-temp-directory nil "py[[:alnum:]]*$")))))))

(defun py--fetch-indent-line-above (&optional orig)
  "Report the preceding indent. "
  (save-excursion
    (when orig (goto-char orig))
    (forward-line -1)
    (current-indentation)))

(defun py-continuation-offset (&optional arg)
  "Set if numeric ARG differs from 1. "
  (interactive "p")
  (and (numberp arg) (not (eq 1 arg)) (setq py-continuation-offset arg))
  (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" py-continuation-offset))
  py-continuation-offset)

(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for ‘parse-partial-sexp’."
  (nth 1 (parse-partial-sexp (or start (point-min)) (point))))

(defun py-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for ‘parse-partial-sexp’."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (parse-partial-sexp ppstart (point)))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" end))
    end))

(defun py--in-comment-p ()
  "Return the beginning of current line's comment, if inside or at comment-start. "
  (save-restriction
    (widen)
    (let* ((pps (parse-partial-sexp (point-min) (point)))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (ignore-errors (looking-at (concat "[ \t]*" comment-start)))
          (setq erg (point))))
      erg)))

(defun py-in-triplequoted-string-p ()
  "Returns character address of start tqs-string, nil if not inside. "
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (erg (when (and (nth 3 pps) (nth 8 pps))(nth 2 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\"\"\\|''''")
                            (goto-char (match-end 0))
                            (setq pps (parse-partial-sexp (point-min) (point)))
                            (when (and (nth 3 pps) (nth 8 pps)) (nth 2 pps)))))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-in-string-p-intern (pps)
  (goto-char (nth 8 pps))
  (list (point) (char-after)(skip-chars-forward (char-to-string (char-after)))))

(defun py-in-string-p ()
  "if inside a double- triple- or singlequoted string,

If non-nil, return a list composed of
- beginning position
- the character used as string-delimiter (in decimal)
- and length of delimiter, commonly 1 or 3 "
  (interactive)
  (save-excursion
    (let* ((pps (parse-partial-sexp (point-min) (point)))
           (erg (when (nth 3 pps)
                  (py-in-string-p-intern pps))))
      (unless erg
        (when (looking-at "\"\\|'")
          (forward-char 1)
          (setq pps (parse-partial-sexp (line-beginning-position) (point)))
          (when (nth 3 pps)
            (setq erg (py-in-string-p-intern pps)))))
      erg)))

(defun py-toggle-local-default-use ()
  "Toggle boolean value of ‘py-use-local-default’.

Returns ‘py-use-local-default’

See also ‘py-install-local-shells’
Installing named virualenv shells is the preffered way,
as it leaves your system default unchanged."
  (interactive)
  (setq py-use-local-default (not py-use-local-default))
  (when (called-interactively-p 'any) (message "py-use-local-default set to %s" py-use-local-default))
  py-use-local-default)

(defun py--beginning-of-buffer-position ()
  "Provided for abstract reasons."
  (point-min))

(defun py--end-of-buffer-position ()
  "Provided for abstract reasons."
  (point-max))

(defun py-backward-comment (&optional pos)
  "Got to beginning of a commented section.

Start from POS if specified"
  (interactive)
  (let ((erg pos)
	last)
    (when erg (goto-char erg))
    (while (and (not (bobp)) (setq erg (py-in-comment-p)))
      (when (< erg (point))
	(goto-char erg)
	(setq last (point)))
      (skip-chars-backward " \t\r\n\f"))
    (when last (goto-char last))
    last))

(defun py-go-to-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

From a programm use macro ‘py-backward-comment’ instead"
  (interactive)
  (let ((erg (py-backward-comment)))
    (when (and py-verbose-p (called-interactively-p 'any))
      (message "%s" erg))))

(defun py--up-decorators-maybe (indent)
  (let ((last (point)))
    (while (and (not (bobp))
		(py-backward-statement)
		(eq (current-indentation) indent)
		(if (looking-at py-decorator-re)
		    (progn (setq last (point)) nil)
		  t)))
    (goto-char last)))

(defun py-leave-comment-or-string-backward ()
  "If inside a comment or string, leave it backward."
  (interactive)
  (let ((pps
         (if (featurep 'xemacs)
             (parse-partial-sexp (point-min) (point))
           (parse-partial-sexp (point-min) (point)))))
    (when (nth 8 pps)
      (goto-char (1- (nth 8 pps))))))

;;  Decorator
(defun py-backward-decorator ()
  "Go to the beginning of a decorator.

Returns position if succesful"
  (interactive)
  (let ((orig (point)))
    (unless (bobp) (forward-line -1)
	    (back-to-indentation)
	    (while (and (progn (looking-at "@\\w+")(not (looking-at "\\w+")))
			(not
			 ;; (py-empty-line-p)
			 (member (char-after) (list 9 10)))
			(not (bobp))(forward-line -1))
	      (back-to-indentation))
	    (or (and (looking-at "@\\w+") (match-beginning 0))
		(goto-char orig)))))

(defun py-forward-decorator ()
  "Go to the end of a decorator.

Returns position if succesful"
  (interactive)
  (let ((orig (point)) erg)
    (unless (looking-at "@\\w+")
      (setq erg (py-backward-decorator)))
    (when erg
      (if
          (re-search-forward py-def-or-class-re nil t)
          (progn
            (back-to-indentation)
            (skip-chars-backward " \t\r\n\f")
            (py-leave-comment-or-string-backward)
            (skip-chars-backward " \t\r\n\f")
            (setq erg (point)))
        (goto-char orig)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (when (ignore-errors (goto-char (py-list-beginning-position)))
          (forward-list))
        (when (< orig (point))
          (setq erg (point))))
      erg)))

(defun py-beginning-of-list-pps (&optional iact last ppstart orig done)
  "Go to the beginning of a list.

IACT - if called interactively
LAST - was last match.
Optional PPSTART indicates a start-position for ‘parse-partial-sexp’.
ORIG - consider orignial position or point.
DONE - transaktional argument
Return beginning position, nil if not inside."
  (interactive "p")
  (let* ((orig (or orig (point)))
         (ppstart (or ppstart (re-search-backward "^[a-zA-Z]" nil t 1) (point-min)))
         erg)
    (unless done (goto-char orig))
    (setq done t)
    (if
        (setq erg (nth 1 (if (featurep 'xemacs)
                             (parse-partial-sexp ppstart (point))
                           (parse-partial-sexp (point-min) (point)))))
        (progn
          (setq last erg)
          (goto-char erg)
          (py-beginning-of-list-pps iact last ppstart orig done))
      last)))

(defun py-end-of-string (&optional beginning-of-string-position)
  "Go to end of string at point if any, if successful return position. "
  (interactive)
  (let ((orig (point))
        (beginning-of-string-position (or beginning-of-string-position (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
                                          (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")(match-beginning 0))))
        erg)
    (if beginning-of-string-position
        (progn
          (goto-char beginning-of-string-position)
          (when
              ;; work around parse-partial-sexp error
              (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
            (goto-char (nth 3 (parse-partial-sexp 1 (point)))))
          (if (ignore-errors (setq erg (scan-sexps (point) 1)))
                              (goto-char erg)
            (goto-char orig)))

      (error (concat "py-end-of-string: don't see end-of-string at " (buffer-name (current-buffer)) "at pos " (point))))
    erg))

(defun py--record-list-error (pps)
  "When encountering a missing parenthesis, store its line, position.
‘py-verbose-p’  must be t"
  (let ((this-err
         (save-excursion
           (list
            (nth 1 pps)
            (progn
              (goto-char (nth 1 pps))
              (py-count-lines (point-min) (point)))))))
    this-err))

(defun py--message-error (err)
  "Receives a list (position line) "
  (message "Closing paren missed: line %s pos %s" (cadr err) (car err)))

(defun py--end-base-determine-secondvalue (regexp)
  "Expects being at block-opener.

REGEXP: a symbol"
  (cond
   ((eq regexp 'py-minor-block-re)
    (cond ((looking-at py-else-re)
	   nil)
	  ((or (looking-at (concat py-try-re)))
	   (concat py-elif-re "\\|" py-else-re "\\|" py-except-re))
	  ((or (looking-at (concat py-except-re "\\|" py-elif-re "\\|" py-if-re)))
	   (concat py-elif-re "\\|" py-else-re))))
   ((member regexp
	    (list
	     'py-block-re
	     'py-block-or-clause-re
	     'py-clause-re
	     'py-if-re
	     ))
    (cond ((looking-at py-if-re)
	   (concat py-elif-re "\\|" py-else-re))
	  ((looking-at py-elif-re)
	   (concat py-elif-re "\\|" py-else-re))
	  ((looking-at py-else-re))
	  ((looking-at py-try-re)
	   (concat py-except-re "\\|" py-else-re "\\|" py-finally-re))
	  ((looking-at py-except-re)
	   (concat py-else-re "\\|" py-finally-re))
	  ((looking-at py-finally-re)
	   nil)))
   ((eq regexp 'py-for-re) nil)
   ((eq regexp 'py-try-re)
    (cond
     ((looking-at py-try-re)
      (concat py-except-re "\\|" py-else-re "\\|" py-finally-re))
     ((looking-at py-except-re)
      (concat py-else-re "\\|" py-finally-re))
     ((looking-at py-finally-re))))))

(defun py--go-to-keyword (regexp &optional maxindent condition ignoreindent)
  "Expects being called from beginning of a statement.

Argument REGEXP: a symbol.

Return a list, whose car is indentation, cdr position.

Keyword detected from REGEXP
Honor MAXINDENT if provided
Optional IGNOREINDENT: find next keyword at any indentation"
  (unless (bobp)
    ;;    (when (py-empty-line-p) (skip-chars-backward " \t\r\n\f"))
    (let* ((orig (point))
	   (condition
	    (or condition (if (member regexp (list 'py-block-re 'py-clause-re)) '< '<=)))
	   ;; py-clause-re would not match block
	   (regexp (if (eq regexp 'py-clause-re) 'py-extended-block-or-clause-re regexp))
	   (regexpvalue (symbol-value regexp))
	   (maxindent
	    (if ignoreindent
		;; just a big value
		9999
	      (or maxindent
                  ;; (min (current-column) (current-indentation))
                  (if (py-empty-line-p) (current-column) (current-indentation))
                  )))
           (lep (line-end-position))
	   erg)
      (unless (py-beginning-of-statement-p)
	(py-backward-statement))
      (cond
       ((looking-at (concat (symbol-value regexp)))
	(if (eq (point) orig)
	    (setq erg (py--backward-regexp regexp maxindent condition orig regexpvalue))
	  (setq erg (point))))
       ((looking-at py-block-closing-keywords-re)
        ;; maybe update maxindent, if already behind the form closed here
        (unless ;; do not update if still starting line
            (eq (line-end-position) lep)
          (setq maxindent (min maxindent (- (current-indentation) py-indent-offset))))
        (setq erg (py--backward-regexp regexp maxindent condition orig regexpvalue)))
       (t (setq erg (py--backward-regexp regexp maxindent condition orig regexpvalue))))
      (when erg (setq erg (cons (current-indentation) erg)))
      (list (car erg) (cdr erg) (py--end-base-determine-secondvalue regexp)))))

(defun py-up-base (regexp &optional indent)
  "Expects a symbol as REGEXP like `'py-clause-re'"
  (unless (py-beginning-of-statement-p) (py-backward-statement))
  (unless (looking-at (symbol-value regexp))
        (py--go-to-keyword regexp (or indent (current-indentation)) '<))
  ;; now from beginning-of-block go one indent level upwards
  (py--go-to-keyword regexp (- (or indent (current-indentation)) py-indent-offset) '<))

(defun py--forward-regexp (regexp)
  "Search forward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (let (erg)
      (while (and
              (setq erg (re-search-forward regexp nil 'move 1))
              (nth 8 (parse-partial-sexp (point-min) (point)))))
      (unless
	  (nth 8 (parse-partial-sexp (point-min) (point)))
        erg))))

(defun py--forward-regexp-keep-indent (regexp &optional indent)
  "Search forward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (let ((indent (or indent (current-indentation)))
          (regexp (if (stringp regexp)
                      regexp
                    (symbol-value regexp)))
	  (orig (point))
          last done)
      (forward-line 1)
      (beginning-of-line)
      (while (and
	      (not done)
              (re-search-forward regexp nil 'move 1)
              (or (nth 8 (parse-partial-sexp (point-min) (point)))
                  (or (< indent (current-indentation))(setq done t))
		  (setq last (line-end-position)))))
      (unless
          (nth 8 (parse-partial-sexp (point-min) (point)))
	(if last (goto-char last)
	  (back-to-indentation))
        (and (< orig (point)) (point))))))

(defun py-down-base (regexp &optional indent bol)
  (let ((indent (or indent (current-indentation))))
    (and (py--forward-regexp-keep-indent regexp indent)
	 (progn
           (if bol
               (beginning-of-line)
             (back-to-indentation))
           (point)))))

(defun py--beginning-of-statement-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘statement’, nil otherwise."
  (interactive)
  (save-excursion
    (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
      (and (not (or (nth 8 pps) (nth 1 pps)))
           (looking-at py-statement-re)
           (looking-back "[^ \t]*" (line-beginning-position))
           (eq (current-column) (current-indentation))
	   (eq (point) (progn (py-forward-statement) (py-backward-statement)))
           (point)))))

(defun py--beginning-of-statement-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘statement’, nil otherwise."
  (save-excursion
    (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
      (and (bolp)
           (not (or (nth 8 pps) (nth 1 pps)))
           (looking-at py-statement-re)
           (looking-back "[^ \t]*" (line-beginning-position))
	   (eq (point) (progn (py-forward-statement-bol) (py-backward-statement-bol)))
           (point)))))

(defun py--refine-regexp-maybe (regexp)
  "Use a more specific regexp if possible. "
  (let ((regexpvalue (symbol-value regexp)))
    (if (looking-at regexpvalue)
	(setq regexp
	      (cond ((looking-at py-if-re)
		     'py-if-re)
		    ((looking-at py-try-re)
		     'py-try-re)
		    ((looking-at py-def-re)
		     'py-def-re)
		    ((looking-at py-class-re)
		     'py-class-re)
		    (t regexp)))
      regexp)))

(defun py-forward-clause-intern (indent)
  (end-of-line)
  (let (last)
    (while
        (and
         (py-forward-statement)
         (save-excursion (py-backward-statement) (< indent (current-indentation)))
         (setq last (point))
         ))
    (when last (goto-char last))))

(defun py--down-according-to-indent (regexp secondvalue &optional indent use-regexp)
  "Return position if moved, nil otherwise.

Optional ENFORCE-REGEXP: search for regexp only."
  (unless (eobp)
    (let* ((orig (point))
	   (indent (or indent 0))
	   done
	   (regexpvalue (if (member regexp (list 'py-def-re 'py-def-or-class-re 'py-class-re))
			    (concat (symbol-value regexp) "\\|" (symbol-value 'py-decorator-re))
			  (symbol-value regexp)))
	   (lastvalue (and secondvalue
			   (pcase regexp
			     (`py-try-re py-finally-re)
			     (`py-if-re py-else-re)))))
      (if (eq regexp 'py-clause-re)
          (py-forward-clause-intern indent)
        (while
	    (and
	     (not done)
	     (progn (end-of-line)
		    (cond (use-regexp
			   ;; using regexpvalue might stop behind global settings, missing the end of form
			   (re-search-forward (concat "^ \\{0,"(format "%s" indent) "\\}"regexpvalue) nil 'move 1))
			  (t (re-search-forward (concat "^ \\{"(format "0,%s" indent) "\\}[[:alnum:]_@]+") nil 'move 1))))
	     (or (nth 8 (parse-partial-sexp (point-min) (point)))
	         (progn (back-to-indentation) (py--forward-string-maybe (nth 8 (parse-partial-sexp orig (point)))))
	         (and secondvalue (looking-at secondvalue))
	         (and lastvalue (looking-at lastvalue))
	         (and (looking-at regexpvalue) (setq done t))
	         ;; py-forward-def-or-class-test-3JzvVW
	         ;; (setq done t)
                 )))
        (and (< orig (point)) (point))))))

(defun py--backward-empty-lines-or-comment ()
  "Travel backward"
  (while
      (or (< 0 (abs (skip-chars-backward " \t\r\n\f")))
	  (py-backward-comment))))

;; (defun py-kill-buffer-unconditional (buffer)
;;   "Kill buffer unconditional, kill buffer-process if existing. "
;;   (interactive
;;    (list (current-buffer)))
;;   (ignore-errors (with-current-buffer buffer
;;     (let (kill-buffer-query-functions)
;;       (set-buffer-modified-p nil)
;;       (ignore-errors (kill-process (get-buffer-process buffer)))
;;       (kill-buffer buffer)))))

(defun py--down-end-form ()
  "Return position."
  (progn (py--backward-empty-lines-or-comment)
	 (point)))

(defun py--which-delay-process-dependent (buffer)
  "Call a ‘py-ipython-send-delay’ or ‘py-python-send-delay’ according to process"
  (if (string-match "^.[IJ]" buffer)
      py-ipython-send-delay
    py-python-send-delay))

(defun py-temp-file-name (strg)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "py")))

    (with-temp-file temp-file-name
      (insert strg)
      (delete-trailing-whitespace))
    temp-file-name))

(defun py--fetch-error (output-buffer &optional origline filename)
  "Highlight exceptions found in BUF.

If an exception occurred return error-string, otherwise return nil.
BUF must exist.

Indicate LINE if code wasn't run from a file,
thus remember ORIGLINE of source buffer"
  (with-current-buffer output-buffer
    (when py-debug-p (switch-to-buffer (current-buffer)))
    ;; (setq py-error (buffer-substring-no-properties (point) (point-max)))
    (goto-char (point-max))
    (when (re-search-backward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
      (when (and filename (re-search-forward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
		 (replace-match filename nil nil nil 1))
	(when (and origline (re-search-forward "line \\([0-9]+\\)\\(.*\\)$" (line-end-position) t 1))
	  (replace-match origline nil nil nil 2)))
      (setq py-error (buffer-substring-no-properties (point) (point-max))))
        py-error))

(defvar py-debug-p nil
  "Used for development purposes.")

(defun py--fetch-result (buffer limit &optional cmd)
  "CMD: some shells echo the command in output-buffer
Delete it here"
  (when py-debug-p (message "(current-buffer): %s" (current-buffer))
	(switch-to-buffer (current-buffer)))
  (cond (python-mode-v5-behavior-p
	 (with-current-buffer buffer
	   (py--string-trim (buffer-substring-no-properties (point-min) (point-max)) nil "\n")))
	((and cmd (< limit (point-max)))
	 (replace-regexp-in-string cmd "" (py--string-trim (replace-regexp-in-string py-shell-prompt-regexp "" (buffer-substring-no-properties limit (point-max))))))
	(t (when (< limit (point-max))
	     (py--string-trim (replace-regexp-in-string py-shell-prompt-regexp "" (buffer-substring-no-properties limit (point-max))))))))

(defun py--postprocess (output-buffer origline limit &optional cmd filename)
  "Provide return values, check result for error, manage windows.

According to OUTPUT-BUFFER ORIGLINE ORIG"
  ;; py--fast-send-string doesn't set origline
  (when (or py-return-result-p py-store-result-p)
    (with-current-buffer output-buffer
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (sit-for (py--which-delay-process-dependent (prin1-to-string output-buffer)))
      ;; (catch 'py--postprocess
      (setq py-result (py--fetch-result output-buffer limit cmd))
      ;; (throw 'py--postprocess (error "py--postprocess failed"))
      ;;)
      (if (and py-result (not (string= "" py-result)))
	  (if (string-match "^Traceback" py-result)
	      (if filename
		  (setq py-error py-result)
		(progn
		  (with-temp-buffer
		    (insert py-result)
		    (sit-for 0.1 t)
		    (setq py-error (py--fetch-error origline filename)))))
	    (when py-store-result-p
	      (kill-new py-result))
	    (when py-verbose-p (message "py-result: %s" py-result))
	    py-result)
	(when py-verbose-p (message "py--postprocess: %s" "Don't see any result"))))))

(defun py-fetch-py-master-file ()
  "Lookup if a ‘py-master-file’ is specified.

See also doku of variable ‘py-master-file’"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^ *# Local Variables:" nil (quote move) 1)
        (when
            (re-search-forward (concat "^\\( *# py-master-file: *\\)\"\\([^ \t]+\\)\" *$") nil t 1)
          (setq py-master-file (match-string-no-properties 2))))))
  ;; (when (called-interactively-p 'any) (message "%s" py-master-file))
  )

(defun py-ipython--which-version (shell)
  "Returns IPython version as string"
  (shell-command-to-string (concat (downcase (replace-regexp-in-string  "[[:punct:]+]" "" shell)) " -V")))

(defun py--provide-command-args (shell fast-process)
  "Unbuffered WRT fast-process"
  (let ((erg
	 (delq nil
	       (cond
		;; ((eq 2 (prefix-numeric-value argprompt))
		;; py-python2-command-args)
		((string-match "^[Ii]" shell)
		 (if (string-match "^[0-4]" (py-ipython--which-version shell))
		     (remove "--simple-prompt"  py-ipython-command-args)
		   (if (member "--simple-prompt"  py-ipython-command-args)
		       py-ipython-command-args
		     (cons "--simple-prompt"  py-ipython-command-args))))
		((string-match "^[^-]+3" shell)
		 py-python3-command-args)
                ((string-match "^[jy]" shell)
                 py-jython-command-args)
		(t
		 py-python-command-args)))))
    (if (and fast-process (not (member "-u" erg)))
	(cons "-u" erg)
      erg)))

;; This and other stuff from python.el
(defun py-info-encoding-from-cookie ()
  "Detect current buffer's encoding from its coding cookie.
Returns the encoding as a symbol."
  (let ((first-two-lines
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (forward-line 2)
             (buffer-substring-no-properties
              (point)
              (point-min))))))
    (when (string-match
	   ;; (py-rx coding-cookie)
	   "^#[[:space:]]*\\(?:coding[:=][[:space:]]*\\(?1:\\(?:[[:word:]]\\|-\\)+\\)\\|-\\*-[[:space:]]*coding:[[:space:]]*\\(?1:\\(?:[[:word:]]\\|-\\)+\\)[[:space:]]*-\\*-\\|vim:[[:space:]]*set[[:space:]]+fileencoding[[:space:]]*=[[:space:]]*\\(?1:\\(?:[[:word:]]\\|-\\)+\\)[[:space:]]*:\\)"
	   first-two-lines)
      (intern (match-string-no-properties 1 first-two-lines)))))

(defun py-info-encoding ()
  "Return encoding for file.
Try ‘py-info-encoding-from-cookie’, if none is found then
default to utf-8."
  (or (py-info-encoding-from-cookie)
      'utf-8))

(defun py-indentation-of-statement ()
  "Returns the indenation of the statement at point. "
  (interactive)
  (let ((erg (save-excursion
               (back-to-indentation)
               (or (py--beginning-of-statement-p)
                   (py-backward-statement))
               (current-indentation))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py--filter-result (strg)
  "Set ‘py-result’ according to ‘py-fast-filter-re’.

Remove trailing newline"
  (py--string-trim
   (replace-regexp-in-string
    py-fast-filter-re
    ""
    (ansi-color-filter-apply strg))))

(defun py--cleanup-shell (orig buffer)
  (with-current-buffer buffer
    (with-silent-modifications
      (sit-for py-python3-send-delay)
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (delete-region orig (point-max)))))

(defun py-shell--save-temp-file (strg)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "py"))
         (coding-system-for-write (py-info-encoding)))
    (with-temp-file temp-file-name
      (insert strg)
      (delete-trailing-whitespace))
    temp-file-name))

(defun py--get-process (&optional argprompt args dedicated shell buffer)
  "Get appropriate Python process for current buffer and return it.

Optional ARGPROMPT DEDICATED SHELL BUFFER"
  (interactive)
  (or (and buffer (get-buffer-process buffer))
      (get-buffer-process (current-buffer))
      (get-buffer-process (py-shell argprompt args dedicated shell buffer))))

(defun py-shell-send-file (file-name &optional process temp-file-name
                                     delete)
  "Send FILE-NAME to Python PROCESS.

If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.  If TEMP-FILE-NAME and DELETE are non-nil, then
TEMP-FILE-NAME is deleted after evaluation is performed.  When
optional argument."
  (interactive
   (list
    (read-file-name "File to send: ")))
  (let* ((proc (or process (py--get-process)))
         (encoding (with-temp-buffer
                     (insert-file-contents
                      (or temp-file-name file-name))
                     (py-info-encoding)))
         (file-name (expand-file-name (file-local-name file-name)))
         (temp-file-name (when temp-file-name
                           (expand-file-name
                            (file-local-name temp-file-name)))))
    (py-shell-send-string
     (format
      (concat
       "import codecs, os;"
       "__pyfile = codecs.open('''%s''', encoding='''%s''');"
       "__code = __pyfile.read().encode('''%s''');"
       "__pyfile.close();"
       (when (and delete temp-file-name)
         (format "os.remove('''%s''');" temp-file-name))
       "exec(compile(__code, '''%s''', 'exec'));")
      (or temp-file-name file-name) encoding encoding file-name)
     proc)))

(defun py-shell-send-string (strg &optional process)
  "Send STRING to Python PROCESS.

Uses ‘comint-send-string’."
  (interactive
   (list (read-string "Python command: ") nil t))
  (let ((process (or process (py--get-process))))
    (if (string-match ".\n+." strg)   ;Multiline.
        (let* ((temp-file-name (py-shell--save-temp-file strg))
               (file-name (or (buffer-file-name) temp-file-name)))
          (py-shell-send-file file-name process temp-file-name t))
      (comint-send-string process strg)
      (when (or (not (string-match "\n\\'" strg))
                (string-match "\n[ \t].*\n?\\'" strg))
        (comint-send-string process "\n")))))

(defun py-fast-process (&optional buffer)
  "Connect am (I)Python process suitable for large output.

Output buffer displays \"Fast\"  by default
It is not in interactive, i.e. comint-mode,
as its bookkeepings seem linked to the freeze reported by lp:1253907"
  (interactive)
  (let ((this-buffer
         (set-buffer (or (and buffer (get-buffer-create buffer))
                         (get-buffer-create py-shell-name)))))
    (let ((proc (start-process py-shell-name this-buffer py-shell-name)))
      (with-current-buffer this-buffer
        (erase-buffer))
      proc)))

(defun py-proc (&optional argprompt)
  "Return the current Python process.

Start a new process if necessary. "
  (interactive "P")
  (let ((erg
         (cond ((comint-check-proc (current-buffer))
                (get-buffer-process (buffer-name (current-buffer))))
               (t (py-shell argprompt)))))
    erg))

(defun py-process-file (filename &optional output-buffer error-buffer)
  "Process \"python FILENAME\".

Optional OUTPUT-BUFFER and ERROR-BUFFER might be given."
  (interactive "fDatei:")
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (output-buffer (or output-buffer (make-temp-name "py-process-file-output")))
        (pcmd (py-choose-shell)))
    (unless (buffer-live-p output-buffer)
      (set-buffer (get-buffer-create output-buffer)))
    (shell-command (concat pcmd " " filename) output-buffer error-buffer)
    (when py-switch-buffers-on-execute-p (switch-to-buffer output-buffer))))

(defvar py-last-exeption-buffer nil
  "Internal use only - when ‘py-up-exception’ is called.

In source-buffer, this will deliver the exception-buffer again.")

(defun py-remove-overlays-at-point ()
  "Remove overlays as set when ‘py-highlight-error-source-p’ is non-nil."
  (interactive "*")
  (delete-overlay (car (overlays-at (point)))))

(defun py--jump-to-exception-intern (act exception-buffer origline)
  (let (erg)
    (set-buffer exception-buffer)
    (goto-char (point-min))
    (forward-line (1- origline))
    (and (search-forward act (line-end-position) t)
         (and py-verbose-p (message "exception-buffer: %s on line %d" py-exception-buffer origline))
         (and py-highlight-error-source-p
              (setq erg (make-overlay (match-beginning 0) (match-end 0)))
              (overlay-put erg
                           'face 'highlight)))))

(defun py--jump-to-exception (perr origline &optional file)
  "Jump to the PERR Python code at ORIGLINE in optional FILE."
  (with-silent-modifications
    (let (
          ;; (inhibit-point-motion-hooks t)
          (file (or file (car perr)))
          (act (nth 2 perr)))
      (cond ((and py-exception-buffer
                  (buffer-live-p py-exception-buffer))
             ;; (pop-to-buffer procbuf)
             (py--jump-to-exception-intern act py-exception-buffer origline))
            ((ignore-errors (file-readable-p file))
             (find-file file)
             (py--jump-to-exception-intern act (get-buffer (file-name-nondirectory file)) origline))
            ((buffer-live-p (get-buffer file))
             (set-buffer file)
             (py--jump-to-exception-intern act file origline))
            (t (setq file (find-file (read-file-name "Exception file: "
                                                     nil
                                                     file t)))
               (py--jump-to-exception-intern act file origline))))))

(defun py-goto-exception (&optional file line)
  "Go to FILE and LINE indicated by the traceback."
  (interactive)
  (let ((file file)
        (line line))
    (unless (and file line)
      (save-excursion
        (beginning-of-line)
        (if (looking-at py-traceback-line-re)
            (setq file (substring-no-properties (match-string 1))
                  line (string-to-number (match-string 2))))))
    (if (not file)
        (error "Not on a traceback line"))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun py--find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
‘re-search-backward’ or ‘re-search-forward’ indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (with-current-buffer buffer
	(goto-char start)
	(if (funcall searchdir py-traceback-line-re nil t)
	    (setq file (match-string 1)
		  line (string-to-number (match-string 2))))))
    (if (and file line)
        (py-goto-exception file line)
      (error "%s of traceback" errwhere))))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((buffer py-output-buffer))
    (if bottom
        (py--find-next-exception 'eob buffer 're-search-backward "Bottom")
      (py--find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((buffer py-output-buffer))
    (if top
        (py--find-next-exception 'bob buffer 're-search-forward "Top")
      (py--find-next-exception 'bol buffer 're-search-backward "Top"))))

;; ;
;;  obsolete by py--fetch-result
;;  followed by py--fetch-error
;;  still used by py--execute-ge24.3

(defun py--find-next-exception-prepare (direction start)
  "According to DIRECTION and START setup exception regexps.

Depends from kind of Python shell."
  (let* ((name (get-process (substring (buffer-name (current-buffer)) 1 -1)))
         (buffer (cond (name (buffer-name (current-buffer)))
                       ((buffer-live-p (get-buffer py-output-buffer))
                        py-output-buffer)
                       (py-last-exeption-buffer (buffer-name py-last-exeption-buffer))
                       (t (error "Don't see exeption buffer")))))
    (when buffer (set-buffer (get-buffer buffer)))
    (if (eq direction 'up)
        (if (string= start "TOP")
            (py--find-next-exception 'bob buffer 're-search-forward "Top")
          (py--find-next-exception 'bol buffer 're-search-backward "Top"))
      (if (string= start "BOTTOM")
          (py--find-next-exception 'eob buffer 're-search-backward "Bottom")
        (py--find-next-exception 'eol buffer 're-search-forward "Bottom")))))

(defun py-shell-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT ends with input prompt."
  (ignore-errors (string-match
		  ;; XXX: It seems on macOS an extra carriage return is attached
		  ;; at the end of output, this handles that too.
		  (concat
		   "\r?\n?"
		   ;; Remove initial caret from calculated regexp
		   (ignore-errors (replace-regexp-in-string
				   (rx string-start ?^) ""
				   py-shell--prompt-calculated-input-regexp))
		   (rx eos))
		  output)))

(defun py-comint-postoutput-scroll-to-bottom (output)
  "Faster version of ‘comint-postoutput-scroll-to-bottom’.
Avoids ‘recenter’ calls until OUTPUT is completely sent."
  (when (and (not (string= "" output))
             (py-shell-comint-end-of-output-p
              (ansi-color-filter-apply output)))
    (comint-postoutput-scroll-to-bottom output))
  output)

(defmacro py-shell--add-to-path-with-priority (pathvar paths)
  "Modify PATHVAR and ensure PATHS are added only once at beginning."
  `(dolist (path (reverse ,paths))
     (cl-delete path ,pathvar :test #'string=)
     (cl-pushnew path ,pathvar :test #'string=)))

(defun py-shell-tramp-refresh-remote-path (vec paths)
  "Update VEC's remote-path giving PATHS priority."
  (let ((remote-path (tramp-get-connection-property vec "remote-path" nil)))
    (when remote-path
      (py-shell--add-to-path-with-priority remote-path paths)
      (tramp-set-connection-property vec "remote-path" remote-path)
      (tramp-set-remote-path vec))))

(defun py-shell-tramp-refresh-process-environment (vec env)
  "Update VEC's process environment with ENV."
  ;; Stolen from ‘tramp-open-connection-setup-interactive-shell’.
  (let ((env (append (when (fboundp 'tramp-get-remote-locale)
                       ;; Emacs<24.4 compat.
                       (list (tramp-get-remote-locale vec)))
		     (copy-sequence env)))
        (tramp-end-of-heredoc
         (if (boundp 'tramp-end-of-heredoc)
             tramp-end-of-heredoc
           (md5 tramp-end-of-output)))
	unset vars item)
    (while env
      (setq item (split-string (car env) "=" 'omit))
      (setcdr item (mapconcat 'identity (cdr item) "="))
      (if (and (stringp (cdr item)) (not (string-equal (cdr item) "")))
	  (push (format "%s %s" (car item) (cdr item)) vars)
	(push (car item) unset))
      (setq env (cdr env)))
    (when vars
      (tramp-send-command
       vec
       (format "while read var val; do export $var=$val; done <<'%s'\n%s\n%s"
	       tramp-end-of-heredoc
	       (mapconcat 'identity vars "\n")
	       tramp-end-of-heredoc)
       t))
    (when unset
      (tramp-send-command
       vec (format "unset %s" (mapconcat 'identity unset " ")) t))))

(defun py-shell-calculate-pythonpath ()
  "Calculate the PYTHONPATH using ‘py-shell-extra-pythonpaths’."
  (let ((pythonpath
         (split-string
          (or (getenv "PYTHONPATH") "") path-separator 'omit)))
    (py-shell--add-to-path-with-priority
     pythonpath py-shell-extra-pythonpaths)
    (mapconcat 'identity pythonpath path-separator)))

(defun py-shell-calculate-exec-path ()
  "Calculate ‘exec-path’.
Prepends ‘py-shell-exec-path’ and adds the binary directory
for virtualenv if ‘py-shell-virtualenv-root’ is set - this
will use the python interpreter from inside the virtualenv when
starting the shell.  If ‘default-directory’ points to a remote host,
the returned value appends ‘py-shell-remote-exec-path’ instead
of ‘exec-path’."
  (let ((new-path (copy-sequence
                   (if (file-remote-p default-directory)
                       py-shell-remote-exec-path
                     exec-path)))

        ;; Windows and POSIX systems use different venv directory structures
        (virtualenv-bin-dir (if (eq system-type 'windows-nt) "Scripts" "bin")))
    (py-shell--add-to-path-with-priority
     new-path py-shell-exec-path)
    (if (not py-shell-virtualenv-root)
        new-path
      (py-shell--add-to-path-with-priority
       new-path
       (list (expand-file-name virtualenv-bin-dir py-shell-virtualenv-root)))
      new-path)))

(defun py-shell-calculate-process-environment ()
  "Calculate ‘process-environment’ or ‘tramp-remote-process-environment’.
Prepends ‘py-shell-process-environment’, sets extra
pythonpaths from ‘py-shell-extra-pythonpaths’ and sets a few
virtualenv related vars.  If ‘default-directory’ points to a
remote host, the returned value is intended for
‘tramp-remote-process-environment’."
  (let* ((remote-p (file-remote-p default-directory))
         (process-environment (if remote-p
                                  tramp-remote-process-environment
                                process-environment))
         (virtualenv (when py-shell-virtualenv-root
                       (directory-file-name py-shell-virtualenv-root))))
    (dolist (env py-shell-process-environment)
      (pcase-let ((`(,key ,value) (split-string env "=")))
        (setenv key value)))
    (when py-shell-unbuffered
      (setenv "PYTHONUNBUFFERED" "1"))
    (when py-shell-extra-pythonpaths
      (setenv "PYTHONPATH" (py-shell-calculate-pythonpath)))
    (if (not virtualenv)
        process-environment
      (setenv "PYTHONHOME" nil)
      (setenv "VIRTUAL_ENV" virtualenv))
    process-environment))

(defmacro py-shell-with-environment (&rest body)
  "Modify shell environment during execution of BODY.
Temporarily sets ‘process-environment’ and ‘exec-path’ during
execution of body.  If ‘default-directory’ points to a remote
machine then modifies ‘tramp-remote-process-environment’ and
‘py-shell-remote-exec-path’ instead."
  (declare (indent 0) (debug (body)))
  (let ((vec (make-symbol "vec")))
    `(progn
       (let* ((,vec
               (when (file-remote-p default-directory)
                 (ignore-errors
                   (tramp-dissect-file-name default-directory 'noexpand))))
              (process-environment
               (if ,vec
                   process-environment
                 (py-shell-calculate-process-environment)))
              (exec-path
               (if ,vec
                   exec-path
                 (py-shell-calculate-exec-path)))
              (tramp-remote-process-environment
               (if ,vec
                   (py-shell-calculate-process-environment)
                 tramp-remote-process-environment)))
         (when (tramp-get-connection-process ,vec)
           ;; For already existing connections, the new exec path must
           ;; be re-set, otherwise it won't take effect.  One example
           ;; of such case is when remote dir-locals are read and
           ;; *then* subprocesses are triggered within the same
           ;; connection.
           (py-shell-tramp-refresh-remote-path
            ,vec (py-shell-calculate-exec-path))
           ;; The ‘tramp-remote-process-environment’ variable is only
           ;; effective when the started process is an interactive
           ;; shell, otherwise (like in the case of processes started
           ;; with ‘process-file’) the environment is not changed.
           ;; This makes environment modifications effective
           ;; unconditionally.
           (py-shell-tramp-refresh-process-environment
            ,vec tramp-remote-process-environment))
         ,(macroexp-progn body)))))

(defun py-shell-prompt-detect ()
  "Detect prompts for the current interpreter.
When prompts can be retrieved successfully from the
interpreter run with
‘py-python-command-args’, returns a list of
three elements, where the first two are input prompts and the
last one is an output prompt.  When no prompts can be detected
shows a warning with instructions to avoid hangs and returns nil.
When ‘py-shell-prompt-detect-p’ is nil avoids any
detection and just returns nil."
  (when py-shell-prompt-detect-p
    (py-shell-with-environment
      (let* ((code (concat
                    "import sys\n"
                    "ps = [getattr(sys, 'ps%s' % i, '') for i in range(1,4)]\n"
                    ;; JSON is built manually for compatibility
                    "ps_json = '\\n[\"%s\", \"%s\", \"%s\"]\\n' % tuple(ps)\n"
                    "print (ps_json)\n"
                    "sys.exit(0)\n"))
             ;; (interpreter py-shell-name)
             ;; (interpreter-arg py-python-command-args)
             (output
              (with-temp-buffer
                ;; TODO: improve error handling by using
                ;; ‘condition-case’ and displaying the error message to
                ;; the user in the no-prompts warning.
                (ignore-errors
                  (let ((code-file
                         ;; Python 2.x on Windows does not handle
                         ;; carriage returns in unbuffered mode.
                         (let ((inhibit-eol-conversion (getenv "PYTHONUNBUFFERED")))
                           (py-shell--save-temp-file code))))
                    (unwind-protect
                        ;; Use ‘process-file’ as it is remote-host friendly.
                        (process-file
                         py-shell-name
                         code-file
                         '(t nil)
                         nil
                         py-python-command-args)
                      ;; Try to cleanup
                      (delete-file code-file))))
                (buffer-string)))
             (prompts
              (catch 'prompts
                (dolist (line (split-string output "\n" t))
                  (let ((res
                         ;; Check if current line is a valid JSON array
                         (and (string= (substring line 0 2) "[\"")
                              (ignore-errors
                                ;; Return prompts as a list, not vector
                                (append (json-read-from-string line) nil)))))
                    ;; The list must contain 3 strings, where the first
                    ;; is the input prompt, the second is the block
                    ;; prompt and the last one is the output prompt.  The
                    ;; input prompt is the only one that can't be empty.
                    (when (and (= (length res) 3)
                               (cl-every #'stringp res)
                               (not (string= (car res) "")))
                      (throw 'prompts res))))
                nil)))
        (if (not prompts)
            (lwarn
             '(python py-shell-prompt-regexp)
             :warning
             (concat
              "Python shell prompts cannot be detected.\n"
              "If your emacs session hangs when starting python shells\n"
              "recover with ‘keyboard-quit’ and then try fixing the\n"
              "interactive flag for your interpreter by adjusting the\n"
              "‘py-python-command-args’ or add regexps\n"
              "matching shell prompts in the directory-local friendly vars:\n"
              "  + ‘py-shell-prompt-regexp’\n"
              "  + `py-shell-input-prompt-2-regexp'\n"
              "  + ‘py-shell-prompt-output-regexp’\n"
              "Or alternatively in:\n"
              "  + ‘py-shell-input-prompt-regexps’\n"
              "  + ‘py-shell-prompt-output-regexps’"))
          prompts)))))

(defun python-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))

(defun py-shell-prompt-validate-regexps ()
  "Validate all user provided regexps for prompts.
Signals ‘user-error’ if any of these vars contain invalid
regexps: ‘py-shell-prompt-regexp’,
`py-shell-input-prompt-2-regexp',
‘py-shell-prompt-pdb-regexp’,
‘py-shell-prompt-output-regexp’,
‘py-shell-input-prompt-regexps’,
‘py-shell-prompt-output-regexps’."
  (dolist (symbol (list 'py-shell-input-prompt-1-regexp
                        'py-shell-prompt-output-regexps
                        'py-shell-input-prompt-2-regexp
                        'py-shell-prompt-pdb-regexp))
    (dolist (regexp (let ((regexps (symbol-value symbol)))
                      (if (listp regexps)
                          regexps
                        (list regexps))))
      (when (not (python-util-valid-regexp-p regexp))
        (user-error "Invalid regexp %s in `%s'"
                    regexp symbol)))))

(defun py-shell-prompt-set-calculated-regexps ()
  "Detect and set input and output prompt regexps.

Build and set the values for input- and output-prompt regexp
using the values from ‘py-shell-prompt-regexp’,
`py-shell-input-prompt-2-regexp', ‘py-shell-prompt-pdb-regexp’,
‘py-shell-prompt-output-regexp’, ‘py-shell-input-prompt-regexps’,
 and detected prompts from ‘py-shell-prompt-detect’."
  (when (not (and py-shell--prompt-calculated-input-regexp
                  py-shell--prompt-calculated-output-regexp))
    (let* ((detected-prompts (py-shell-prompt-detect))
           (input-prompts nil)
           (output-prompts nil)
           (build-regexp
            (lambda (prompts)
              (concat "^\\("
                      (mapconcat #'identity
                                 (sort prompts
                                       (lambda (a b)
                                         (let ((length-a (length a))
                                               (length-b (length b)))
                                           (if (= length-a length-b)
                                               (string< a b)
                                             (> (length a) (length b))))))
                                 "\\|")
                      "\\)"))))
      ;; Validate ALL regexps
      (py-shell-prompt-validate-regexps)
      ;; Collect all user defined input prompts
      (dolist (prompt (append py-shell-input-prompt-regexps
                              (list py-shell-input-prompt-2-regexp
                                    py-shell-prompt-pdb-regexp)))
        (cl-pushnew prompt input-prompts :test #'string=))
      ;; Collect all user defined output prompts
      (dolist (prompt (cons py-shell-prompt-output-regexp
                            py-shell-prompt-output-regexps))
        (cl-pushnew prompt output-prompts :test #'string=))
      ;; Collect detected prompts if any
      (when detected-prompts
        (dolist (prompt (butlast detected-prompts))
          (setq prompt (regexp-quote prompt))
          (cl-pushnew prompt input-prompts :test #'string=))
        (setq py-shell--block-prompt (nth 1 detected-prompts))
        (cl-pushnew (regexp-quote
                     (car (last detected-prompts)))
                    output-prompts :test #'string=))
      ;; Set input and output prompt regexps from collected prompts
      (setq py-shell--prompt-calculated-input-regexp
            (funcall build-regexp input-prompts)
            py-shell--prompt-calculated-output-regexp
            (funcall build-regexp output-prompts)))))

(defun py-shell-output-filter (strg)
  "Filter used in ‘py-shell-send-string-no-output’ to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
‘py-shell-output-filter-buffer’ and stops receiving it after
detecting a prompt at the end of the buffer."
  (let ((py-shell--prompt-calculated-output-regexp
	 (or py-shell--prompt-calculated-output-regexp (py-shell-prompt-set-calculated-regexps))))
    (setq
     strg (ansi-color-filter-apply strg)
     py-shell-output-filter-buffer
     (concat py-shell-output-filter-buffer strg))
    (when (py-shell-comint-end-of-output-p
	   py-shell-output-filter-buffer)
      ;; Output ends when ‘py-shell-output-filter-buffer’ contains
      ;; the prompt attached at the end of it.
      (setq py-shell-output-filter-in-progress nil
	    py-shell-output-filter-buffer
	    (substring py-shell-output-filter-buffer
		       0 (match-beginning 0)))
      (when (string-match
	     py-shell--prompt-calculated-output-regexp
	     py-shell-output-filter-buffer)
	;; Some shells, like IPython might append a prompt before the
	;; output, clean that.
	(setq py-shell-output-filter-buffer
	      (substring py-shell-output-filter-buffer (match-end 0)))))
    ""))

(defun py--fast-send-string-no-output-intern (strg proc limit output-buffer no-output)
  (let (erg)
    (with-current-buffer output-buffer
      ;; (when py-debug-p (switch-to-buffer (current-buffer)))
      ;; (erase-buffer)
      (process-send-string proc strg)
      (or (string-match "\n$" strg)
	  (process-send-string proc "\n")
	  (goto-char (point-max))
	  )
      (cond (no-output
	     (delete-region (field-beginning) (field-end))
	     ;; (erase-buffer)
	     ;; (delete-region (point-min) (line-beginning-position))
	     )
	    (t
	     (if
		 (setq erg (py--fetch-result output-buffer limit strg))
		 (setq py-result (py--filter-result erg))
	       (dotimes (_ 3) (unless (setq erg (py--fetch-result output-buffer limit))(sit-for 1 t)))
	       (or (py--fetch-result output-buffer limit))
	       (error "py--fast-send-string-no-output-intern: py--fetch-result: no result")))))))

(defun py-execute-string (strg &optional process result no-output orig output-buffer fast argprompt args dedicated shell exception-buffer split switch internal)
   "Evaluate STRG in Python PROCESS.

With optional Arg PROCESS send to process.
With optional Arg RESULT store result in var ‘py-result’, also return it.
With optional Arg NO-OUTPUT don't display any output
With optional Arg ORIG deliver original position.
With optional Arg OUTPUT-BUFFER specify output-buffer"
  (interactive "sPython command: ")
  (save-excursion
    (let* ((buffer (or output-buffer (or (and process (buffer-name (process-buffer process))) (buffer-name (py-shell argprompt args dedicated shell output-buffer fast exception-buffer split switch internal)))))
	   (proc (or process (get-buffer-process buffer)))
	   ;; nil nil nil nil (buffer-name buffer))))
	   (orig (or orig (point)))
   	   (limit (ignore-errors (marker-position (process-mark proc)))))
      (cond ((and no-output fast)
	     (py--fast-send-string-no-output-intern strg proc limit buffer no-output))
	    (no-output
	     (py-send-string-no-output strg proc))
	    ((and (string-match ".\n+." strg) (string-match "^[Ii]"
							    ;; (buffer-name buffer)
							    buffer
							    ))  ;; multiline
	     (let* ((temp-file-name (py-temp-file-name strg))
		    (file-name (or (buffer-file-name) temp-file-name)))
	       (py-execute-file file-name proc)))
	    (t (with-current-buffer buffer
		 (comint-send-string proc strg)
		 (when (or (not (string-match "\n\\'" strg))
			   (string-match "\n[ \t].*\n?\\'" strg))
		   (comint-send-string proc "\n"))
		 (sit-for py-python-send-delay)
		 (cond (result
			(setq py-result
			      (py--fetch-result buffer limit strg)))
		       (no-output
			(and orig (py--cleanup-shell orig buffer))))))))))

(defun py--execute-file-base (filename &optional proc cmd procbuf origline fast interactivep)
  "Send to Python interpreter process PROC.

In Python version 2.. \"execfile('FILENAME')\".

Takes also CMD PROCBUF ORIGLINE NO-OUTPUT.

Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
‘kill-output-from-shell’ does The Right Thing.
Returns position where output starts."
  (let* ((filename (expand-file-name filename))
	 (buffer (or procbuf (and proc (process-buffer proc)) (py-shell nil nil nil nil nil fast)))
	 (proc (or proc (get-buffer-process buffer)))
	 (limit (marker-position (process-mark proc)))
	 (cmd (or cmd (py-execute-file-command filename)))
	 erg)
    (if fast
	(process-send-string proc cmd)
      (py-execute-string cmd proc))
    ;; (message "%s" (current-buffer))
    (with-current-buffer buffer
      (when (or py-return-result-p py-store-result-p)
	(setq erg (py--postprocess buffer origline limit cmd filename))
	(if py-error
	    (setq py-error (prin1-to-string py-error))
	  erg)))
    (when (or interactivep
	      (or py-switch-buffers-on-execute-p py-split-window-on-execute))
      (py--shell-manage-windows buffer (find-file-noselect filename) py-split-window-on-execute py-switch-buffers-on-execute-p))))

(defun py-restore-window-configuration ()
  "Restore ‘py-restore-window-configuration’."
  (let (val)
    (and (setq val (get-register py--windows-config-register))(and (consp val) (window-configuration-p (car val))(markerp (cadr val)))(marker-buffer (cadr val))
	 (jump-to-register py--windows-config-register))))

(defun py-toggle-split-window-function ()
  "If window is splitted vertically or horizontally.

When code is executed and ‘py-split-window-on-execute’ is t,
the result is displays in an output-buffer, \"\*Python\*\" by default.

Customizable variable ‘py-split-windows-on-execute-function’
tells how to split the screen."
  (interactive)
  (if (eq 'split-window-vertically py-split-windows-on-execute-function)
      (setq py-split-windows-on-execute-function'split-window-horizontally)
    (setq py-split-windows-on-execute-function 'split-window-vertically))
  (when (and py-verbose-p (called-interactively-p 'any))
    (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function)))

(defun py--manage-windows-set-and-switch (buffer)
  "Switch to output BUFFER, go to ‘point-max’.

Internal use"
  (set-buffer buffer)
  (goto-char (process-mark (get-buffer-process (current-buffer)))))

(defun py--alternative-split-windows-on-execute-function ()
  "Toggle split-window-horizontally resp. vertically."
  (if (eq py-split-windows-on-execute-function 'split-window-vertically)
      'split-window-horizontally
    'split-window-vertically))

(defun py--get-splittable-window ()
  "Search ‘window-list’ for a window suitable for splitting."
  (or (and (window-left-child)(split-window (window-left-child)))
      (and (window-top-child)(split-window (window-top-child)))
      (and (window-parent)(ignore-errors (split-window (window-parent))))
      (and (window-atom-root)(split-window (window-atom-root)))))

(defun py--manage-windows-split (buffer)
  "If one window, split BUFFER.

according to ‘py-split-windows-on-execute-function’."
  (interactive)
  (set-buffer buffer)
  (or
   ;; (split-window (selected-window) nil ’below)
   (ignore-errors (funcall py-split-windows-on-execute-function))
   ;; If call didn't succeed according to settings of
   ;; ‘split-height-threshold’, ‘split-width-threshold’
   ;; resp. ‘window-min-height’, ‘window-min-width’
   ;; try alternative split
   (unless (ignore-errors (funcall (py--alternative-split-windows-on-execute-function)))
     ;; if alternative split fails, look for larger window
     (py--get-splittable-window)
     (ignore-errors (funcall (py--alternative-split-windows-on-execute-function))))))

;; (defun py--display-windows (output-buffer)
;;     "Otherwise new window appears above"
;;       (display-buffer output-buffer)
;;       (select-window py-exception-window))

(defun py--split-t-not-switch-wm (output-buffer number-of-windows exception-buffer)
  (unless (window-live-p output-buffer)
    (with-current-buffer (get-buffer exception-buffer)

      (when (< number-of-windows py-split-window-on-execute-threshold)
	(unless
	    (member (get-buffer-window output-buffer) (window-list))
	  (py--manage-windows-split exception-buffer)))
      (display-buffer output-buffer t)
      (switch-to-buffer exception-buffer)
      )))

(defun py--shell-manage-windows (output-buffer &optional exception-buffer split switch)
  "Adapt or restore window configuration from OUTPUT-BUFFER.

Optional EXCEPTION-BUFFER SPLIT SWITCH
Return nil."
  (let* ((exception-buffer (or exception-buffer (other-buffer)))
	 (old-window-list (window-list))
	 (number-of-windows (length old-window-list))
	 (split (or split py-split-window-on-execute))
	 (switch
	  (or py-switch-buffers-on-execute-p switch py-pdbtrack-tracked-buffer)))
    ;; (output-buffer-displayed-p)
    (cond
     (py-keep-windows-configuration
      (py-restore-window-configuration)
      (set-buffer output-buffer)
      (goto-char (point-max)))
     ((and (eq split 'always)
	   switch)
      (if (member (get-buffer-window output-buffer) (window-list))
	  ;; (delete-window (get-buffer-window output-buffer))
	  (select-window (get-buffer-window output-buffer))
	(py--manage-windows-split exception-buffer)
	;; otherwise new window appears above
	(save-excursion
	  (other-window 1)
	  (switch-to-buffer output-buffer))
	(display-buffer exception-buffer)))
     ((and
       (eq split 'always)
       (not switch))
      (if (member (get-buffer-window output-buffer) (window-list))
	  (select-window (get-buffer-window output-buffer))
	(py--manage-windows-split exception-buffer)
	(display-buffer output-buffer)
	(pop-to-buffer exception-buffer)))
     ((and
       (eq split 'just-two)
       switch)
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (py--manage-windows-split exception-buffer)
      ;; otherwise new window appears above
      (other-window 1)
      (set-buffer output-buffer)
      (switch-to-buffer (current-buffer)))
     ((and
       (eq split 'just-two)
       (not switch))
      (switch-to-buffer exception-buffer)
      (delete-other-windows)
      (unless
	  (member (get-buffer-window output-buffer) (window-list))
	(py--manage-windows-split exception-buffer))
      ;; Fixme: otherwise new window appears above
      (save-excursion
	(other-window 1)
	(pop-to-buffer output-buffer)
	(goto-char (point-max))
	(other-window 1)))
     ((and
       split
       (not switch))
      ;; https://bugs.launchpad.net/python-mode/+bug/1478122
      ;; > If the shell is visible in any of the windows it should re-use that window
      ;; > I did double check and py-keep-window-configuration is nil and split is t.
      (py--split-t-not-switch-wm output-buffer number-of-windows exception-buffer))
     ((and split switch)
      (unless
	  (member (get-buffer-window output-buffer) (window-list))
	(py--manage-windows-split exception-buffer))
      ;; Fixme: otherwise new window appears above
      ;; (save-excursion
      ;; (other-window 1)
      ;; (pop-to-buffer output-buffer)
      ;; [Bug 1579309] python buffer window on top when using python3
      (set-buffer output-buffer)
      (switch-to-buffer output-buffer)
      (goto-char (point-max))
      ;; (other-window 1)
      )
     ((not switch)
      (let (pop-up-windows)
	(py-restore-window-configuration))))))

(defun py-execute-file (filename &optional proc)
  "When called interactively, user is prompted for FILENAME."
  (interactive "fFilename: ")
  (let (;; postprocess-output-buffer might want origline
        (origline 1)
        (py-exception-buffer filename)
        erg)
    (if (file-readable-p filename)
        (if py-store-result-p
            (setq erg (py--execute-file-base (expand-file-name filename) nil nil nil origline))
          (py--execute-file-base (expand-file-name filename) proc))
      (message "%s not readable. %s" filename "Do you have write permissions?"))
    (py--shell-manage-windows py-output-buffer py-exception-buffer nil
                              (or (called-interactively-p 'interactive)))
    erg))

(defun py-send-string-no-output (strg &optional process buffer-name)
  "Send STRING to PROCESS and inhibit output.

Return the output."
  (let* ((proc (or process (py--get-process)))
	 (buffer (or buffer-name (if proc (buffer-name (process-buffer proc)) (py-shell))))
         (comint-preoutput-filter-functions
          '(py-shell-output-filter))
         (py-shell-output-filter-in-progress t)
         (inhibit-quit t)
	 (delay (py--which-delay-process-dependent buffer))
	 temp-file-name)
    (or
     (with-local-quit
       (if (and (string-match ".\n+." strg) (string-match "^\*[Ii]" buffer))  ;; IPython or multiline
           (let ((file-name (or (buffer-file-name) (setq temp-file-name (py-temp-file-name strg)))))
	     (py-execute-file file-name proc)
	     (when temp-file-name (delete-file temp-file-name)))
	 (py-shell-send-string strg proc))
       ;; (switch-to-buffer buffer)
       ;; (accept-process-output proc 9)
       (while py-shell-output-filter-in-progress
         ;; ‘py-shell-output-filter’ takes care of setting
         ;; ‘py-shell-output-filter-in-progress’ to NIL after it
         ;; detects end of output.
         (accept-process-output proc delay))
       (prog1
           py-shell-output-filter-buffer
         (setq py-shell-output-filter-buffer nil)))
     (with-current-buffer (process-buffer proc)
       (comint-interrupt-subjob)))))

(defun py--leave-backward-string-list-and-comment-maybe (pps)
  (while (or (and (nth 8 pps) (goto-char (nth 8 pps)))
             (and (nth 1 pps) (goto-char (nth 1 pps)))
             (and (nth 4 pps) (goto-char (nth 4 pps))))
    ;; (back-to-indentation)
    (when (or (looking-at comment-start)(member (char-after) (list ?\" ?')))
      (skip-chars-backward " \t\r\n\f"))
    (setq pps (parse-partial-sexp (point-min) (point)))))

(defun py-set-ipython-completion-command-string (shell)
  "Set and return ‘py-ipython-completion-command-string’ according to SHELL."
  (interactive)
  (let* ((ipython-version (py-ipython--which-version shell)))
    (if (string-match "[0-9]" ipython-version)
        (setq py-ipython-completion-command-string
              (cond ((string-match "^[^0].+" ipython-version)
		     py-ipython0.11-completion-command-string)
                    ((string-match "^0.1[1-3]" ipython-version)
                     py-ipython0.11-completion-command-string)
                    ((string= "^0.10" ipython-version)
                     py-ipython0.10-completion-command-string)))
      (error ipython-version))))

(defun py-ipython--module-completion-import (proc)
  "Import module-completion according to PROC."
  (interactive)
  (let ((ipython-version (shell-command-to-string (concat py-shell-name " -V"))))
    (when (and (string-match "^[0-9]" ipython-version)
               (string-match "^[^0].+" ipython-version))
      (process-send-string proc "from IPython.core.completerlib import module_completion"))))

(defun py--compose-buffer-name-initials (liste)
  (let (erg)
    (dolist (ele liste)
      (unless (string= "" ele)
	(setq erg (concat erg (char-to-string (aref ele 0))))))
    erg))

(defun py--remove-home-directory-from-list (liste)
  "Prepare for compose-buffer-name-initials according to LISTE."
  (let ((case-fold-search t)
	(liste liste)
	erg)
    (if (listp (setq erg (split-string (expand-file-name "~") "\/")))
	erg
      (setq erg (split-string (expand-file-name "~") "\\\\")))
     (while erg
      (when (member (car erg) liste)
	(setq liste (cdr (member (car erg) liste))))
      (setq erg (cdr erg)))
    (butlast liste)))

(defun py--prepare-shell-name (erg)
  "Provide a readable shell name by capitalizing etc."
  (cond ((string-match "^ipython" erg)
	 (replace-regexp-in-string "ipython" "IPython" erg))
	((string-match "^jython" erg)
	 (replace-regexp-in-string "jython" "Jython" erg))
	((string-match "^python" erg)
	 (replace-regexp-in-string "python" "Python" erg))
	((string-match "^python2" erg)
	 (replace-regexp-in-string "python2" "Python2" erg))
	((string-match "^python3" erg)
	 (replace-regexp-in-string "python3" "Python3" erg))
	((string-match "^pypy" erg)
	 (replace-regexp-in-string "pypy" "PyPy" erg))
	(t erg)))

(defun py--choose-buffer-name (&optional name dedicated fast-process)
  "Return an appropriate NAME to display in modeline.

Optional DEDICATED FAST-PROCESS
SEPCHAR is the file-path separator of your system."
  (let* ((name-first (or name py-shell-name))
	 (erg (when name-first (if (stringp name-first) name-first (prin1-to-string name-first))))
	 (fast-process (or fast-process py-fast-process-p))
	 prefix)
    (when (string-match "^py-" erg)
      (setq erg (nth 1 (split-string erg "-"))))
    ;; remove home-directory from prefix to display
    (unless py-modeline-acronym-display-home-p
      (save-match-data
	(let ((case-fold-search t))
	  (when (string-match (concat ".*" (expand-file-name "~")) erg)
	    (setq erg (replace-regexp-in-string (concat "^" (expand-file-name "~")) "" erg))))))
    (if (or (and (setq prefix (split-string erg "\\\\"))
		 (< 1 (length prefix)))
	    (and (setq prefix (split-string erg "\/"))
		 (< 1 (length prefix))))
	(progn
	  ;; exect something like default py-shell-name
	  (setq erg (car (last prefix)))
	  (unless py-modeline-acronym-display-home-p
	    ;; home-directory may still inside
	    (setq prefix (py--remove-home-directory-from-list prefix))
	    (setq prefix (py--compose-buffer-name-initials prefix))))
      (setq erg (or erg py-shell-name))
      (setq prefix nil))
    (when fast-process (setq erg (concat erg " Fast")))
    (setq erg
          (py--prepare-shell-name erg))
    (when (or dedicated py-dedicated-process-p)
      (setq erg (make-temp-name (concat erg "-"))))
    (cond ((and prefix (string-match "^\*" erg))
           (setq erg (replace-regexp-in-string "^\*" (concat "*" prefix " ") erg)))
          (prefix
           (setq erg (concat "*" prefix " " erg "*")))
          (t (unless (string-match "^\*" erg) (setq erg (concat "*" erg "*")))))
    erg))

(defun py-shell (&optional argprompt args dedicated shell buffer fast exception-buffer split switch internal)
  "Connect process to BUFFER.

Start an interpreter according to ‘py-shell-name’ or SHELL.

Optional ARGPROMPT: with \\[universal-argument] start in a new
dedicated shell.

Optional ARGS: Specify other than default command args.

Optional DEDICATED: start in a new dedicated shell.
Optional string SHELL overrides default ‘py-shell-name’.
Optional string BUFFER allows a name, the Python process is connected to
Optional FAST: no fontification in process-buffer.
Optional EXCEPTION-BUFFER: point to error.
Optional SPLIT: see var ‘py-split-window-on-execute’
Optional SWITCH: see var ‘py-switch-buffers-on-execute-p’
Optional INTERNAL shell will be invisible for users

Reusing existing processes: For a given buffer and same values,
if a process is already running for it, it will do nothing.

Runs the hook ‘py-shell-mode-hook’ after
‘comint-mode-hook’ is run.  (Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive "p")
  (let* ((interactivep (and argprompt (eq 1 (prefix-numeric-value argprompt))))
	 (fast (unless (eq major-mode 'org-mode)
		 (or fast py-fast-process-p)))
	 (dedicated (or (eq 4 (prefix-numeric-value argprompt)) dedicated py-dedicated-process-p))
	 (shell (if shell
		    (pcase shell
                      ("python"
                       (or (and (executable-find shell) shell)
                           (and (executable-find "python3") "python3")))
		      (_ (if (executable-find shell)
			     shell
		           (error (concat "py-shell: Can't see an executable for `"shell "' on your system. Maybe needs a link?")))))
		  (py-choose-shell)))
	 (args (or args (py--provide-command-args shell fast)))
         ;; Make sure a new one is created if required
	 (buffer-name
	  (or buffer
              (and python-mode-v5-behavior-p (get-buffer-create "*Python Output*"))
	      (py--choose-buffer-name shell dedicated fast)))
	 (proc (get-buffer-process buffer-name))
	 (done nil)
	 (delay nil)
	 (buffer
	  (or
	   (and (ignore-errors (process-buffer proc))
		(save-excursion (with-current-buffer (process-buffer proc)
				  ;; point might not be left there
				  (goto-char (point-max))
				  (push-mark)
				  (setq done t)
				  (process-buffer proc))))
	   (save-excursion
	     (py-shell-with-environment
	       (if fast
		   (process-buffer (apply 'start-process shell buffer-name shell args))
		 (apply #'make-comint-in-buffer shell buffer-name
			shell nil args))))))
	 ;; (py-shell-prompt-detect-p (or (string-match "^\*IP" buffer) py-shell-prompt-detect-p))
         )
    (setq py-output-buffer (buffer-name (if python-mode-v5-behavior-p (get-buffer "*Python Output*") buffer)))
    (unless done
      (with-current-buffer buffer
	(setq delay (py--which-delay-process-dependent buffer-name))
	(unless fast
	  (when interactivep
            (setq py-shell-mode-syntax-table python-mode-syntax-table)
	    (cond ((string-match "^.I" buffer-name)
		   (message "Waiting according to `py-ipython-send-delay:' %s" delay))
		  ((string-match "^.+3" buffer-name)
		   (message "Waiting according to `py-python3-send-delay:' %s" delay))))
	  (setq py-modeline-display (py--update-lighter buffer-name))
	  ;; (sit-for delay t)
          )))
    (if (setq proc (get-buffer-process buffer))
	(progn
	  (with-current-buffer buffer
	    (unless (or done fast) (py-shell-mode))
	    (and internal (set-process-query-on-exit-flag proc nil)))
	  (when (or interactivep
		    (or switch py-switch-buffers-on-execute-p py-split-window-on-execute))
	    (py--shell-manage-windows buffer exception-buffer split (or interactivep switch)))
	  buffer)
      (error (concat "py-shell:" (py--fetch-error py-output-buffer))))))

;; python-components-rx

;; The `rx--translate...' functions below return (REGEXP . PRECEDENCE),
;; where REGEXP is a list of string expressions that will be
;; concatenated into a regexp, and PRECEDENCE is one of
;;
;;  t    -- can be used as argument to postfix operators (eg. "a")
;;  seq  -- can be concatenated in sequence with other seq or higher (eg. "ab")
;;  lseq -- can be concatenated to the left of rseq or higher (eg. "^a")
;;  rseq -- can be concatenated to the right of lseq or higher (eg. "a$")
;;  nil  -- can only be used in alternatives (eg. "a\\|b")
;;
;; They form a lattice:
;;
;;           t          highest precedence
;;           |
;;          seq
;;         /   \
;;      lseq   rseq
;;         \   /
;;          nil         lowest precedence


(defconst rx--char-classes
  '((digit         . digit)
    (numeric       . digit)
    (num           . digit)
    (control       . cntrl)
    (cntrl         . cntrl)
    (hex-digit     . xdigit)
    (hex           . xdigit)
    (xdigit        . xdigit)
    (blank         . blank)
    (graphic       . graph)
    (graph         . graph)
    (printing      . print)
    (print         . print)
    (alphanumeric  . alnum)
    (alnum         . alnum)
    (letter        . alpha)
    (alphabetic    . alpha)
    (alpha         . alpha)
    (ascii         . ascii)
    (nonascii      . nonascii)
    (lower         . lower)
    (lower-case    . lower)
    (punctuation   . punct)
    (punct         . punct)
    (space         . space)
    (whitespace    . space)
    (white         . space)
    (upper         . upper)
    (upper-case    . upper)
    (word          . word)
    (wordchar      . word)
    (unibyte       . unibyte)
    (multibyte     . multibyte))
  "Alist mapping rx symbols to character classes.
Most of the names are from SRE.")

(defvar rx-constituents nil
  "Alist of old-style rx extensions, for compatibility.
For new code, use ‘rx-define’, ‘rx-let’ or ‘rx-let-eval’.

Each element is (SYMBOL . DEF).

If DEF is a symbol, then SYMBOL is an alias of DEF.

If DEF is a string, then SYMBOL is a plain rx symbol defined as the
   regexp string DEF.

If DEF is a list on the form (FUN MIN-ARGS MAX-ARGS PRED), then
   SYMBOL is an rx form with at least MIN-ARGS and at most
   MAX-ARGS arguments.  If MAX-ARGS is nil, then there is no upper limit.
   FUN is a function taking the entire rx form as single argument
   and returning the translated regexp string.
   If PRED is non-nil, it is a predicate that all actual arguments must
   satisfy.")

(defvar rx--local-definitions nil
  "Alist of dynamic local rx definitions.
Each entry is:
 (NAME DEF)      -- NAME is an rx symbol defined as the rx form DEF.
 (NAME ARGS DEF) -- NAME is an rx form with arglist ARGS, defined
                    as the rx form DEF (which can contain members of ARGS).")

(defsubst rx--lookup-def (name)
  "Current definition of NAME: (DEF) or (ARGS DEF), or nil if none."
  (or (cdr (assq name rx--local-definitions))
      (get name 'rx-definition)))

(defun rx--expand-def (form)
  "FORM expanded (once) if a user-defined construct; otherwise nil."
  (cond ((symbolp form)
         (let ((def (rx--lookup-def form)))
           (and def
                (if (cdr def)
                    (error "Not an ‘rx’ symbol definition: %s" form)
                  (car def)))))
        ((and (consp form) (symbolp (car form)))
         (let* ((op (car form))
                (def (rx--lookup-def op)))
           (and def
                (if (cdr def)
                    (rx--expand-template
                     op (cdr form) (nth 0 def) (nth 1 def))
                  (error "Not an ‘rx’ form definition: %s" op)))))))

;; TODO: Additions to consider:
;; - A construct like ‘or’ but without the match order guarantee,
;;   maybe ‘unordered-or’.  Useful for composition or generation of
;;   alternatives; permits more effective use of regexp-opt.

(defun rx--translate-symbol (sym)
  "Translate an rx symbol.  Return (REGEXP . PRECEDENCE)."
  (pcase sym
    ;; Use ‘list’ instead of a quoted list to wrap the strings here,
    ;; since the return value may be mutated.
    ((or 'nonl 'not-newline 'any) (cons (list ".") t))
    ((or 'anychar 'anything)      (cons (list "[^z-a]") t))
    ('unmatchable                 (rx--empty))
    ((or 'bol 'line-start)        (cons (list "^") 'lseq))
    ((or 'eol 'line-end)          (cons (list "$") 'rseq))
    ((or 'bos 'string-start 'bot 'buffer-start) (cons (list "\\`") t))
    ((or 'eos 'string-end   'eot 'buffer-end)   (cons (list "\\'") t))
    ('point                       (cons (list "\\=") t))
    ((or 'bow 'word-start)        (cons (list "\\<") t))
    ((or 'eow 'word-end)          (cons (list "\\>") t))
    ('word-boundary               (cons (list "\\b") t))
    ('not-word-boundary           (cons (list "\\B") t))
    ('symbol-start                (cons (list "\\_<") t))
    ('symbol-end                  (cons (list "\\_>") t))
    ('not-wordchar                (cons (list "\\W") t))
    (_
     (cond
      ((let ((class (cdr (assq sym rx--char-classes))))
         (and class (cons (list (concat "[[:" (symbol-name class) ":]]")) t))))

      ((let ((expanded (rx--expand-def sym)))
         (and expanded (rx--translate expanded))))

      ;; For compatibility with old rx.
      ((let ((entry (assq sym rx-constituents)))
         (and (progn
                (while (and entry (not (stringp (cdr entry))))
                  (setq entry
                        (if (symbolp (cdr entry))
                            ;; Alias for another entry.
                            (assq (cdr entry) rx-constituents)
                          ;; Wrong type, try further down the list.
                          (assq (car entry)
                                (cdr (memq entry rx-constituents))))))
                entry)
              (cons (list (cdr entry)) nil))))
      (t (error "Unknown rx symbol `%s'" sym))))))

(defun rx--enclose (left-str rexp right-str)
  "Bracket REXP by LEFT-STR and RIGHT-STR."
  (append (list left-str) rexp (list right-str)))

(defun rx--bracket (rexp)
  (rx--enclose "\\(?:" rexp "\\)"))

(defun rx--sequence (left right)
  "Return the sequence (concatenation) of two translated items,
each on the form (REGEXP . PRECEDENCE), returning (REGEXP . PRECEDENCE)."
  ;; Concatenation rules:
  ;;  seq  ++ seq  -> seq
  ;;  lseq ++ seq  -> lseq
  ;;  seq  ++ rseq -> rseq
  ;;  lseq ++ rseq -> nil
  (cond ((not (car left)) right)
        ((not (car right)) left)
        (t
         (let ((l (if (memq (cdr left) '(nil rseq))
                      (cons (rx--bracket (car left)) t)
                    left))
               (r (if (memq (cdr right) '(nil lseq))
                      (cons (rx--bracket (car right)) t)
                    right)))
           (cons (append (car l) (car r))
                 (if (eq (cdr l) 'lseq)
                     (if (eq (cdr r) 'rseq)
                         nil                   ; lseq ++ rseq
                       'lseq)                  ; lseq ++ seq
                   (if (eq (cdr r) 'rseq)
                       'rseq                   ; seq ++ rseq
                     'seq)))))))               ; seq ++ seq

(defun rx--translate-seq (body)
  "Translate a sequence of zero or more rx items.
Return (REGEXP . PRECEDENCE)."
  (if body
      (let* ((items (mapcar #'rx--translate body))
             (result (car items)))
        (dolist (item (cdr items))
          (setq result (rx--sequence result item)))
        result)
    (cons nil 'seq)))

(defun rx--empty ()
  "Regexp that never matches anything."
  (cons (list regexp-unmatchable) 'seq))

;; ‘cl-every’ replacement to avoid bootstrapping problems.
(defun rx--every (pred list)
  "Whether PRED is true for every element of LIST."
  (while (and list (funcall pred (car list)))
    (setq list (cdr list)))
  (null list))

(defun rx--foldl (f x l)
  "(F (F (F X L0) L1) L2) ...
Left-fold the list L, starting with X, by the binary function F."
  (while l
    (setq x (funcall f x (car l)))
    (setq l (cdr l)))
  x)

(defun rx--normalize-or-arg (form)
  "Normalize the ‘or’ argument FORM.
Characters become strings, user-definitions and ‘eval’ forms are expanded,
and ‘or’ forms are normalized recursively."
  (cond ((characterp form)
         (char-to-string form))
        ((and (consp form) (memq (car form) '(or |)))
         (cons (car form) (mapcar #'rx--normalize-or-arg (cdr form))))
        ((and (consp form) (eq (car form) 'eval))
         (rx--normalize-or-arg (rx--expand-eval (cdr form))))
        (t
         (let ((expanded (rx--expand-def form)))
           (if expanded
               (rx--normalize-or-arg expanded)
             form)))))

(defun rx--all-string-or-args (body)
  "If BODY only consists of strings or such ‘or’ forms, return all the strings.
Otherwise throw ‘rx--nonstring’."
  (mapcan (lambda (form)
            (cond ((stringp form) (list form))
                  ((and (consp form) (memq (car form) '(or |)))
                   (rx--all-string-or-args (cdr form)))
                  (t (throw 'rx--nonstring nil))))
          body))

(defun rx--translate-or (body)
  "Translate an or-pattern of zero or more rx items.
Return (REGEXP . PRECEDENCE)."
  ;; FIXME: Possible improvements:
  ;;
  ;; - Flatten sub-patterns first: (or (or A B) (or C D)) -> (or A B C D)
  ;;   Then call regexp-opt on runs of string arguments. Example:
  ;;   (or (+ digit) "CHARLIE" "CHAN" (+ blank))
  ;;   -> (or (+ digit) (or "CHARLIE" "CHAN") (+ blank))
  ;;
  ;; - Optimize single-character alternatives better:
  ;;     * classes: space, alpha, ...
  ;;     * (syntax S), for some S (whitespace, word)
  ;;   so that (or "@" "%" digit (any "A-Z" space) (syntax word))
  ;;        -> (any "@" "%" digit "A-Z" space word)
  ;;        -> "[A-Z@%[:digit:][:space:][:word:]]"
  (cond
   ((null body)                    ; No items: a never-matching regexp.
    (rx--empty))
   ((null (cdr body))              ; Single item.
    (rx--translate (car body)))
   (t
    (let* ((args (mapcar #'rx--normalize-or-arg body))
           (all-strings (catch 'rx--nonstring (rx--all-string-or-args args))))
      (cond
       (all-strings                       ; Only strings.
        (cons (list (regexp-opt all-strings nil))
              t))
       ((rx--every #'rx--charset-p args)  ; All charsets.
        (rx--translate-union nil args))
       (t
        (cons (append (car (rx--translate (car args)))
                      (mapcan (lambda (item)
                                (cons "\\|" (car (rx--translate item))))
                              (cdr args)))
              nil)))))))

(defun rx--charset-p (form)
  "Whether FORM looks like a charset, only consisting of character intervals
and set operations."
  (or (and (consp form)
           (or (and (memq (car form) '(any in char))
                    (rx--every (lambda (x) (not (symbolp x))) (cdr form)))
               (and (memq (car form) '(not or | intersection))
                    (rx--every #'rx--charset-p (cdr form)))))
      (characterp form)
      (and (stringp form) (= (length form) 1))
      (and (or (symbolp form) (consp form))
           (let ((expanded (rx--expand-def form)))
             (and expanded
                  (rx--charset-p expanded))))))

(defun rx--string-to-intervals (str)
  "Decode STR as intervals: A-Z becomes (?A . ?Z), and the single
character X becomes (?X . ?X).  Return the intervals in a list."
  ;; We could just do string-to-multibyte on the string and work with
  ;; that instead of this ‘decode-char’ workaround.
  (let ((decode-char
         (if (multibyte-string-p str)
             #'identity
           #'unibyte-char-to-multibyte))
        (len (length str))
        (i 0)
        (intervals nil))
    (while (< i len)
      (cond ((and (< i (- len 2))
                  (= (aref str (1+ i)) ?-))
             ;; Range.
             (let ((start (funcall decode-char (aref str i)))
                   (end   (funcall decode-char (aref str (+ i 2)))))
               (cond ((and (<= start #x7f) (>= end #x3fff80))
                      ;; Ranges between ASCII and raw bytes are split to
                      ;; avoid having them absorb Unicode characters
                      ;; caught in-between.
                      (push (cons start #x7f) intervals)
                      (push (cons #x3fff80 end) intervals))
                     ((<= start end)
                      (push (cons start end) intervals))
                     (t
                      (error "Invalid rx ‘any’ range: %s"
                             (substring str i (+ i 3)))))
               (setq i (+ i 3))))
            (t
             ;; Single character.
             (let ((char (funcall decode-char (aref str i))))
               (push (cons char char) intervals))
             (setq i (+ i 1)))))
    intervals))

(defun rx--condense-intervals (intervals)
  "Merge adjacent and overlapping intervals by mutation, preserving the order.
INTERVALS is a list of (START . END) with START ≤ END, sorted by START."
  (let ((tail intervals)
        d)
    (while (setq d (cdr tail))
      (if (>= (cdar tail) (1- (caar d)))
          (progn
            (setcdr (car tail) (max (cdar tail) (cdar d)))
            (setcdr tail (cdr d)))
        (setq tail d)))
    intervals))

(defun rx--parse-any (body)
  "Parse arguments of an (any ...) construct.
Return (INTERVALS . CLASSES), where INTERVALS is a sorted list of
disjoint intervals (each a cons of chars), and CLASSES
a list of named character classes in the order they occur in BODY."
  (let ((classes nil)
        (strings nil)
        (conses nil))
    ;; Collect strings, conses and characters, and classes in separate bins.
    (dolist (arg body)
      (cond ((stringp arg)
             (push arg strings))
            ((and (consp arg)
                  (characterp (car arg))
                  (characterp (cdr arg))
                  (<= (car arg) (cdr arg)))
             ;; Copy the cons, in case we need to modify it.
             (push (cons (car arg) (cdr arg)) conses))
            ((characterp arg)
             (push (cons arg arg) conses))
            ((and (symbolp arg)
                  (let ((class (cdr (assq arg rx--char-classes))))
                    (and class
                         (or (memq class classes)
                             (progn (push class classes) t))))))
            (t (error "Invalid rx ‘any’ argument: %s" arg))))
    (cons (rx--condense-intervals
           (sort (append conses
                         (mapcan #'rx--string-to-intervals strings))
                 #'car-less-than-car))
          (reverse classes))))

(defun rx--generate-alt (negated intervals classes)
  "Generate a character alternative.  Return (REGEXP . PRECEDENCE).
If NEGATED is non-nil, negate the result; INTERVALS is a sorted
list of disjoint intervals and CLASSES a list of named character
classes."
  (let ((items (append intervals classes)))
    ;; Move lone ] and range ]-x to the start.
    (let ((rbrac-l (assq ?\] items)))
      (when rbrac-l
        (setq items (cons rbrac-l (delq rbrac-l items)))))

    ;; Split x-] and move the lone ] to the start.
    (let ((rbrac-r (rassq ?\] items)))
      (when (and rbrac-r (not (eq (car rbrac-r) ?\])))
        (setcdr rbrac-r ?\\)
        (setq items (cons '(?\] . ?\]) items))))

    ;; Split ,-- (which would end up as ,- otherwise).
    (let ((dash-r (rassq ?- items)))
      (when (eq (car dash-r) ?,)
        (setcdr dash-r ?,)
        (setq items (nconc items '((?- . ?-))))))

    ;; Remove - (lone or at start of interval)
    (let ((dash-l (assq ?- items)))
      (when dash-l
        (if (eq (cdr dash-l) ?-)
            (setq items (delq dash-l items))   ; Remove lone -
          (setcar dash-l ?.))                  ; Reduce --x to .-x
        (setq items (nconc items '((?- . ?-))))))

    ;; Deal with leading ^ and range ^-x.
    (when (and (consp (car items))
               (eq (caar items) ?^)
               (cdr items))
      ;; Move ^ and ^-x to second place.
      (setq items (cons (cadr items)
                        (cons (car items) (cddr items)))))

    (cond
     ;; Empty set: if negated, any char, otherwise match-nothing.
     ((null items)
      (if negated
          (rx--translate-symbol 'anything)
        (rx--empty)))
     ;; Single non-negated character.
     ((and (null (cdr items))
           (consp (car items))
           (eq (caar items) (cdar items))
           (not negated))
      (cons (list (regexp-quote (char-to-string (caar items))))
            t))
     ;; Negated newline.
     ((and (equal items '((?\n . ?\n)))
           negated)
      (rx--translate-symbol 'nonl))
     ;; At least one character or class, possibly negated.
     (t
      (cons
       (list
        (concat
         "["
         (and negated "^")
         (mapconcat (lambda (item)
                      (cond ((symbolp item)
                             (format "[:%s:]" item))
                            ((eq (car item) (cdr item))
                             (char-to-string (car item)))
                            ((eq (1+ (car item)) (cdr item))
                             (string (car item) (cdr item)))
                            (t
                             (string (car item) ?- (cdr item)))))
                    items nil)
         "]"))
       t)))))

(defun rx--translate-any (negated body)
  "Translate an (any ...) construct.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense."
  (let ((parsed (rx--parse-any body)))
    (rx--generate-alt negated (car parsed) (cdr parsed))))

(defun rx--intervals-to-alt (negated intervals)
  "Generate a character alternative from an interval set.
Return (REGEXP . PRECEDENCE).
INTERVALS is a sorted list of disjoint intervals.
If NEGATED, negate the sense."
  ;; Detect whether the interval set is better described in
  ;; complemented form.  This is not just a matter of aesthetics: any
  ;; range from ASCII to raw bytes will automatically exclude the
  ;; entire non-ASCII Unicode range by the regexp engine.
  (if (rx--every (lambda (iv) (not (<= (car iv) #x3ffeff (cdr iv))))
                 intervals)
      (rx--generate-alt negated intervals nil)
    (rx--generate-alt
     (not negated) (rx--complement-intervals intervals) nil)))

;; FIXME: Consider turning ‘not’ into a variadic operator, following SRE:
;; (not A B) = (not (or A B)) = (intersection (not A) (not B)), and
;; (not) = anychar.
;; Maybe allow singleton characters as arguments.

(defun rx--translate-not (negated body)
  "Translate a (not ...) construct.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense (thus making it positive)."
  (unless (and body (null (cdr body)))
    (error "rx ‘not’ form takes exactly one argument"))
  (let ((arg (car body)))
    (cond
     ((and (consp arg)
           (pcase (car arg)
             ((or 'any 'in 'char)
              (rx--translate-any      (not negated) (cdr arg)))
             ('syntax
              (rx--translate-syntax   (not negated) (cdr arg)))
             ('category
              (rx--translate-category (not negated) (cdr arg)))
             ('not
              (rx--translate-not      (not negated) (cdr arg)))
             ((or 'or '|)
              (rx--translate-union    (not negated) (cdr arg)))
             ('intersection
              (rx--translate-intersection (not negated) (cdr arg))))))
     ((let ((class (cdr (assq arg rx--char-classes))))
        (and class
             (rx--generate-alt (not negated) nil (list class)))))
     ((eq arg 'word-boundary)
      (rx--translate-symbol
       (if negated 'word-boundary 'not-word-boundary)))
     ((characterp arg)
      (rx--generate-alt (not negated) (list (cons arg arg)) nil))
     ((and (stringp arg) (= (length arg) 1))
      (let ((char (string-to-char arg)))
        (rx--generate-alt (not negated) (list (cons char char)) nil)))
     ((let ((expanded (rx--expand-def arg)))
        (and expanded
             (rx--translate-not negated (list expanded)))))
     (t (error "Illegal argument to rx ‘not’: %S" arg)))))

(defun rx--complement-intervals (intervals)
  "Complement of the interval list INTERVALS."
  (let ((compl nil)
        (c 0))
    (dolist (iv intervals)
      (when (< c (car iv))
        (push (cons c (1- (car iv))) compl))
      (setq c (1+ (cdr iv))))
    (when (< c (max-char))
      (push (cons c (max-char)) compl))
    (nreverse compl)))

(defun rx--intersect-intervals (ivs-a ivs-b)
  "Intersection of the interval lists IVS-A and IVS-B."
  (let ((isect nil))
    (while (and ivs-a ivs-b)
      (let ((a (car ivs-a))
            (b (car ivs-b)))
        (cond
         ((< (cdr a) (car b)) (setq ivs-a (cdr ivs-a)))
         ((> (car a) (cdr b)) (setq ivs-b (cdr ivs-b)))
         (t
          (push (cons (max (car a) (car b))
                      (min (cdr a) (cdr b)))
                isect)
          (setq ivs-a (cdr ivs-a))
          (setq ivs-b (cdr ivs-b))
          (cond ((< (cdr a) (cdr b))
                 (push (cons (1+ (cdr a)) (cdr b))
                       ivs-b))
                ((> (cdr a) (cdr b))
                 (push (cons (1+ (cdr b)) (cdr a))
                       ivs-a)))))))
    (nreverse isect)))

(defun rx--union-intervals (ivs-a ivs-b)
  "Union of the interval lists IVS-A and IVS-B."
  (rx--complement-intervals
   (rx--intersect-intervals
    (rx--complement-intervals ivs-a)
    (rx--complement-intervals ivs-b))))

(defun rx--charset-intervals (charset)
  "Return a sorted list of non-adjacent disjoint intervals from CHARSET.
CHARSET is any expression allowed in a character set expression:
characters, single-char strings, ‘any’ forms (no classes permitted),
or ‘not’, ‘or’ or ‘intersection’ forms whose arguments are charsets."
  (pcase charset
    (`(,(or 'any 'in 'char) . ,body)
     (let ((parsed (rx--parse-any body)))
       (when (cdr parsed)
         (error
          "Character class not permitted in set operations: %S"
          (cadr parsed)))
       (car parsed)))
    (`(not ,x) (rx--complement-intervals (rx--charset-intervals x)))
    (`(,(or 'or '|) . ,body) (rx--charset-union body))
    (`(intersection . ,body) (rx--charset-intersection body))
    ((pred characterp)
     (list (cons charset charset)))
    ((guard (and (stringp charset) (= (length charset) 1)))
     (let ((char (string-to-char charset)))
       (list (cons char char))))
    (_ (let ((expanded (rx--expand-def charset)))
         (if expanded
             (rx--charset-intervals expanded)
           (error "Bad character set: %S" charset))))))

(defun rx--charset-union (charsets)
  "Union of CHARSETS, as a set of intervals."
  (rx--foldl #'rx--union-intervals nil
             (mapcar #'rx--charset-intervals charsets)))

(defconst rx--charset-all (list (cons 0 (max-char))))

(defun rx--charset-intersection (charsets)
  "Intersection of CHARSETS, as a set of intervals."
  (rx--foldl #'rx--intersect-intervals rx--charset-all
             (mapcar #'rx--charset-intervals charsets)))

(defun rx--translate-union (negated body)
  "Translate an (or ...) construct of charsets.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense."
  (rx--intervals-to-alt negated (rx--charset-union body)))

(defun rx--translate-intersection (negated body)
  "Translate an (intersection ...) construct.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense."
  (rx--intervals-to-alt negated (rx--charset-intersection body)))

(defun rx--atomic-regexp (item)
  "ITEM is (REGEXP . PRECEDENCE); return a regexp of precedence t."
  (if (eq (cdr item) t)
      (car item)
    (rx--bracket (car item))))

(defun rx--translate-counted-repetition (min-count max-count body)
  (let ((operand (rx--translate-seq body)))
    (if (car operand)
        (cons (append
               (rx--atomic-regexp operand)
               (list (concat "\\{"
                             (number-to-string min-count)
                             (cond ((null max-count) ",")
                                   ((< min-count max-count)
                                    (concat "," (number-to-string max-count))))
                             "\\}")))
              t)
      operand)))

(defun rx--check-repeat-arg (name min-args body)
  (unless (>= (length body) min-args)
    (error "rx `%s' requires at least %d argument%s"
           name min-args (if (= min-args 1) "" "s")))
  ;; There seems to be no reason to disallow zero counts.
  (unless (natnump (car body))
    (error "rx `%s' first argument must be nonnegative" name)))

(defun rx--translate-bounded-repetition (name body)
  (let ((min-count (car body))
        (max-count (cadr body))
        (items (cddr body)))
    (unless (and (natnump min-count)
                 (natnump max-count)
                 (<= min-count max-count))
      (error "rx `%s' range error" name))
    (rx--translate-counted-repetition min-count max-count items)))

(defun rx--translate-repeat (body)
  (rx--check-repeat-arg 'repeat 2 body)
  (if (= (length body) 2)
      (rx--translate-counted-repetition (car body) (car body) (cdr body))
    (rx--translate-bounded-repetition 'repeat body)))

(defun rx--translate-** (body)
  (rx--check-repeat-arg '** 2 body)
  (rx--translate-bounded-repetition '** body))

(defun rx--translate->= (body)
  (rx--check-repeat-arg '>= 1 body)
  (rx--translate-counted-repetition (car body) nil (cdr body)))

(defun rx--translate-= (body)
  (rx--check-repeat-arg '= 1 body)
  (rx--translate-counted-repetition (car body) (car body) (cdr body)))

(defvar rx--greedy t)

(defun rx--translate-rep (op-string greedy body)
  "Translate a repetition; OP-STRING is one of \"*\", \"+\" or \"?\".
GREEDY is a boolean.  Return (REGEXP . PRECEDENCE)."
  (let ((operand (rx--translate-seq body)))
    (if (car operand)
        (cons (append (rx--atomic-regexp operand)
                      (list (concat op-string (unless greedy "?"))))
              ;; The result has precedence seq to avoid (? (* "a")) -> "a*?"
              'seq)
      operand)))

(defun rx--control-greedy (greedy body)
  "Translate the sequence BODY with greediness GREEDY.
Return (REGEXP . PRECEDENCE)."
  (let ((rx--greedy greedy))
    (rx--translate-seq body)))

(defun rx--translate-group (body)
  "Translate the ‘group’ form.  Return (REGEXP . PRECEDENCE)."
  (cons (rx--enclose "\\("
                     (car (rx--translate-seq body))
                     "\\)")
        t))

(defun rx--translate-group-n (body)
  "Translate the ‘group-n’ form.  Return (REGEXP . PRECEDENCE)."
  (unless (and (integerp (car body)) (> (car body) 0))
    (error "rx ‘group-n’ requires a positive number as first argument"))
  (cons (rx--enclose (concat "\\(?" (number-to-string (car body)) ":")
                     (car (rx--translate-seq (cdr body)))
                     "\\)")
        t))

(defun rx--translate-backref (body)
  "Translate the ‘backref’ form.  Return (REGEXP . PRECEDENCE)."
  (unless (and (= (length body) 1) (integerp (car body)) (<= 1 (car body) 9))
    (error "rx ‘backref’ requires an argument in the range 1..9"))
  (cons (list "\\" (number-to-string (car body))) t))

(defconst rx--syntax-codes
  '((whitespace         . ?-)           ; SPC also accepted
    (punctuation        . ?.)
    (word               . ?w)           ; W also accepted
    (symbol             . ?_)
    (open-parenthesis   . ?\()
    (close-parenthesis  . ?\))
    (expression-prefix  . ?\')
    (string-quote       . ?\")
    (paired-delimiter   . ?$)
    (escape             . ?\\)
    (character-quote    . ?/)
    (comment-start      . ?<)
    (comment-end        . ?>)
    (string-delimiter   . ?|)
    (comment-delimiter  . ?!)))

(defun rx--translate-syntax (negated body)
  "Translate the ‘syntax’ form.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "rx ‘syntax’ form takes exactly one argument"))
  (let* ((sym (car body))
         (syntax (cdr (assq sym rx--syntax-codes))))
    (unless syntax
      (cond
       ;; Syntax character directly (sregex compatibility)
       ((and (characterp sym) (rassq sym rx--syntax-codes))
        (setq syntax sym))
       ;; Syntax character as symbol (sregex compatibility)
       ((symbolp sym)
        (let ((name (symbol-name sym)))
          (when (= (length name) 1)
            (let ((char (string-to-char name)))
              (when (rassq char rx--syntax-codes)
                (setq syntax char)))))))
      (unless syntax
        (error "Unknown rx syntax name `%s'" sym)))
    (cons (list (string ?\\ (if negated ?S ?s) syntax))
          t)))

(defconst rx--categories
  '((space-for-indent           . ?\s)
    (base                       . ?.)
    (consonant                  . ?0)
    (base-vowel                 . ?1)
    (upper-diacritical-mark     . ?2)
    (lower-diacritical-mark     . ?3)
    (tone-mark                  . ?4)
    (symbol                     . ?5)
    (digit                      . ?6)
    (vowel-modifying-diacritical-mark . ?7)
    (vowel-sign                 . ?8)
    (semivowel-lower            . ?9)
    (not-at-end-of-line         . ?<)
    (not-at-beginning-of-line   . ?>)
    (alpha-numeric-two-byte     . ?A)
    (chinese-two-byte           . ?C)
    (chinse-two-byte            . ?C)   ; A typo in Emacs 21.1-24.3.
    (greek-two-byte             . ?G)
    (japanese-hiragana-two-byte . ?H)
    (indian-two-byte            . ?I)
    (japanese-katakana-two-byte . ?K)
    (strong-left-to-right       . ?L)
    (korean-hangul-two-byte     . ?N)
    (strong-right-to-left       . ?R)
    (cyrillic-two-byte          . ?Y)
    (combining-diacritic        . ?^)
    (ascii                      . ?a)
    (arabic                     . ?b)
    (chinese                    . ?c)
    (ethiopic                   . ?e)
    (greek                      . ?g)
    (korean                     . ?h)
    (indian                     . ?i)
    (japanese                   . ?j)
    (japanese-katakana          . ?k)
    (latin                      . ?l)
    (lao                        . ?o)
    (tibetan                    . ?q)
    (japanese-roman             . ?r)
    (thai                       . ?t)
    (vietnamese                 . ?v)
    (hebrew                     . ?w)
    (cyrillic                   . ?y)
    (can-break                  . ?|)))

(defun rx--translate-category (negated body)
  "Translate the ‘category’ form.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "rx ‘category’ form takes exactly one argument"))
  (let* ((arg (car body))
         (category
          (cond ((symbolp arg)
                 (let ((cat (assq arg rx--categories)))
                   (unless cat
                     (error "Unknown rx category `%s'" arg))
                   (cdr cat)))
                ((characterp arg) arg)
                (t (error "Invalid rx ‘category’ argument `%s'" arg)))))
    (cons (list (string ?\\ (if negated ?C ?c) category))
          t)))

(defvar rx--delayed-evaluation nil
  "Whether to allow certain forms to be evaluated at runtime.")

(defun rx--translate-literal (body)
  "Translate the ‘literal’ form.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "rx ‘literal’ form takes exactly one argument"))
  (let ((arg (car body)))
    (cond ((stringp arg)
           (cons (list (regexp-quote arg)) (if (= (length arg) 1) t 'seq)))
          (rx--delayed-evaluation
           (cons (list (list 'regexp-quote arg)) 'seq))
          (t (error "rx ‘literal’ form with non-string argument")))))

(defun rx--expand-eval (body)
  "Expand ‘eval’ arguments.  Return a new rx form."
  (unless (and body (null (cdr body)))
    (error "rx ‘eval’ form takes exactly one argument"))
  (eval (car body)))

(defun rx--translate-eval (body)
  "Translate the ‘eval’ form.  Return (REGEXP . PRECEDENCE)."
  (rx--translate (rx--expand-eval body)))

(defvar rx--regexp-atomic-regexp nil)

(defun rx--translate-regexp (body)
  "Translate the ‘regexp’ form.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "rx ‘regexp’ form takes exactly one argument"))
  (let ((arg (car body)))
    (cond ((stringp arg)
           ;; Generate the regexp when needed, since rx isn't
           ;; necessarily present in the byte-compilation environment.
           (unless rx--regexp-atomic-regexp
             (setq rx--regexp-atomic-regexp
                   ;; Match atomic (precedence t) regexps: may give
                   ;; false negatives but no false positives, assuming
                   ;; the target string is syntactically correct.
                   (rx-to-string
                    '(seq
                      bos
                      (or (seq "["
                               (opt "^")
                               (opt "]")
                               (* (or (seq "[:" (+ (any "a-z")) ":]")
                                      (not (any "]"))))
                               "]")
                          (not (any "*+?^$[\\"))
                          (seq "\\"
                               (or anything
                                   (seq (any "sScC_") anything)
                                   (seq "("
                                        (* (or (not (any "\\"))
                                               (seq "\\" (not (any ")")))))
                                        "\\)"))))
                      eos)
                    t)))
           (cons (list arg)
                 (if (string-match-p rx--regexp-atomic-regexp arg) t nil)))
          (rx--delayed-evaluation
           (cons (list arg) nil))
          (t (error "rx ‘regexp’ form with non-string argument")))))

(defun rx--translate-compat-form (def form)
  "Translate a compatibility form from ‘rx-constituents’.
DEF is the definition tuple.  Return (REGEXP . PRECEDENCE)."
  (let* ((fn (nth 0 def))
         (min-args (nth 1 def))
         (max-args (nth 2 def))
         (predicate (nth 3 def))
         (nargs (1- (length form))))
    (when (< nargs min-args)
      (error "The `%s' form takes at least %d argument(s)"
             (car form) min-args))
    (when (and max-args (> nargs max-args))
      (error "The `%s' form takes at most %d argument(s)"
             (car form) max-args))
    (when (and predicate (not (rx--every predicate (cdr form))))
      (error "The `%s' form requires arguments satisfying `%s'"
             (car form) predicate))
    (let ((regexp (funcall fn form)))
      (unless (stringp regexp)
        (error "The `%s' form did not expand to a string" (car form)))
      (cons (list regexp) nil))))

(defun rx--substitute (bindings form)
  "Substitute BINDINGS in FORM.  BINDINGS is an alist of (NAME . VALUES)
where VALUES is a list to splice into FORM wherever NAME occurs.
Return the substitution result wrapped in a list, since a single value
can expand to any number of values."
  (cond ((symbolp form)
         (let ((binding (assq form bindings)))
           (if binding
               (cdr binding)
             (list form))))
        ((consp form)
         (if (listp (cdr form))
             ;; Proper list.  We substitute variables even in the head
             ;; position -- who knows, might be handy one day.
             (list (mapcan (lambda (x) (copy-sequence
                                        (rx--substitute bindings x)))
                           form))
           ;; Cons pair (presumably an interval).
           (let ((first (rx--substitute bindings (car form)))
                 (second (rx--substitute bindings (cdr form))))
             (if (and first (not (cdr first))
                      second (not (cdr second)))
                 (list (cons (car first) (car second)))
               (error
                "Cannot substitute a &rest parameter into a dotted pair")))))
        (t (list form))))

;; FIXME: Consider adding extensions in Lisp macro style, where
;; arguments are passed unevaluated to code that returns the rx form
;; to use.  Example:
;;
;;   (rx-let ((radix-digit (radix)
;;             :lisp (list 'any (cons ?0 (+ ?0 (eval radix) -1)))))
;;     (rx (radix-digit (+ 5 3))))
;; =>
;;   "[0-7]"
;;
;; While this would permit more powerful extensions, it's unclear just
;; how often they would be used in practice.  Let's wait until there is
;; demand for it.

;; FIXME: An alternative binding syntax would be
;;
;;   (NAME RXs...)
;; and
;;   ((NAME ARGS...) RXs...)
;;
;; which would have two minor advantages: multiple RXs with implicit
;; ‘seq’ in the definition, and the arglist is no longer an optional
;; element in the middle of the list.  On the other hand, it's less
;; like traditional lisp arglist constructs (defun, defmacro).
;; Since it's a Scheme-like syntax, &rest parameters could be done using
;; dotted lists:
;;  (rx-let (((name arg1 arg2 . rest) ...definition...)) ...)

(defun rx--expand-template (op values arglist template)
  "Return TEMPLATE with variables in ARGLIST replaced with VALUES."
  (let ((bindings nil)
        (value-tail values)
        (formals arglist))
    (while formals
      (pcase (car formals)
        ('&rest
         (unless (cdr formals)
           (error
            "Expanding rx def `%s': missing &rest parameter name" op))
         (push (cons (cadr formals) value-tail) bindings)
         (setq formals nil)
         (setq value-tail nil))
        (name
         (unless value-tail
           (error
            "Expanding rx def `%s': too few arguments (got %d, need %s%d)"
            op (length values)
            (if (memq '&rest arglist) "at least " "")
            (- (length arglist) (length (memq '&rest arglist)))))
         (push (cons name (list (car value-tail))) bindings)
         (setq value-tail (cdr value-tail))))
      (setq formals (cdr formals)))
    (when value-tail
      (error
       "Expanding rx def `%s': too many arguments (got %d, need %d)"
       op (length values) (length arglist)))
    (let ((subst (rx--substitute bindings template)))
      (if (and subst (not (cdr subst)))
          (car subst)
        (error "Expanding rx def `%s': must result in a single value" op)))))

(defun rx--translate-form (form)
  "Translate an rx form (list structure).  Return (REGEXP . PRECEDENCE)."
  (let ((body (cdr form)))
    (pcase (car form)
      ((or 'seq : 'and 'sequence) (rx--translate-seq body))
      ((or 'or '|)              (rx--translate-or body))
      ((or 'any 'in 'char)      (rx--translate-any nil body))
      ('not-char                (rx--translate-any t body))
      ('not                     (rx--translate-not nil body))
      ('intersection            (rx--translate-intersection nil body))

      ('repeat                  (rx--translate-repeat body))
      ('=                       (rx--translate-= body))
      ('>=                      (rx--translate->= body))
      ('**                      (rx--translate-** body))

      ((or 'zero-or-more '0+)           (rx--translate-rep "*" rx--greedy body))
      ((or 'one-or-more '1+)            (rx--translate-rep "+" rx--greedy body))
      ((or 'zero-or-one 'opt 'optional) (rx--translate-rep "?" rx--greedy body))

      ('*                       (rx--translate-rep "*" t body))
      ('+                       (rx--translate-rep "+" t body))
      ((or '\? ?\s)             (rx--translate-rep "?" t body))

      ('*?                      (rx--translate-rep "*" nil body))
      ('+?                      (rx--translate-rep "+" nil body))
      ((or '\?? ??)             (rx--translate-rep "?" nil body))

      ('minimal-match           (rx--control-greedy nil body))
      ('maximal-match           (rx--control-greedy t   body))

      ((or 'group 'submatch)     (rx--translate-group body))
      ((or 'group-n 'submatch-n) (rx--translate-group-n body))
      ('backref                  (rx--translate-backref body))

      ('syntax                  (rx--translate-syntax nil body))
      ('not-syntax              (rx--translate-syntax t body))
      ('category                (rx--translate-category nil body))

      ('literal                 (rx--translate-literal body))
      ('eval                    (rx--translate-eval body))
      ((or 'regexp 'regex)      (rx--translate-regexp body))

      (op
       (cond
        ((not (symbolp op)) (error "Bad rx operator `%S'" op))

        ((let ((expanded (rx--expand-def form)))
           (and expanded
                (rx--translate expanded))))

        ;; For compatibility with old rx.
        ((let ((entry (assq op rx-constituents)))
           (and (progn
                  (while (and entry (not (consp (cdr entry))))
                    (setq entry
                          (if (symbolp (cdr entry))
                              ;; Alias for another entry.
                              (assq (cdr entry) rx-constituents)
                            ;; Wrong type, try further down the list.
                            (assq (car entry)
                                  (cdr (memq entry rx-constituents))))))
                  entry)
                (rx--translate-compat-form (cdr entry) form))))

        (t (error "Unknown rx form `%s'" op)))))))

(defconst rx--builtin-forms
  '(seq sequence : and or | any in char not-char not intersection
    repeat = >= **
    zero-or-more 0+ *
    one-or-more 1+ +
    zero-or-one opt optional \?
    *? +? \??
    minimal-match maximal-match
    group submatch group-n submatch-n backref
    syntax not-syntax category
    literal eval regexp regex)
  "List of built-in rx function-like symbols.")

(defconst rx--builtin-symbols
  (append '(nonl not-newline any anychar anything unmatchable
            bol eol line-start line-end
            bos eos string-start string-end
            bow eow word-start word-end
            symbol-start symbol-end
            point word-boundary not-word-boundary not-wordchar)
          (mapcar #'car rx--char-classes))
  "List of built-in rx variable-like symbols.")

(defconst rx--builtin-names
  (append rx--builtin-forms rx--builtin-symbols)
  "List of built-in rx names.  These cannot be redefined by the user.")

(defun rx--translate (item)
  "Translate the rx-expression ITEM.  Return (REGEXP . PRECEDENCE)."
  (cond
   ((stringp item)
    (if (= (length item) 0)
        (cons nil 'seq)
      (cons (list (regexp-quote item)) (if (= (length item) 1) t 'seq))))
   ((characterp item)
    (cons (list (regexp-quote (char-to-string item))) t))
   ((symbolp item)
    (rx--translate-symbol item))
   ((consp item)
    (rx--translate-form item))
   (t (error "Bad rx expression: %S" item))))


(defun rx-to-string (form &optional no-group)
  "Translate FORM from ‘rx’ sexp syntax into a string regexp.
The arguments to ‘literal’ and ‘regexp’ forms inside FORM must be
constant strings.
If NO-GROUP is non-nil, don't bracket the result in a non-capturing
group.

For extending the ‘rx’ notation in FORM, use ‘rx-define’ or ‘rx-let-eval’."
  (let* ((item (rx--translate form))
         (exprs (if no-group
                    (car item)
                  (rx--atomic-regexp item))))
    (apply #'concat exprs)))

(defun rx--to-expr (form)
  "Translate the rx-expression FORM to a Lisp expression yielding a regexp."
  (let* ((rx--delayed-evaluation t)
         (elems (car (rx--translate form)))
         (args nil))
    ;; Merge adjacent strings.
    (while elems
      (let ((strings nil))
        (while (and elems (stringp (car elems)))
          (push (car elems) strings)
          (setq elems (cdr elems)))
        (let ((s (apply #'concat (nreverse strings))))
          (unless (zerop (length s))
            (push s args))))
      (when elems
        (push (car elems) args)
        (setq elems (cdr elems))))
    (cond ((null args) "")                             ; 0 args
          ((cdr args) (cons 'concat (nreverse args)))  ; ≥2 args
          (t (car args)))))                            ; 1 arg


(defmacro rx (&rest regexps)
  "Translate regular expressions REGEXPS in sexp form to a regexp string.
Each argument is one of the forms below; RX is a subform, and RX... stands
for zero or more RXs.  For details, see Info node `(elisp) Rx Notation'.
See ‘rx-to-string’ for the corresponding function.

STRING         Match a literal string.
CHAR           Match a literal character.

(seq RX...)    Match the RXs in sequence.  Alias: :, sequence, and.
(or RX...)     Match one of the RXs.  Alias: |.

(zero-or-more RX...) Match RXs zero or more times.  Alias: 0+.
(one-or-more RX...)  Match RXs one or more times.  Alias: 1+.
(zero-or-one RX...)  Match RXs or the empty string.  Alias: opt, optional.
(* RX...)       Match RXs zero or more times; greedy.
(+ RX...)       Match RXs one or more times; greedy.
(? RX...)       Match RXs or the empty string; greedy.
(*? RX...)      Match RXs zero or more times; non-greedy.
(+? RX...)      Match RXs one or more times; non-greedy.
(?? RX...)      Match RXs or the empty string; non-greedy.
(= N RX...)     Match RXs exactly N times.
(>= N RX...)    Match RXs N or more times.
(** N M RX...)  Match RXs N to M times.  Alias: repeat.
(minimal-match RX)  Match RX, with zero-or-more, one-or-more, zero-or-one
                and aliases using non-greedy matching.
(maximal-match RX)  Match RX, with zero-or-more, one-or-more, zero-or-one
                and aliases using greedy matching, which is the default.

(any SET...)    Match a character from one of the SETs.  Each SET is a
                character, a string, a range as string \"A-Z\" or cons
                (?A . ?Z), or a character class (see below).  Alias: in, char.
(not CHARSPEC)  Match one character not matched by CHARSPEC.  CHARSPEC
                can be a character, single-char string, (any ...), (or ...),
                (intersection ...), (syntax ...), (category ...),
                or a character class.
(intersection CHARSET...) Match all CHARSETs.
                CHARSET is (any...), (not...), (or...) or (intersection...),
                a character or a single-char string.
not-newline     Match any character except a newline.  Alias: nonl.
anychar         Match any character.  Alias: anything.
unmatchable     Never match anything at all.

CHARCLASS       Match a character from a character class.  One of:
 alpha, alphabetic, letter   Alphabetic characters (defined by Unicode).
 alnum, alphanumeric         Alphabetic or decimal digit chars (Unicode).
 digit, numeric, num         0-9.
 xdigit, hex-digit, hex      0-9, A-F, a-f.
 cntrl, control              ASCII codes 0-31.
 blank                       Horizontal whitespace (Unicode).
 space, whitespace, white    Chars with whitespace syntax.
 lower, lower-case           Lower-case chars, from current case table.
 upper, upper-case           Upper-case chars, from current case table.
 graph, graphic              Graphic characters (Unicode).
 print, printing             Whitespace or graphic (Unicode).
 punct, punctuation          Not control, space, letter or digit (ASCII);
                              not word syntax (non-ASCII).
 word, wordchar              Characters with word syntax.
 ascii                       ASCII characters (codes 0-127).
 nonascii                    Non-ASCII characters (but not raw bytes).

(syntax SYNTAX)  Match a character with syntax SYNTAX, being one of:
  whitespace, punctuation, word, symbol, open-parenthesis,
  close-parenthesis, expression-prefix, string-quote,
  paired-delimiter, escape, character-quote, comment-start,
  comment-end, string-delimiter, comment-delimiter

(category CAT)   Match a character in category CAT, being one of:
  space-for-indent, base, consonant, base-vowel,
  upper-diacritical-mark, lower-diacritical-mark, tone-mark, symbol,
  digit, vowel-modifying-diacritical-mark, vowel-sign,
  semivowel-lower, not-at-end-of-line, not-at-beginning-of-line,
  alpha-numeric-two-byte, chinese-two-byte, greek-two-byte,
  japanese-hiragana-two-byte, indian-two-byte,
  japanese-katakana-two-byte, strong-left-to-right,
  korean-hangul-two-byte, strong-right-to-left, cyrillic-two-byte,
  combining-diacritic, ascii, arabic, chinese, ethiopic, greek,
  korean, indian, japanese, japanese-katakana, latin, lao,
  tibetan, japanese-roman, thai, vietnamese, hebrew, cyrillic,
  can-break

Zero-width assertions: these all match the empty string in specific places.
 line-start         At the beginning of a line.  Alias: bol.
 line-end           At the end of a line.  Alias: eol.
 string-start       At the start of the string or buffer.
                     Alias: buffer-start, bos, bot.
 string-end         At the end of the string or buffer.
                     Alias: buffer-end, eos, eot.
 point              At point.
 word-start         At the beginning of a word.  Alias: bow.
 word-end           At the end of a word.  Alias: eow.
 word-boundary      At the beginning or end of a word.
 not-word-boundary  Not at the beginning or end of a word.
 symbol-start       At the beginning of a symbol.
 symbol-end         At the end of a symbol.

(group RX...)  Match RXs and define a capture group.  Alias: submatch.
(group-n N RX...) Match RXs and define capture group N.  Alias: submatch-n.
(backref N)    Match the text that capture group N matched.

(literal EXPR) Match the literal string from evaluating EXPR at run time.
(regexp EXPR)  Match the string regexp from evaluating EXPR at run time.
(eval EXPR)    Match the rx sexp from evaluating EXPR at macro-expansion
                (compile) time.

Additional constructs can be defined using ‘rx-define’ and ‘rx-let’,
which see.

\(fn REGEXPS...)"
  ;; Retrieve local definitions from the macroexpansion environment.
  ;; (It's unclear whether the previous value of ‘rx--local-definitions’
  ;; should be included, and if so, in which order.)
  (let ((rx--local-definitions
         (cdr (assq :rx-locals macroexpand-all-environment))))
    (rx--to-expr (cons 'seq regexps))))

(defun rx--make-binding (name tail)
  "Make a definitions entry out of TAIL.
TAIL is on the form ([ARGLIST] DEFINITION)."
  (unless (symbolp name)
    (error "Bad ‘rx’ definition name: %S" name))
  ;; FIXME: Consider using a hash table or symbol property, for speed.
  (when (memq name rx--builtin-names)
    (error "Cannot redefine built-in rx name `%s'" name))
  (pcase tail
    (`(,def)
     (list def))
    (`(,args ,def)
     (unless (and (listp args) (rx--every #'symbolp args))
       (error "Bad argument list for ‘rx’ definition %s: %S" name args))
     (list args def))
    (_ (error "Bad ‘rx’ definition of %s: %S" name tail))))

(defun rx--make-named-binding (bindspec)
  "Make a definitions entry out of BINDSPEC.
BINDSPEC is on the form (NAME [ARGLIST] DEFINITION)."
  (unless (consp bindspec)
    (error "Bad ‘rx-let’ binding: %S" bindspec))
  (cons (car bindspec)
        (rx--make-binding (car bindspec) (cdr bindspec))))

(defun rx--extend-local-defs (bindspecs)
  (append (mapcar #'rx--make-named-binding bindspecs)
          rx--local-definitions))

(defmacro rx-let-eval (bindings &rest body)
  "Evaluate BODY with local BINDINGS for ‘rx-to-string’.
BINDINGS, after evaluation, is a list of definitions each on the form
(NAME [(ARGS...)] RX), in effect for calls to ‘rx-to-string’
in BODY.

For bindings without an ARGS list, NAME is defined as an alias
for the ‘rx’ expression RX.  Where ARGS is supplied, NAME is
defined as an ‘rx’ form with ARGS as argument list.  The
parameters are bound from the values in the (NAME ...) form and
are substituted in RX.  ARGS can contain `&rest' parameters,
whose values are spliced into RX where the parameter name occurs.

Any previous definitions with the same names are shadowed during
the expansion of BODY only.
For extensions when using the ‘rx’ macro, use ‘rx-let’.
To make global rx extensions, use ‘rx-define’.
For more details, see Info node `(elisp) Extending Rx'.

\(fn BINDINGS BODY...)"
  (declare (indent 1) (debug (form body)))
  ;; FIXME: this way, ‘rx--extend-local-defs’ may need to be autoloaded.
  `(let ((rx--local-definitions (rx--extend-local-defs ,bindings)))
     ,@body))

(defmacro rx-let (bindings &rest body)
  "Evaluate BODY with local BINDINGS for ‘rx’.
BINDINGS is an unevaluated list of bindings each on the form
(NAME [(ARGS...)] RX).
They are bound lexically and are available in ‘rx’ expressions in
BODY only.

For bindings without an ARGS list, NAME is defined as an alias
for the ‘rx’ expression RX.  Where ARGS is supplied, NAME is
defined as an ‘rx’ form with ARGS as argument list.  The
parameters are bound from the values in the (NAME ...) form and
are substituted in RX.  ARGS can contain `&rest' parameters,
whose values are spliced into RX where the parameter name occurs.

Any previous definitions with the same names are shadowed during
the expansion of BODY only.
For local extensions to ‘rx-to-string’, use ‘rx-let-eval’.
To make global rx extensions, use ‘rx-define’.
For more details, see Info node `(elisp) Extending Rx'.

\(fn BINDINGS BODY...)"
  (declare (indent 1) (debug (sexp body)))
  (let ((prev-locals (cdr (assq :rx-locals macroexpand-all-environment)))
        (new-locals (mapcar #'rx--make-named-binding bindings)))
    (macroexpand-all (cons 'progn body)
                     (cons (cons :rx-locals (append new-locals prev-locals))
                           macroexpand-all-environment))))

(defmacro rx-define (name &rest definition)
  "Define NAME as a global ‘rx’ definition.
If the ARGS list is omitted, define NAME as an alias for the ‘rx’
expression RX.

If the ARGS list is supplied, define NAME as an ‘rx’ form with
ARGS as argument list.  The parameters are bound from the values
in the (NAME ...) form and are substituted in RX.
ARGS can contain `&rest' parameters, whose values are spliced
into RX where the parameter name occurs.

Any previous global definition of NAME is overwritten with the new one.
To make local rx extensions, use ‘rx-let’ for ‘rx’,
‘rx-let-eval’ for ‘rx-to-string’.
For more details, see Info node `(elisp) Extending Rx'.

\(fn NAME [(ARGS...)] RX)"
  (declare (indent defun))
  `(eval-and-compile
     (put ',name 'rx-definition ',(rx--make-binding name definition))
     ',name))

;; During ‘rx--pcase-transform’, list of defined variables in right-to-left
;; order.
(defvar rx--pcase-vars)

;; FIXME: The rewriting strategy for pcase works so-so with extensions;
;; definitions cannot expand to ‘let’ or named ‘backref’.  If this ever
;; becomes a problem, we can handle those forms in the ordinary parser,
;; using a dynamic variable for activating the augmented forms.

(defun rx--pcase-transform (rx)
  "Transform RX, an rx-expression augmented with ‘let’ and named ‘backref’,
into a plain rx-expression, collecting names into ‘rx--pcase-vars’."
  (pcase rx
    (`(let ,name . ,body)
     (let* ((index (length (memq name rx--pcase-vars)))
            (i (if (zerop index)
                   (length (push name rx--pcase-vars))
                 index)))
       `(group-n ,i ,(rx--pcase-transform (cons 'seq body)))))
    ((and `(backref ,ref)
          (guard (symbolp ref)))
     (let ((index (length (memq ref rx--pcase-vars))))
       (when (zerop index)
         (error "rx ‘backref’ variable must be one of: %s"
                (mapconcat #'symbol-name rx--pcase-vars " ")))
       `(backref ,index)))
    ((and `(,head . ,rest)
          (guard (and (or (symbolp head) (memq head '(?\s ??)))
                      (not (memq head '(literal regexp regex eval))))))
     (cons head (mapcar #'rx--pcase-transform rest)))
    (_ rx)))

(defun rx--reduce-right (f l)
  "Right-reduction on L by F.  L must be non-empty."
  (if (cdr l)
      (funcall f (car l) (rx--reduce-right f (cdr l)))
    (car l)))

(pcase-defmacro rx (&rest regexps)
  "A pattern that matches strings against ‘rx’ REGEXPS in sexp form.
REGEXPS are interpreted as in ‘rx’.  The pattern matches any
string that is a match for REGEXPS, as if by ‘string-match’.

In addition to the usual ‘rx’ syntax, REGEXPS can contain the
following constructs:

  (let REF RX...)  binds the symbol REF to a submatch that matches
                   the regular expressions RX.  REF is bound in
                   CODE to the string of the submatch or nil, but
                   can also be used in ‘backref’.
  (backref REF)    matches whatever the submatch REF matched.
                   REF can be a number, as usual, or a name
                   introduced by a previous (let REF ...)
                   construct."
  (let* ((rx--pcase-vars nil)
         (regexp (rx--to-expr (rx--pcase-transform (cons 'seq regexps)))))
    `(and (pred stringp)
          ,(pcase (length rx--pcase-vars)
            (0
             ;; No variables bound: a single predicate suffices.
             `(pred (string-match ,regexp)))
            (1
             ;; Create a match value that on a successful regexp match
             ;; is the submatch value, 0 on failure.  We can't use nil
             ;; for failure because it is a valid submatch value.
             `(app (lambda (s)
                     (if (string-match ,regexp s)
                         (match-string 1 s)
                       0))
                   (and ,(car rx--pcase-vars) (pred (not numberp)))))
            (nvars
             ;; Pack the submatches into a dotted list which is then
             ;; immediately destructured into individual variables again.
             ;; This is of course slightly inefficient.
             ;; A dotted list is used to reduce the number of conses
             ;; to create and take apart.
             `(app (lambda (s)
                     (and (string-match ,regexp s)
                          ,(rx--reduce-right
                            (lambda (a b) `(cons ,a ,b))
                            (mapcar (lambda (i) `(match-string ,i s))
                                    (number-sequence 1 nvars)))))
                   ,(list '\`
                          (rx--reduce-right
                           #'cons
                           (mapcar (lambda (name) (list '\, name))
                                   (reverse rx--pcase-vars))))))))))

;; Obsolete internal symbol, used in old versions of the ‘flycheck’ package.
(define-obsolete-function-alias 'rx-submatch-n 'rx-to-string "27.1")

;; python-components-extra

(defun py-util-comint-last-prompt ()
  "Return comint last prompt overlay start and end.
This is for compatibility with Emacs < 24.4."
  (cond ((bound-and-true-p comint-last-prompt-overlay)
         (cons (overlay-start comint-last-prompt-overlay)
               (overlay-end comint-last-prompt-overlay)))
        ((bound-and-true-p comint-last-prompt)
         comint-last-prompt)
        (t nil)))

(defun py-shell-accept-process-output (process &optional timeout regexp)
  "Accept PROCESS output with TIMEOUT until REGEXP is found.
Optional argument TIMEOUT is the timeout argument to
‘accept-process-output’ calls.  Optional argument REGEXP
overrides the regexp to match the end of output, defaults to
‘comint-prompt-regexp’.  Returns non-nil when output was
properly captured.

This utility is useful in situations where the output may be
received in chunks, since ‘accept-process-output’ gives no
guarantees they will be grabbed in a single call.  An example use
case for this would be the CPython shell start-up, where the
banner and the initial prompt are received separately."
  (let ((regexp (or regexp comint-prompt-regexp)))
    (catch 'found
      (while t
        (when (not (accept-process-output process timeout))
          (throw 'found nil))
        (when (looking-back
               regexp (car (py-util-comint-last-prompt)))
          (throw 'found t))))))

(defun py-shell-completion-get-completions (process import input)
  "Do completion at point using PROCESS for IMPORT or INPUT.
When IMPORT is non-nil takes precedence over INPUT for
completion."
  (setq input (or import input))
  (with-current-buffer (process-buffer process)
    (let ((completions
           (ignore-errors
	     (py--string-trim
	      (py-send-string-no-output
	       (format
		(concat py-completion-setup-code
			"\nprint (" py-shell-completion-string-code ")")
		input) process (buffer-name (current-buffer)))))))
      (when (> (length completions) 2)
        (split-string completions
                      "^'\\|^\"\\|;\\|'$\\|\"$" t)))))

(defun py-shell-completion-at-point (&optional process)
  "Function for ‘completion-at-point-functions’ in ‘py-shell-mode’.
Optional argument PROCESS forces completions to be retrieved
using that one instead of current buffer's process."
  ;; (setq process (or process (get-buffer-process (current-buffer))))
  (let*
      ((process (or process (get-buffer-process (current-buffer))))
       (line-start (if (derived-mode-p 'py-shell-mode)
		       ;; Working on a shell buffer: use prompt end.
		       (or (cdr (py-util-comint-last-prompt))
			   (line-beginning-position))
		     (line-beginning-position)))
       (import-statement
	(when (string-match-p
	       (rx (* space) word-start (or "from" "import") word-end space)
	       (buffer-substring-no-properties line-start (point)))
	  (buffer-substring-no-properties line-start (point))))
       (start
	(save-excursion
	  (if (not (re-search-backward
		    ;; (py-rx
		    ;;  (or whitespace open-paren close-paren string-delimiter simple-operator))
		    "[[:space:]]\\|[([{]\\|[])}]\\|\\(?:[^\"'\\]\\|\\=\\|\\(?:[^\\]\\|\\=\\)\\\\\\(?:\\\\\\\\\\)*[\"']\\)\\(?:\\\\\\\\\\)*\\(\\(?:\"\"\"\\|'''\\|[\"']\\)\\)\\|[%&*+/<->^|~-]"
		    line-start
		    t 1))
	      line-start
	    (forward-char (length (match-string-no-properties 0)))
	    (point))))
       (end (point))
              (completion-fn
	(with-current-buffer (process-buffer process)
	  #'py-shell-completion-get-completions)))
    (list start end
          (completion-table-dynamic
           (apply-partially
            completion-fn
            process import-statement)))))

(defun py-comint-watch-for-first-prompt-output-filter (output)
  "Run ‘py-shell-first-prompt-hook’ when first prompt is found in OUTPUT."
  (when (not py-shell--first-prompt-received)
    (set (make-local-variable 'py-shell--first-prompt-received-output-buffer)
         (concat py-shell--first-prompt-received-output-buffer
                 (ansi-color-filter-apply output)))
    (when (py-shell-comint-end-of-output-p
           py-shell--first-prompt-received-output-buffer)
      (if (string-match-p
           (concat py-shell-prompt-pdb-regexp (rx eos))
           (or py-shell--first-prompt-received-output-buffer ""))
          ;; Skip pdb prompts and reset the buffer.
          (setq py-shell--first-prompt-received-output-buffer nil)
        (set (make-local-variable 'py-shell--first-prompt-received) t)
        (setq py-shell--first-prompt-received-output-buffer nil)
        (with-current-buffer (current-buffer)
          (let ((inhibit-quit nil))
            (run-hooks 'py-shell-first-prompt-hook))))))
  output)

(defun py-shell-font-lock-get-or-create-buffer ()
  "Get or create a font-lock buffer for current inferior process."
  (with-current-buffer (current-buffer)
    (if py-shell--font-lock-buffer
        py-shell--font-lock-buffer
      (let ((process-name
             (process-name (get-buffer-process (current-buffer)))))
        (generate-new-buffer
         (format " *%s-font-lock*" process-name))))))

(defun py-font-lock-kill-buffer ()
  "Kill the font-lock buffer safely."
  (when (and py-shell--font-lock-buffer
             (buffer-live-p py-shell--font-lock-buffer))
    (kill-buffer py-shell--font-lock-buffer)
    (when (derived-mode-p 'py-shell-mode)
      (setq py-shell--font-lock-buffer nil))))

(defmacro py-shell-font-lock-with-font-lock-buffer (&rest body)
  "Execute the forms in BODY in the font-lock buffer.
The value returned is the value of the last form in BODY.  See
also ‘with-current-buffer’."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (when (not (and py-shell--font-lock-buffer
		     (get-buffer py-shell--font-lock-buffer)))
       (setq py-shell--font-lock-buffer
	     (py-shell-font-lock-get-or-create-buffer)))
     (set-buffer py-shell--font-lock-buffer)
     (when (not font-lock-mode)
       (font-lock-mode 1))
     (set (make-local-variable 'delay-mode-hooks) t)
     (let (py-smart-indentation)
       (when (not (derived-mode-p 'python-mode))
	 (python-mode))
       ,@body)))

(defun py-shell-font-lock-cleanup-buffer ()
  "Cleanup the font-lock buffer.
Provided as a command because this might be handy if something
goes wrong and syntax highlighting in the shell gets messed up."
  (interactive)
  (with-current-buffer (current-buffer)
    (py-shell-font-lock-with-font-lock-buffer
      (erase-buffer))))

(defun py-shell-font-lock-comint-output-filter-function (output)
  "Clean up the font-lock buffer after any OUTPUT."
  (if (and (not (string= "" output))
           ;; Is end of output and is not just a prompt.
           (not (member
                 (py-shell-comint-end-of-output-p
                  (ansi-color-filter-apply output))
                 '(nil 0))))
      ;; If output is other than an input prompt then "real" output has
      ;; been received and the font-lock buffer must be cleaned up.
      (py-shell-font-lock-cleanup-buffer)
    ;; Otherwise just add a newline.
    (py-shell-font-lock-with-font-lock-buffer
      (goto-char (point-max))
      (newline 1)))
  output)

(defun py-font-lock-post-command-hook ()
  "Fontifies current line in shell buffer."
  (let ((prompt-end
	 (or (cdr (py-util-comint-last-prompt))
	     (progn (sit-for 0.1)
		    (cdr (py-util-comint-last-prompt))))))
    (when (and prompt-end (> (point) prompt-end)
               (process-live-p (get-buffer-process (current-buffer))))
      (let* ((input (buffer-substring-no-properties
                     prompt-end (point-max)))
             (deactivate-mark nil)
             (start-pos prompt-end)
             (buffer-undo-list t)
             (font-lock-buffer-pos nil)
             (replacement
              (py-shell-font-lock-with-font-lock-buffer
                (delete-region (line-beginning-position)
                               (point-max))
                (setq font-lock-buffer-pos (point))
                (insert input)
                ;; Ensure buffer is fontified, keeping it
                ;; compatible with Emacs < 24.4.
		(when py-shell-fontify-p
		    (if (fboundp 'font-lock-ensure)
			(funcall 'font-lock-ensure)
		      (font-lock-default-fontify-buffer)))
                (buffer-substring font-lock-buffer-pos
                                  (point-max))))
             (replacement-length (length replacement))
             (i 0))
        ;; Inject text properties to get input fontified.
        (while (not (= i replacement-length))
          (let* ((plist (text-properties-at i replacement))
                 (next-change (or (next-property-change i replacement)
                                  replacement-length))
                 (plist (let ((face (plist-get plist 'face)))
                          (if (not face)
                              plist
                            ;; Replace FACE text properties with
                            ;; FONT-LOCK-FACE so input is fontified.
                            (plist-put plist 'face nil)
                            (plist-put plist 'font-lock-face face)))))
            (set-text-properties
             (+ start-pos i) (+ start-pos next-change) plist)
            (setq i next-change)))))))

(defun py-shell-font-lock-turn-on (&optional msg)
  "Turn on shell font-lock.
With argument MSG show activation message."
  (interactive "p")
  (save-current-buffer
    (py-font-lock-kill-buffer)
    (set (make-local-variable 'py-shell--font-lock-buffer) nil)
    (add-hook 'post-command-hook
	      #'py-font-lock-post-command-hook nil 'local)
    (add-hook 'kill-buffer-hook
              #'py-font-lock-kill-buffer nil 'local)
    (add-hook 'comint-output-filter-functions
              #'py-shell-font-lock-comint-output-filter-function
              'append 'local)
    (when msg
      (message "Shell font-lock is enabled"))))

(defun py-shell-font-lock-turn-off (&optional msg)
  "Turn off shell font-lock.
With argument MSG show deactivation message."
  (interactive "p")
  (with-current-buffer (current-buffer)
    (py-font-lock-kill-buffer)
    (when (py-util-comint-last-prompt)
      ;; Cleanup current fontification
      (remove-text-properties
       (cdr (py-util-comint-last-prompt))
       (line-end-position)
       '(face nil font-lock-face nil)))
    (set (make-local-variable 'py-shell--font-lock-buffer) nil)
    (remove-hook 'post-command-hook
                 #'py-font-lock-post-command-hook 'local)
    (remove-hook 'kill-buffer-hook
                 #'py-font-lock-kill-buffer 'local)
    (remove-hook 'comint-output-filter-functions
                 #'py-shell-font-lock-comint-output-filter-function
                 'local)
    (when msg
      (message "Shell font-lock is disabled"))))

(defun py-shell-font-lock-toggle (&optional msg)
  "Toggle font-lock for shell.
With argument MSG show activation/deactivation message."
  (interactive "p")
  (with-current-buffer (current-buffer)
    (set (make-local-variable 'py-shell-fontify-p)
         (not py-shell-fontify-p))
    (if py-shell-fontify-p
        (py-shell-font-lock-turn-on msg)
      (py-shell-font-lock-turn-off msg))
    py-shell-fontify-p))

(when (featurep 'comint-mime)
  (defun comint-mime-setup-py-shell ()
    "Enable ‘comint-mime’.

Setup code specific to ‘py-shell-mode’."
    (interactive)
    ;; (if (not py-shell--first-prompt-received)
    ;; (add-hook 'py-shell-first-prompt-hook #'comint-mime-setup-py-shell nil t)
    (setq py-python-command "ipython3"
          py-ipython-command "ipython3"
          py-ipython-command-args '("--pylab" "--matplotlib=inline" "--automagic" "--simple-prompt")
          py-python-command-args '("--pylab" "--matplotlib=inline" "--automagic" "--simple-prompt"))
    (py-send-string-no-output
     (format "%s\n__COMINT_MIME_setup('''%s''')"
             (with-temp-buffer
               (switch-to-buffer (current-buffer))
               (insert-file-contents
                (expand-file-name "comint-mime.py"
                                  comint-mime-setup-script-dir))
               (buffer-string))
             (if (listp comint-mime-enabled-types)
                 (string-join comint-mime-enabled-types ";")
               comint-mime-enabled-types))))

  (add-hook 'py-shell-mode-hook 'comint-mime-setup-py-shell)
  (push '(py-shell-mode . comint-mime-setup-py-shell)
	comint-mime-setup-function-alist)
  ;; (setq py-python-command "ipython3"
  ;; 	py-ipython-command "ipython3"
  ;; 	py-python-command-args '("--pylab" "--matplotlib=inline" "--automagic" "--simple-prompt")
  ;; 	;; "-i" doesn't work with `isympy3'
  ;; 	py-ipython-command-args '("--pylab" "--matplotlib=inline" "--automagic" "--simple-prompt"))
  )

;; python-components-shift-forms


(defun py-shift-left (&optional count start end)
  "Dedent region according to ‘py-indent-offset’ by COUNT times.

If no region is active, current line is dedented.
Return indentation reached
Optional COUNT: COUNT times ‘py-indent-offset’
Optional START: region beginning
Optional END: region end"
  (interactive "p")
  (py--shift-intern (- count) start end))

(defun py-shift-right (&optional count beg end)
  "Indent region according to ‘py-indent-offset’ by COUNT times.

If no region is active, current line is indented.
Return indentation reached
Optional COUNT: COUNT times ‘py-indent-offset’
Optional BEG: region beginning
Optional END: region end"
  (interactive "p")
  (py--shift-intern count beg end))

(defun py--shift-intern (count &optional start end)
  (save-excursion
    (let* (;; obsolete
           ;; (inhibit-point-motion-hooks t)
           deactivate-mark
           (beg (cond (start)
                      ((use-region-p)
                       (save-excursion
                         (goto-char
                          (region-beginning))))
                      (t (line-beginning-position))))
           (end (cond (end)
                      ((use-region-p)
                       (save-excursion
                         (goto-char
                          (region-end))))
                      (t (line-end-position)))))
      (setq beg (copy-marker beg))
      (setq end (copy-marker end))
      (if (< 0 count)
          (indent-rigidly beg end py-indent-offset)
        (indent-rigidly beg end (- py-indent-offset)))
      (push-mark beg t)
      (goto-char end)
      (skip-chars-backward " \t\r\n\f"))
    (py-indentation-of-statement)))

(defun py--shift-forms-base (form arg &optional beg end)
  (let* ((begform (intern-soft (concat "py-backward-" form)))
         (endform (intern-soft (concat "py-forward-" form)))
         (orig (copy-marker (point)))
         (beg (cond (beg)
                    ((use-region-p)
                     (save-excursion
                       (goto-char (region-beginning))
                       (line-beginning-position)))
                    (t (save-excursion
                         (funcall begform)
                         (line-beginning-position)))))
         (end (cond (end)
                    ((use-region-p)
                     (region-end))
                    (t (funcall endform))))
         (erg (py--shift-intern arg beg end)))
    (goto-char orig)
    erg))

(defun py-shift-block-right (&optional arg)
  "Indent block by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "block" (or arg py-indent-offset)))

(defun py-shift-block-left (&optional arg)
  "Dedent block by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "block" (- (or arg py-indent-offset))))

(defun py-shift-block-or-clause-right (&optional arg)
  "Indent block-or-clause by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "block-or-clause" (or arg py-indent-offset)))

(defun py-shift-block-or-clause-left (&optional arg)
  "Dedent block-or-clause by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "block-or-clause" (- (or arg py-indent-offset))))

(defun py-shift-class-right (&optional arg)
  "Indent class by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "class" (or arg py-indent-offset)))

(defun py-shift-class-left (&optional arg)
  "Dedent class by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "class" (- (or arg py-indent-offset))))

(defun py-shift-clause-right (&optional arg)
  "Indent clause by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "clause" (or arg py-indent-offset)))

(defun py-shift-clause-left (&optional arg)
  "Dedent clause by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "clause" (- (or arg py-indent-offset))))

(defun py-shift-comment-right (&optional arg)
  "Indent comment by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "comment" (or arg py-indent-offset)))

(defun py-shift-comment-left (&optional arg)
  "Dedent comment by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "comment" (- (or arg py-indent-offset))))

(defun py-shift-def-right (&optional arg)
  "Indent def by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "def" (or arg py-indent-offset)))

(defun py-shift-def-left (&optional arg)
  "Dedent def by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "def" (- (or arg py-indent-offset))))

(defun py-shift-def-or-class-right (&optional arg)
  "Indent def-or-class by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "def-or-class" (or arg py-indent-offset)))

(defun py-shift-def-or-class-left (&optional arg)
  "Dedent def-or-class by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "def-or-class" (- (or arg py-indent-offset))))

(defun py-shift-indent-right (&optional arg)
  "Indent indent by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "indent" (or arg py-indent-offset)))

(defun py-shift-indent-left (&optional arg)
  "Dedent indent by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "indent" (- (or arg py-indent-offset))))

(defun py-shift-minor-block-right (&optional arg)
  "Indent minor-block by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "minor-block" (or arg py-indent-offset)))

(defun py-shift-minor-block-left (&optional arg)
  "Dedent minor-block by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "minor-block" (- (or arg py-indent-offset))))

(defun py-shift-paragraph-right (&optional arg)
  "Indent paragraph by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "paragraph" (or arg py-indent-offset)))

(defun py-shift-paragraph-left (&optional arg)
  "Dedent paragraph by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "paragraph" (- (or arg py-indent-offset))))

(defun py-shift-region-right (&optional arg)
  "Indent region by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "region" (or arg py-indent-offset)))

(defun py-shift-region-left (&optional arg)
  "Dedent region by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "region" (- (or arg py-indent-offset))))

(defun py-shift-statement-right (&optional arg)
  "Indent statement by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "statement" (or arg py-indent-offset)))

(defun py-shift-statement-left (&optional arg)
  "Dedent statement by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "statement" (- (or arg py-indent-offset))))

(defun py-shift-top-level-right (&optional arg)
  "Indent top-level by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "top-level" (or arg py-indent-offset)))

(defun py-shift-top-level-left (&optional arg)
  "Dedent top-level by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \[universal-argument] to specify a different value.

Return outmost indentation reached."
  (interactive "*P")
  (py--shift-forms-base "top-level" (- (or arg py-indent-offset))))

;; python-components-down


(defun py-down-block (&optional indent)
  "Go to the beginning of next block downwards according to INDENT.

Return position if block found, nil otherwise."
  (interactive)
  (py-down-base 'py-block-re indent))

(defun py-down-class (&optional indent)
  "Go to the beginning of next class downwards according to INDENT.

Return position if class found, nil otherwise."
  (interactive)
  (py-down-base 'py-class-re indent))

(defun py-down-clause (&optional indent)
  "Go to the beginning of next clause downwards according to INDENT.

Return position if clause found, nil otherwise."
  (interactive)
  (py-down-base 'py-clause-re indent))

(defun py-down-block-or-clause (&optional indent)
  "Go to the beginning of next block-or-clause downwards according to INDENT.

Return position if block-or-clause found, nil otherwise."
  (interactive)
  (py-down-base 'py-block-or-clause-re indent))

(defun py-down-def (&optional indent)
  "Go to the beginning of next def downwards according to INDENT.

Return position if def found, nil otherwise."
  (interactive)
  (py-down-base 'py-def-re indent))

(defun py-down-def-or-class (&optional indent)
  "Go to the beginning of next def-or-class downwards according to INDENT.

Return position if def-or-class found, nil otherwise."
  (interactive)
  (py-down-base 'py-def-or-class-re indent))

(defun py-down-minor-block (&optional indent)
  "Go to the beginning of next minor-block downwards according to INDENT.

Return position if minor-block found, nil otherwise."
  (interactive)
  (py-down-base 'py-minor-block-re indent))

(defun py-down-block-bol (&optional indent)
  "Go to the beginning of next block below according to INDENT.

Go to beginning of line
Optional INDENT: honor indentation
Return position if block found, nil otherwise "
  (interactive)
  (py-down-base 'py-block-re indent t)
  (progn (beginning-of-line)(point)))

(defun py-down-class-bol (&optional indent)
  "Go to the beginning of next class below according to INDENT.

Go to beginning of line
Optional INDENT: honor indentation
Return position if class found, nil otherwise "
  (interactive)
  (py-down-base 'py-class-re indent t)
  (progn (beginning-of-line)(point)))

(defun py-down-clause-bol (&optional indent)
  "Go to the beginning of next clause below according to INDENT.

Go to beginning of line
Optional INDENT: honor indentation
Return position if clause found, nil otherwise "
  (interactive)
  (py-down-base 'py-clause-re indent t)
  (progn (beginning-of-line)(point)))

(defun py-down-block-or-clause-bol (&optional indent)
  "Go to the beginning of next block-or-clause below according to INDENT.

Go to beginning of line
Optional INDENT: honor indentation
Return position if block-or-clause found, nil otherwise "
  (interactive)
  (py-down-base 'py-block-or-clause-re indent t)
  (progn (beginning-of-line)(point)))

(defun py-down-def-bol (&optional indent)
  "Go to the beginning of next def below according to INDENT.

Go to beginning of line
Optional INDENT: honor indentation
Return position if def found, nil otherwise "
  (interactive)
  (py-down-base 'py-def-re indent t)
  (progn (beginning-of-line)(point)))

(defun py-down-def-or-class-bol (&optional indent)
  "Go to the beginning of next def-or-class below according to INDENT.

Go to beginning of line
Optional INDENT: honor indentation
Return position if def-or-class found, nil otherwise "
  (interactive)
  (py-down-base 'py-def-or-class-re indent t)
  (progn (beginning-of-line)(point)))

(defun py-down-minor-block-bol (&optional indent)
  "Go to the beginning of next minor-block below according to INDENT.

Go to beginning of line
Optional INDENT: honor indentation
Return position if minor-block found, nil otherwise "
  (interactive)
  (py-down-base 'py-minor-block-re indent t)
  (progn (beginning-of-line)(point)))

;; python-components-down.el ends here
;; python-components-start-Zf98zM

(defun py--end-base (regexp &optional orig bol repeat)
  "Used internal by functions going to the end FORM.

Returns the indentation of FORM-start
Arg REGEXP, a symbol"
  (unless (eobp)
    (let (;; not looking for an assignment
	  (use-regexp (member regexp (list 'py-def-re 'py-class-re 'py-def-or-class-re)))
	  (orig (or orig (point))))
      (unless (eobp)
	(unless (py-beginning-of-statement-p)
	  (py-backward-statement))
	(let* (;; when at block-start, be specific
	       ;; (regexp (py--refine-regexp-maybe regexp))
               (regexpvalue (symbol-value regexp))
               ;; (regexp (or regexp (symbol-value 'py-extended-block-or-clause-re)))
	       (repeat (if repeat (1+ repeat) 0))
	       (indent (if
			   (looking-at regexpvalue)
			   (if (bolp) 0
			     (abs
			      (- (current-indentation) py-indent-offset)))
			 (current-indentation)))
	       ;; when at block-start, be specific
	       ;; return current-indentation, position and possibly needed clause-regexps (secondvalue)
	       (res
		(cond
		 ((and (py-beginning-of-statement-p)
		       ;; (eq 0 (current-column))
		       (or (looking-at regexpvalue)
			   (and (member regexp (list 'py-def-re 'py-def-or-class-re 'py-class-re))
				(looking-at py-decorator-re)
				(py-down-def-or-class (current-indentation)))
			   (and (member regexp (list 'py-minor-block-re 'py-if-re 'py-for-re 'py-try-re))
				(looking-at py-minor-clause-re))))
		  (list (current-indentation) (point) (py--end-base-determine-secondvalue regexp)))
		 ((looking-at regexpvalue)
		  (list (current-indentation) (point) (py--end-base-determine-secondvalue regexp)))
		 ((eq 0 (current-indentation))
		  (py--down-according-to-indent regexp nil 0 use-regexp))
		 ;; look upward
		 (t (py--go-to-keyword regexp))))
	       (secondvalue (ignore-errors (nth 2 res)))
	       erg)
	  ;; (py-for-block-p (looking-at py-for-re))
	  (setq indent (or (and res (car-safe res)) indent))
	  (cond
	   (res (setq erg
		      (and
		       (py--down-according-to-indent regexp secondvalue (current-indentation))
		       ;; (if (>= indent (current-indentation))
		       (py--down-end-form)
		       ;; (py--end-base regexp orig bol repeat)
		       ;; )
		       )))
	   (t (unless (< 0 repeat) (goto-char orig))
	      (py--forward-regexp (symbol-value regexp))
	      (beginning-of-line)
	      (setq erg (and
			 (py--down-according-to-indent regexp secondvalue (current-indentation) t)
			 (py--down-end-form)))))
	  (cond ((< orig (point))
		 (setq erg (point))
		 (progn
		   (and erg bol (setq erg (py--beginning-of-line-form)))
		   (and erg (cons (current-indentation) erg))))
		((eq (point) orig)
		 (unless (eobp)
		   (cond
		    ((and (< repeat 1)
			  (or
			   ;; looking next indent as part of body
			   (py--down-according-to-indent regexp secondvalue
							 indent
							 ;; if expected indent is 0,
							 ;; search for new start,
							 ;; search for regexp only
							 (eq 0 indent))
			   (and
			    ;; next block-start downwards, reduce expected indent maybe
			    (setq indent (or (and (< 0 indent) (- indent py-indent-offset)) indent))
			    (py--down-according-to-indent regexp secondvalue
							  indent t))))
		     (py--end-base regexp orig bol (1+ repeat))))))
		((< (point) orig)
		 (goto-char orig)
		 (when (py--down-according-to-indent regexp secondvalue nil t)
		   (py--end-base regexp (point) bol (1+ repeat))))))))))


;; python-components-start-Zf98zM.el ends here
;; python-components-backward-forms

(defun py-backward-region ()
  "Go to the beginning of current region."
  (interactive)
  (let ((beg (region-beginning)))
    (when beg (goto-char beg))))

(defun py-backward-block ()
 "Go to beginning of ‘block’.

If already at beginning, go one ‘block’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (let (erg)
    (setq erg (car-safe (cdr-safe (py--go-to-keyword 'py-block-re))))
    (when py-mark-decorators (and (py-backward-decorator)
                                                 (setq erg (point))))
    erg))

(defun py-backward-class ()
 "Go to beginning of ‘class’.

If already at beginning, go one ‘class’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (let (erg)
    (setq erg (car-safe (cdr-safe (py--go-to-keyword 'py-class-re))))
    (when py-mark-decorators (and (py-backward-decorator)
                                                 (setq erg (point))))
    erg))

(defun py-backward-def ()
 "Go to beginning of ‘def’.

If already at beginning, go one ‘def’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (let (erg)
    (setq erg (car-safe (cdr-safe (py--go-to-keyword 'py-def-re))))
    (when py-mark-decorators (and (py-backward-decorator)
                                                 (setq erg (point))))
    erg))

(defun py-backward-def-or-class ()
 "Go to beginning of ‘def-or-class’.

If already at beginning, go one ‘def-or-class’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (let (erg)
    (setq erg (car-safe (cdr-safe (py--go-to-keyword 'py-def-or-class-re))))
    (when py-mark-decorators (and (py-backward-decorator)
                                                 (setq erg (point))))
    erg))

(defun py-backward-block-bol ()
  "Go to beginning of ‘block’, go to BOL.
If already at beginning, go one ‘block’ backward.
Return beginning of ‘block’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-class-bol ()
  "Go to beginning of ‘class’, go to BOL.
If already at beginning, go one ‘class’ backward.
Return beginning of ‘class’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-class)
       (progn (beginning-of-line)(point))))

(defun py-backward-def-bol ()
  "Go to beginning of ‘def’, go to BOL.
If already at beginning, go one ‘def’ backward.
Return beginning of ‘def’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-def)
       (progn (beginning-of-line)(point))))

(defun py-backward-def-or-class-bol ()
  "Go to beginning of ‘def-or-class’, go to BOL.
If already at beginning, go one ‘def-or-class’ backward.
Return beginning of ‘def-or-class’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-def-or-class)
       (progn (beginning-of-line)(point))))

(defun py-backward-assignment ()
 "Go to beginning of ‘assignment’.

If already at beginning, go one ‘assignment’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-assignment-re))))

(defun py-backward-block-or-clause ()
 "Go to beginning of ‘block-or-clause’.

If already at beginning, go one ‘block-or-clause’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-block-or-clause-re))))

(defun py-backward-clause ()
 "Go to beginning of ‘clause’.

If already at beginning, go one ‘clause’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-clause-re))))

(defun py-backward-elif-block ()
 "Go to beginning of ‘elif-block’.

If already at beginning, go one ‘elif-block’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-elif-re))))

(defun py-backward-else-block ()
 "Go to beginning of ‘else-block’.

If already at beginning, go one ‘else-block’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-else-re))))

(defun py-backward-except-block ()
 "Go to beginning of ‘except-block’.

If already at beginning, go one ‘except-block’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-except-re))))

(defun py-backward-for-block ()
 "Go to beginning of ‘for-block’.

If already at beginning, go one ‘for-block’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-for-re))))

(defun py-backward-if-block ()
 "Go to beginning of ‘if-block’.

If already at beginning, go one ‘if-block’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-if-re))))

(defun py-backward-minor-block ()
 "Go to beginning of ‘minor-block’.

If already at beginning, go one ‘minor-block’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-minor-block-re))))

(defun py-backward-try-block ()
 "Go to beginning of ‘try-block’.

If already at beginning, go one ‘try-block’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (car-safe (cdr-safe (py--go-to-keyword 'py-try-re))))

(defun py-backward-assignment-bol ()
  "Go to beginning of ‘assignment’, go to BOL.
If already at beginning, go one ‘assignment’ backward.
Return beginning of ‘assignment’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-assignment)
       (progn (beginning-of-line)(point))))

(defun py-backward-block-or-clause-bol ()
  "Go to beginning of ‘block-or-clause’, go to BOL.
If already at beginning, go one ‘block-or-clause’ backward.
Return beginning of ‘block-or-clause’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-block-or-clause)
       (progn (beginning-of-line)(point))))

(defun py-backward-clause-bol ()
  "Go to beginning of ‘clause’, go to BOL.
If already at beginning, go one ‘clause’ backward.
Return beginning of ‘clause’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-clause)
       (progn (beginning-of-line)(point))))

(defun py-backward-elif-block-bol ()
  "Go to beginning of ‘elif-block’, go to BOL.
If already at beginning, go one ‘elif-block’ backward.
Return beginning of ‘elif-block’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-elif-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-else-block-bol ()
  "Go to beginning of ‘else-block’, go to BOL.
If already at beginning, go one ‘else-block’ backward.
Return beginning of ‘else-block’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-else-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-except-block-bol ()
  "Go to beginning of ‘except-block’, go to BOL.
If already at beginning, go one ‘except-block’ backward.
Return beginning of ‘except-block’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-except-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-for-block-bol ()
  "Go to beginning of ‘for-block’, go to BOL.
If already at beginning, go one ‘for-block’ backward.
Return beginning of ‘for-block’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-for-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-if-block-bol ()
  "Go to beginning of ‘if-block’, go to BOL.
If already at beginning, go one ‘if-block’ backward.
Return beginning of ‘if-block’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-if-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-minor-block-bol ()
  "Go to beginning of ‘minor-block’, go to BOL.
If already at beginning, go one ‘minor-block’ backward.
Return beginning of ‘minor-block’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-minor-block)
       (progn (beginning-of-line)(point))))

(defun py-backward-try-block-bol ()
  "Go to beginning of ‘try-block’, go to BOL.
If already at beginning, go one ‘try-block’ backward.
Return beginning of ‘try-block’ if successful, nil otherwise"
  (interactive)
  (and (py-backward-try-block)
       (progn (beginning-of-line)(point))))

;; python-components-forward-forms


(defun py-forward-assignment (&optional orig bol)
  "Go to end of assignment.

Return end of ‘assignment’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-assignment-re orig bol)))

(defun py-forward-assignment-bol ()
  "Goto beginning of line following end of ‘assignment’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-assignment’."
  (interactive)
  (py-forward-assignment nil t))

(defun py-forward-region ()
  "Go to the end of current region."
  (interactive)
  (let ((end (region-end)))
    (when end (goto-char end))))

(defun py-forward-block (&optional orig bol)
  "Go to end of block.

Return end of ‘block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-block-re orig bol)))

(defun py-forward-block-bol ()
  "Goto beginning of line following end of ‘block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-block’."
  (interactive)
  (py-forward-block nil t))

(defun py-forward-block-or-clause (&optional orig bol)
  "Go to end of block-or-clause.

Return end of ‘block-or-clause’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-block-or-clause-re orig bol)))

(defun py-forward-block-or-clause-bol ()
  "Goto beginning of line following end of ‘block-or-clause’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-block-or-clause’."
  (interactive)
  (py-forward-block-or-clause nil t))

(defun py-forward-class (&optional orig bol)
  "Go to end of class.

Return end of ‘class’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-class-re orig bol)))

(defun py-forward-class-bol ()
  "Goto beginning of line following end of ‘class’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-class’."
  (interactive)
  (py-forward-class nil t))

(defun py-forward-clause (&optional orig bol)
  "Go to end of clause.

Return end of ‘clause’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-clause-re orig bol)))

(defun py-forward-clause-bol ()
  "Goto beginning of line following end of ‘clause’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-clause’."
  (interactive)
  (py-forward-clause nil t))

(defun py-forward-def (&optional orig bol)
  "Go to end of def.

Return end of ‘def’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-def-re orig bol)))

(defun py-forward-def-bol ()
  "Goto beginning of line following end of ‘def’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-def’."
  (interactive)
  (py-forward-def nil t))

(defun py-forward-def-or-class (&optional orig bol)
  "Go to end of def-or-class.

Return end of ‘def-or-class’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-def-or-class-re orig bol)))

(defun py-forward-def-or-class-bol ()
  "Goto beginning of line following end of ‘def-or-class’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-def-or-class’."
  (interactive)
  (py-forward-def-or-class nil t))

(defun py-forward-elif-block (&optional orig bol)
  "Go to end of elif-block.

Return end of ‘elif-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-elif-re orig bol)))

(defun py-forward-elif-block-bol ()
  "Goto beginning of line following end of ‘elif-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-elif-block’."
  (interactive)
  (py-forward-elif-block nil t))

(defun py-forward-else-block (&optional orig bol)
  "Go to end of else-block.

Return end of ‘else-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-else-re orig bol)))

(defun py-forward-else-block-bol ()
  "Goto beginning of line following end of ‘else-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-else-block’."
  (interactive)
  (py-forward-else-block nil t))

(defun py-forward-except-block (&optional orig bol)
  "Go to end of except-block.

Return end of ‘except-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-except-re orig bol)))

(defun py-forward-except-block-bol ()
  "Goto beginning of line following end of ‘except-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-except-block’."
  (interactive)
  (py-forward-except-block nil t))

(defun py-forward-for-block (&optional orig bol)
  "Go to end of for-block.

Return end of ‘for-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-for-re orig bol)))

(defun py-forward-for-block-bol ()
  "Goto beginning of line following end of ‘for-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-for-block’."
  (interactive)
  (py-forward-for-block nil t))

(defun py-forward-if-block (&optional orig bol)
  "Go to end of if-block.

Return end of ‘if-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-if-re orig bol)))

(defun py-forward-if-block-bol ()
  "Goto beginning of line following end of ‘if-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-if-block’."
  (interactive)
  (py-forward-if-block nil t))

(defun py-forward-minor-block (&optional orig bol)
  "Go to end of minor-block.

Return end of ‘minor-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-minor-block-re orig bol)))

(defun py-forward-minor-block-bol ()
  "Goto beginning of line following end of ‘minor-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-minor-block’."
  (interactive)
  (py-forward-minor-block nil t))

(defun py-forward-try-block (&optional orig bol)
  "Go to end of try-block.

Return end of ‘try-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (py--end-base 'py-try-re orig bol)))

(defun py-forward-try-block-bol ()
  "Goto beginning of line following end of ‘try-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-try-block’."
  (interactive)
  (py-forward-try-block nil t))

;; python-components-forward-forms.el ends here
;; python-components-start2


(defun py--fix-start (strg)
  "Internal use by py-execute... functions.

Takes STRG
Avoid empty lines at the beginning."
  ;; (when py-debug-p (message "py--fix-start:"))
  (let (py--imenu-create-index-p
	py-guess-py-install-directory-p
	py-autopair-mode
	py-complete-function
	py-load-pymacs-p
	py-load-skeletons-p
	erg)
    (with-temp-buffer
      (with-current-buffer (current-buffer)
	(when py-debug-p
	  (switch-to-buffer (current-buffer)))
	;; (python-mode)
	(insert strg)
	(goto-char (point-min))
	(when (< 0 (setq erg (skip-chars-forward " \t\r\n\f" (line-end-position))))
	  (dotimes (_ erg)
	    (indent-rigidly-left (point-min) (point-max))))
	(unless (py--beginning-of-statement-p)
	  (py-forward-statement))
	(while (not (eq (current-indentation) 0))
	  (py-shift-left py-indent-offset))
	(goto-char (point-max))
	(unless (py-empty-line-p)
	  (newline 1))
	(buffer-substring-no-properties 1 (point-max))))))

(defun py-fast-send-string (strg  &optional proc output-buffer result no-output argprompt args dedicated shell exception-buffer)
  (interactive
   (list (read-string "Python command: ")))
  (py-execute-string strg proc result no-output nil output-buffer t argprompt args dedicated shell exception-buffer))

(defun py--fast-send-string-no-output (strg  &optional proc output-buffer result)
  (py-fast-send-string strg proc output-buffer result t))

(defun py--send-to-fast-process (strg proc output-buffer result)
  "Called inside of ‘py--execute-base-intern’.

Optional STRG PROC OUTPUT-BUFFER RETURN"
  (let ((output-buffer (or output-buffer (process-buffer proc)))
	(inhibit-read-only t))
    ;; (switch-to-buffer (current-buffer))
    (with-current-buffer output-buffer
      ;; (erase-buffer)
      (py-fast-send-string strg
			   proc
			   output-buffer result))))

(defun py--point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol -- beginning of line
  eol -- end of line
  bod -- beginning of def or class
  eod -- end of def or class
  bob -- beginning of buffer
  eob -- end of buffer
  boi -- back to indentation
  bos -- beginning of statement

This function does not modify point or mark."
  (save-excursion
    (progn
      (cond
       ((eq position 'bol) (beginning-of-line))
       ((eq position 'eol) (end-of-line))
       ((eq position 'bod) (py-backward-def-or-class))
       ((eq position 'eod) (py-forward-def-or-class))
       ;; Kind of funny, I know, but useful for py-up-exception.
       ((eq position 'bob) (goto-char (point-min)))
       ((eq position 'eob) (goto-char (point-max)))
       ((eq position 'boi) (back-to-indentation))
       ((eq position 'bos) (py-backward-statement))
       (t (error "Unknown buffer position requested: %s" position))))))

(defun py-backward-top-level ()
  "Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise "
  (interactive)
  (let (erg done)
    (unless (bobp)
      (while (and (not done)(not (bobp))
                  (setq erg (re-search-backward "^[[:alpha:]_'\"]" nil t 1)))
        (if
            (nth 8 (parse-partial-sexp (point-min) (point)))
            (setq erg nil)
          (setq done t)))
      erg)))

;; might be slow due to repeated calls of ‘py-down-statement’
(defun py-forward-top-level ()
  "Go to end of top-level form at point.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (unless (py--beginning-of-statement-p)
        (py-backward-statement))
      (unless (eq 0 (current-column))
        (py-backward-top-level))
      (cond ((looking-at py-def-re)
             (setq erg (py-forward-def)))
            ((looking-at py-class-re)
             (setq erg (py-forward-class)))
            ((looking-at py-block-re)
             (setq erg (py-forward-block)))
            (t (setq erg (py-forward-statement))))
      (unless (< orig (point))
        (while (and (not (eobp)) (py-down-statement)(< 0 (current-indentation))))
        (if (looking-at py-block-re)
            (setq erg (py-forward-block))
          (setq erg (py-forward-statement))))
      erg)))

;; python-components-start3

(defun toggle-force-py-shell-name-p (&optional arg)
  "If customized default ‘py-shell-name’ should be enforced upon execution.

If ‘py-force-py-shell-name-p’ should be on or off.
Returns value of ‘py-force-py-shell-name-p’ switched to.

Optional ARG
See also commands
‘force-py-shell-name-p-on’
‘force-py-shell-name-p-off’

Caveat: Completion might not work that way."
  (interactive)
  (let ((arg (or arg (if py-force-py-shell-name-p -1 1))))
    (if (< 0 arg)
        (setq py-force-py-shell-name-p t)
      (setq py-force-py-shell-name-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
    py-force-py-shell-name-p))

(defun force-py-shell-name-p-on ()
  "Switch ‘py-force-py-shell-name-p’ on.

Customized default ‘py-shell-name’ will be enforced upon execution.
Returns value of ‘py-force-py-shell-name-p’.

Caveat: Completion might not work that way."
  (interactive)
  (toggle-force-py-shell-name-p 1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
  py-force-py-shell-name-p)

(defun force-py-shell-name-p-off ()
  "Make sure, ‘py-force-py-shell-name-p’ is off.

Function to use by executes will be guessed from environment.
Returns value of ‘py-force-py-shell-name-p’."
  (interactive)
  (toggle-force-py-shell-name-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
  py-force-py-shell-name-p)

(defun py--fix-if-name-main-permission (strg)
  "Remove \"if __name__ == '__main__ '\" STRG from code to execute.

See ‘py-if-name-main-permission-p’"
  (let ((strg (if py-if-name-main-permission-p strg
		(replace-regexp-in-string
		 "if[( ]*__name__[) ]*==[( ]*['\"]\\{1,3\\}__main__['\"]\\{1,3\\}[) ]*:"
		 ;; space after __main__, i.e. will not be executed
		 "if __name__ == '__main__ ':" strg))))
    strg))

(defun py-symbol-at-point ()
  "Return the current Python symbol.

When interactively called, copy and message it"
  (interactive)
  (let ((erg (with-syntax-table
                 py-dotted-expression-syntax-table
               (current-word))))
    (when (called-interactively-p 'interactive) (kill-new erg)
	  (message "%s" erg))
    erg))

(defun py--line-backward-maybe ()
  "Return result of (< 0 (abs (skip-chars-backward \" \\t\\r\\n\\f\"))) "
  (skip-chars-backward " \t\f" (line-beginning-position))
  (< 0 (abs (skip-chars-backward " \t\r\n\f"))))

(defun py--after-empty-line ()
  "Return ‘t’ if line before contains only whitespace characters. "
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun py-guessed-sanity-check (guessed)
  (and (>= guessed 2)(<= guessed 8)(eq 0 (% guessed 2))))

(defun py--guess-indent-final (indents)
  "Calculate and do sanity-check.

Expects INDENTS, a cons"
  (let* ((first (car indents))
         (second (cadr indents))
         (erg (if (and first second)
                  (if (< second first)
                      (- first second)
                    (- second first))
                (default-value 'py-indent-offset))))
    (setq erg (and (py-guessed-sanity-check erg) erg))
    erg))

(defun py--guess-indent-forward ()
  "Called when moving to end of a form and ‘py-smart-indentation’ is on."
  (let* ((first (if
                    (py--beginning-of-statement-p)
                    (current-indentation)
                  (progn
                    (py-forward-statement)
                    (py-backward-statement)
                    (current-indentation))))
         (second (if (or (looking-at py-extended-block-or-clause-re)(eq 0 first))
                     (progn
                       (py-forward-statement)
                       (py-forward-statement)
                       (py-backward-statement)
                       (current-indentation))
                   ;; when not starting from block, look above
                   (while (and (re-search-backward py-extended-block-or-clause-re nil 'movet 1)
                               (or (>= (current-indentation) first)
                                   (nth 8 (parse-partial-sexp (point-min) (point))))))
                   (current-indentation))))
    (list first second)))

(defun py--guess-indent-backward ()
  "Called when moving to beginning of a form and ‘py-smart-indentation’ is on."
  (let* ((cui (current-indentation))
         (indent (if (< 0 cui) cui 999))
         (pos (progn (while (and (re-search-backward py-extended-block-or-clause-re nil 'move 1)
                                 (or (>= (current-indentation) indent)
                                     (nth 8 (parse-partial-sexp (point-min) (point))))))
                     (unless (bobp) (point))))
         (first (and pos (current-indentation)))
         (second (and pos (py-forward-statement) (py-forward-statement) (py-backward-statement)(current-indentation))))
    (list first second)))

(defun py-guess-indent-offset (&optional direction)
  "Guess ‘py-indent-offset’.

Set local value of ‘py-indent-offset’, return it

Might change local value of ‘py-indent-offset’ only when called
downwards from beginning of block followed by a statement.
Otherwise ‘default-value’ is returned.
Unless DIRECTION is symbol \\='forward, go backward first"
  (interactive)
  (save-excursion
    (let* ((indents
            (cond (direction
                   (if (eq 'forward direction)
                       (py--guess-indent-forward)
                     (py--guess-indent-backward)))
                  ;; guess some usable indent is above current position
                  ((eq 0 (current-indentation))
                   (py--guess-indent-forward))
                  (t (py--guess-indent-backward))))
           (erg (py--guess-indent-final indents)))
      (if erg (setq py-indent-offset erg)
        (setq py-indent-offset
              (default-value 'py-indent-offset)))
      (when (called-interactively-p 'any) (message "%s" py-indent-offset))
      py-indent-offset)))

(defun py--execute-buffer-finally (strg proc procbuf origline filename fast wholebuf)
  (if (and filename wholebuf (not (buffer-modified-p)))
      (unwind-protect
	  (py--execute-file-base filename proc nil procbuf origline fast))
    (let* ((tempfile (concat (expand-file-name py-temp-directory) py-separator-char "temp" (md5 (format "%s" (nth 3 (current-time)))) ".py")))
      (with-temp-buffer
	(insert strg)
	(write-file tempfile))
      (unwind-protect
	  (py--execute-file-base tempfile proc nil procbuf origline fast)
	(and (file-readable-p tempfile) (delete-file tempfile py-debug-p))))))

(defun py--postprocess-intern (&optional origline exception-buffer output-buffer)
  "Highlight exceptions found in BUF.

Optional ORIGLINE EXCEPTION-BUFFER
If an exception occurred return error-string,
otherwise return nil.
BUF must exist.

Indicate LINE if code wasn't run from a file,
thus remember line of source buffer"
  (save-excursion
    (with-current-buffer output-buffer
      (let* (estring ecode erg)
	;; (switch-to-buffer (current-buffer))
	(goto-char (point-max))
	(sit-for 0.1)
	(save-excursion
	  (unless (looking-back py-pdbtrack-input-prompt (line-beginning-position))
	    (forward-line -1)
	    (end-of-line)
	    (when (re-search-backward py-shell-prompt-regexp t 1)
		;; (or (re-search-backward py-shell-prompt-regexp nil t 1)
		;; (re-search-backward (concat py-ipython-input-prompt-re "\\|" py-ipython-output-prompt-re) nil t 1))
	      (save-excursion
		(when (re-search-forward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
		  (setq erg (copy-marker (point)))
		  (delete-region (progn (beginning-of-line)
					(save-match-data
					  (when (looking-at
						 ;; all prompt-regexp known
						 py-shell-prompt-regexp)
					    (goto-char (match-end 0)))))

					(progn (skip-chars-forward " \t\r\n\f"   (line-end-position))(point)))
		  (insert (concat "    File " (buffer-name exception-buffer) ", line "
				  (prin1-to-string origline)))))
	      ;; these are let-bound as ‘tempbuf’
	      ;; (and (boundp 'tempbuf)
	      ;;      (search-forward (buffer-name tempbuf) nil t)
	      ;;      (delete-region (line-beginning-position) (1+ (line-end-position))))
	      ;; if no buffer-file exists, signal "Buffer", not "File(when
	      (when erg
		(goto-char erg)
		;; (forward-char -1)
		;; (skip-chars-backward "^\t\r\n\f")
		;; (skip-chars-forward " \t")
		(save-match-data
		  (and (not (py--buffer-filename-remote-maybe
			     (or
			      (get-buffer exception-buffer)
			      (get-buffer (file-name-nondirectory exception-buffer)))))
		       (string-match "^[ \t]*File" (buffer-substring-no-properties (point) (line-end-position)))
		       (looking-at "[ \t]*File")
		       (replace-match " Buffer")))
		(push origline py-error)
		(push (buffer-name exception-buffer) py-error)
		(forward-line 1)
		(when (looking-at "[ \t]*\\([^\t\n\r\f]+\\)[ \t]*$")
		  (setq estring (match-string-no-properties 1))
		  (setq ecode (replace-regexp-in-string "[ \n\t\f\r^]+" " " estring))
		  (push 'py-error ecode))))))
	py-error))))

(defun py-execute-python-mode-v5 (start end origline filename)
  "Take START END &optional EXCEPTION-BUFFER ORIGLINE."
  (interactive "r")
  (let ((output-buffer "*Python Output*")
	(py-split-window-on-execute 'just-two)
	(pcmd (concat py-shell-name (if (string-equal py-which-bufname
                                                      "Jython")
                                        " -"
                                      ;; " -c "
                                      ""))))
    (save-excursion
      (shell-command-on-region start end
                               pcmd output-buffer))
    (if (not (get-buffer output-buffer))
        (message "No output.")
      (setq py-result (py--fetch-result (get-buffer  output-buffer) nil))
      (if (string-match "Traceback" py-result)
	  (message "%s" (setq py-error (py--fetch-error output-buffer origline filename)))
	py-result))))

(defun py--execute-ge24.3 (start end execute-directory which-shell &optional exception-buffer proc file origline)
  "An alternative way to do it.

According to START END EXECUTE-DIRECTORY WHICH-SHELL
Optional EXCEPTION-BUFFER PROC FILE ORIGLINE
May we get rid of the temporary file?"
  (and (py--buffer-filename-remote-maybe) buffer-offer-save (buffer-modified-p (py--buffer-filename-remote-maybe)) (y-or-n-p "Save buffer before executing? ")
       (write-file (py--buffer-filename-remote-maybe)))
  (let* ((start (copy-marker start))
         (end (copy-marker end))
         (exception-buffer (or exception-buffer (current-buffer)))
         (line (py-count-lines (point-min) (if (eq start (line-beginning-position)) (1+ start) start)))
         (strg (buffer-substring-no-properties start end))
         (tempfile (or (py--buffer-filename-remote-maybe) (concat (expand-file-name py-temp-directory) py-separator-char (replace-regexp-in-string py-separator-char "-" "temp") ".py")))

         (proc (or proc (if py-dedicated-process-p
                            (get-buffer-process (py-shell nil nil t which-shell))
                          (or (get-buffer-process py-buffer-name)
                              (get-buffer-process (py-shell nil nil py-dedicated-process-p which-shell py-buffer-name))))))
         (procbuf (process-buffer proc))
         (file (or file (with-current-buffer py-buffer-name
                          (concat (file-remote-p default-directory) tempfile))))
         (filebuf (get-buffer-create file)))
    (set-buffer filebuf)
    (erase-buffer)
    (newline line)
    (save-excursion
      (insert strg))
    (py--fix-start (buffer-substring-no-properties (point) (point-max)))
    (unless (string-match "[jJ]ython" which-shell)
      ;; (when (and execute-directory py-use-current-dir-when-execute-p
      ;; (not (string= execute-directory default-directory)))
      ;; (message "Warning: options ‘execute-directory’ and ‘py-use-current-dir-when-execute-p’ may conflict"))
      (and execute-directory
           (process-send-string proc (concat "import os; os.chdir(\"" execute-directory "\")\n"))))
    (set-buffer filebuf)
    (process-send-string proc
                         (buffer-substring-no-properties
                          (point-min) (point-max)))
    (sit-for 0.1 t)
    (if (and (setq py-error (save-excursion (py--postprocess-intern origline exception-buffer)))
             (car py-error)
             (not (markerp py-error)))
        (py--jump-to-exception py-error origline)
      (unless (string= (buffer-name (current-buffer)) (buffer-name procbuf))
        (when py-verbose-p (message "Output buffer: %s" procbuf))))))

(defun py--execute-base-intern (strg filename proc wholebuf buffer origline execute-directory start end &optional fast)
  "Select the handler according to:

STRG FILENAME PROC FILE WHOLEBUF
BUFFER ORIGLINE EXECUTE-DIRECTORY START END WHICH-SHELL
Optional FAST RETURN"
  (setq py-error nil)
  (cond ;; (fast (py-fast-send-string strg proc buffer result))
   ;; enforce proceeding as python-mode.el v5
   (python-mode-v5-behavior-p
    (py-execute-python-mode-v5 start end origline filename))
   (py-execute-no-temp-p
    (py--execute-ge24.3 start end execute-directory py-shell-name py-exception-buffer proc filename origline))
   ((and filename wholebuf)
    (py--execute-file-base filename proc nil buffer origline fast))
   (t
    ;; (message "(current-buffer) %s" (current-buffer))
    (py--execute-buffer-finally strg proc buffer origline filename fast wholebuf)
    ;; (py--delete-temp-file tempfile)
    )))

(defun py--execute-base (&optional start end shell filename proc wholebuf fast dedicated split switch)
  "Update optional variables.
START END SHELL FILENAME PROC FILE WHOLEBUF FAST DEDICATED SPLIT SWITCH."
  (setq py-error nil)
  (when py-debug-p (message "py--execute-base: (current-buffer): %s" (current-buffer)))
  ;; (when (or fast py-fast-process-p) (ignore-errors (py-kill-buffer-unconditional py-output-buffer)))
  (let* ((orig (point))
	 (fast (or fast py-fast-process-p))
	 (exception-buffer (current-buffer))
	 (start (or start (and (use-region-p) (region-beginning)) (point-min)))
	 (end (or end (and (use-region-p) (region-end)) (point-max)))
	 (strg-raw (if py-if-name-main-permission-p
		       (buffer-substring-no-properties start end)
		     (py--fix-if-name-main-permission (buffer-substring-no-properties start end))))
	 (strg (py--fix-start strg-raw))
	 (wholebuf (unless filename (or wholebuf (and (eq (buffer-size) (- end start))))))
	 ;; error messages may mention differently when running from a temp-file
	 (origline
	  (format "%s" (save-restriction
			 (widen)
			 (py-count-lines (point-min) orig))))
	 ;; argument SHELL might be a string like "python", "IPython" "python3", a symbol holding PATH/TO/EXECUTABLE or just a symbol like 'python3
	 (shell (or
		 (and shell
		      ;; shell might be specified in different ways
		      (or (and (stringp shell) shell)
			  (ignore-errors (eval shell))
			  (and (symbolp shell) (format "%s" shell))))
		 ;; (save-excursion
		 (py-choose-shell)
		 ;;)
		 ))
	 (shell (or shell (py-choose-shell)))
	 (buffer-name
	  (py--choose-buffer-name shell dedicated fast))
	 (execute-directory
	  (cond ((ignore-errors (file-name-directory (file-remote-p (buffer-file-name) 'localname))))
		((and py-use-current-dir-when-execute-p (buffer-file-name))
		 (file-name-directory (buffer-file-name)))
		((and py-use-current-dir-when-execute-p
		      py-fileless-buffer-use-default-directory-p)
		 (expand-file-name default-directory))
		((stringp py-execute-directory)
		 py-execute-directory)
		((getenv "VIRTUAL_ENV"))
		(t (getenv "HOME"))))
	 (filename (or (and filename (expand-file-name filename))
		       (py--buffer-filename-remote-maybe)))
	 (py-orig-buffer-or-file (or filename (current-buffer)))
	 (proc-raw (or proc (get-buffer-process buffer-name)))

	 (proc (or proc-raw (get-buffer-process buffer-name)
		   (prog1
		       (get-buffer-process (py-shell nil nil dedicated shell buffer-name fast exception-buffer split switch))
		     (sit-for 1)
		     )))
	 (split (if python-mode-v5-behavior-p 'just-two split)))
    (setq py-output-buffer (or (and python-mode-v5-behavior-p py-output-buffer) (and proc (buffer-name (process-buffer proc)))
			       (py--choose-buffer-name shell dedicated fast)))
    (py--execute-base-intern strg filename proc wholebuf py-output-buffer origline execute-directory start end fast)
    (when (or split py-split-window-on-execute py-switch-buffers-on-execute-p)
      (py--shell-manage-windows py-output-buffer exception-buffer (or split py-split-window-on-execute) switch))))

;; python-components-execute-file

;; Execute file given

(defun py-execute-file-ipython (filename)
  "Send file to IPython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil nil "ipython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-ipython3 (filename)
  "Send file to IPython3 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil nil "ipython3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-jython (filename)
  "Send file to Jython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil nil "jython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python (filename)
  "Send file to Python interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil nil "python" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python2 (filename)
  "Send file to Python2 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil nil "python2" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python3 (filename)
  "Send file to Python3 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil nil "python3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-pypy (filename)
  "Send file to PyPy interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil nil "pypy" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file- (filename)
  "Send file to  interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil nil "" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-ipython-dedicated (filename)
  "Send file to a dedicatedIPython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "ipython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-ipython3-dedicated (filename)
  "Send file to a dedicatedIPython3 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "ipython3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-jython-dedicated (filename)
  "Send file to a dedicatedJython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "jython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python-dedicated (filename)
  "Send file to a dedicatedPython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "python" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python2-dedicated (filename)
  "Send file to a dedicatedPython2 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "python2" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python3-dedicated (filename)
  "Send file to a dedicatedPython3 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "python3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-pypy-dedicated (filename)
  "Send file to a dedicatedPyPy interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "pypy" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file--dedicated (filename)
  "Send file to a dedicated interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

;; python-components-up


(defun py-up-block (&optional indent)
  "Go to the beginning of next block upwards according to INDENT.
Optional INDENT
Return position if block found, nil otherwise."
  (interactive)
  (py-up-base 'py-block-re indent))

(defun py-up-class (&optional indent)
  "Go to the beginning of next class upwards according to INDENT.
Optional INDENT
Return position if class found, nil otherwise."
  (interactive)
  (py-up-base 'py-class-re indent))

(defun py-up-clause (&optional indent)
  "Go to the beginning of next clause upwards according to INDENT.
Optional INDENT
Return position if clause found, nil otherwise."
  (interactive)
  (py-up-base 'py-clause-re indent))

(defun py-up-block-or-clause (&optional indent)
  "Go to the beginning of next block-or-clause upwards according to INDENT.
Optional INDENT
Return position if block-or-clause found, nil otherwise."
  (interactive)
  (py-up-base 'py-block-or-clause-re indent))

(defun py-up-def (&optional indent)
  "Go to the beginning of next def upwards according to INDENT.
Optional INDENT
Return position if def found, nil otherwise."
  (interactive)
  (py-up-base 'py-def-re indent))

(defun py-up-def-or-class (&optional indent)
  "Go to the beginning of next def-or-class upwards according to INDENT.
Optional INDENT
Return position if def-or-class found, nil otherwise."
  (interactive)
  (py-up-base 'py-def-or-class-re indent))

(defun py-up-minor-block (&optional indent)
  "Go to the beginning of next minor-block upwards according to INDENT.
Optional INDENT
Return position if minor-block found, nil otherwise."
  (interactive)
  (py-up-base 'py-minor-block-re indent))

(defun py-up-block-bol (&optional indent)
  "Go to the beginning of next block upwards according to INDENT.

Go to beginning of line.
Return position if block found, nil otherwise."
  (interactive)
  (py-up-base 'py-block-re indent)
  (progn (beginning-of-line)(point)))

(defun py-up-class-bol (&optional indent)
  "Go to the beginning of next class upwards according to INDENT.

Go to beginning of line.
Return position if class found, nil otherwise."
  (interactive)
  (py-up-base 'py-class-re indent)
  (progn (beginning-of-line)(point)))

(defun py-up-clause-bol (&optional indent)
  "Go to the beginning of next clause upwards according to INDENT.

Go to beginning of line.
Return position if clause found, nil otherwise."
  (interactive)
  (py-up-base 'py-clause-re indent)
  (progn (beginning-of-line)(point)))

(defun py-up-block-or-clause-bol (&optional indent)
  "Go to the beginning of next block-or-clause upwards according to INDENT.

Go to beginning of line.
Return position if block-or-clause found, nil otherwise."
  (interactive)
  (py-up-base 'py-block-or-clause-re indent)
  (progn (beginning-of-line)(point)))

(defun py-up-def-bol (&optional indent)
  "Go to the beginning of next def upwards according to INDENT.

Go to beginning of line.
Return position if def found, nil otherwise."
  (interactive)
  (py-up-base 'py-def-re indent)
  (progn (beginning-of-line)(point)))

(defun py-up-def-or-class-bol (&optional indent)
  "Go to the beginning of next def-or-class upwards according to INDENT.

Go to beginning of line.
Return position if def-or-class found, nil otherwise."
  (interactive)
  (py-up-base 'py-def-or-class-re indent)
  (progn (beginning-of-line)(point)))

(defun py-up-minor-block-bol (&optional indent)
  "Go to the beginning of next minor-block upwards according to INDENT.

Go to beginning of line.
Return position if minor-block found, nil otherwise."
  (interactive)
  (py-up-base 'py-minor-block-re indent)
  (progn (beginning-of-line)(point)))

;; python-components-up.el ends here
;; python-components-booleans-beginning-forms

(defun py--beginning-of-comment-p (&optional pps)
  "If cursor is at the beginning of a ‘comment’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at (concat "\\b" py-comment-re))
         (point))))

(defun py--beginning-of-expression-p (&optional pps)
  "If cursor is at the beginning of a ‘expression’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at (concat "\\b" py-expression-re))
         (point))))

(defun py--beginning-of-line-p (&optional pps)
  "If cursor is at the beginning of a ‘line’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at (concat "\\b" py-line-re))
         (point))))

(defun py--beginning-of-paragraph-p (&optional pps)
  "If cursor is at the beginning of a ‘paragraph’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at (concat "\\b" py-paragraph-re))
         (point))))

(defun py--beginning-of-partial-expression-p (&optional pps)
  "If cursor is at the beginning of a ‘partial-expression’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at (concat "\\b" py-partial-expression-re))
         (point))))

(defun py--beginning-of-section-p (&optional pps)
  "If cursor is at the beginning of a ‘section’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at (concat "\\b" py-section-re))
         (point))))

(defun py--beginning-of-top-level-p (&optional pps)
  "If cursor is at the beginning of a ‘top-level’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at (concat "\\b" py-top-level-re))
         (point))))

(defun py--beginning-of-assignment-p (&optional pps)
  "If cursor is at the beginning of a ‘assignment’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-assignment-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-block-p (&optional pps)
  "If cursor is at the beginning of a ‘block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-block-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-block-or-clause-p (&optional pps)
  "If cursor is at the beginning of a ‘block-or-clause’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-block-or-clause-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-class-p (&optional pps)
  "If cursor is at the beginning of a ‘class’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-class-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-clause-p (&optional pps)
  "If cursor is at the beginning of a ‘clause’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-clause-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-def-p (&optional pps)
  "If cursor is at the beginning of a ‘def’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-def-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-def-or-class-p (&optional pps)
  "If cursor is at the beginning of a ‘def-or-class’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-def-or-class-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-elif-block-p (&optional pps)
  "If cursor is at the beginning of a ‘elif-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-elif-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-else-block-p (&optional pps)
  "If cursor is at the beginning of a ‘else-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-else-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-except-block-p (&optional pps)
  "If cursor is at the beginning of a ‘except-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-except-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-for-block-p (&optional pps)
  "If cursor is at the beginning of a ‘for-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-for-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-if-block-p (&optional pps)
  "If cursor is at the beginning of a ‘if-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-if-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-indent-p (&optional pps)
  "If cursor is at the beginning of a ‘indent’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-indent-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-minor-block-p (&optional pps)
  "If cursor is at the beginning of a ‘minor-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-minor-block-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-try-block-p (&optional pps)
  "If cursor is at the beginning of a ‘try-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-try-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))

(defun py--beginning-of-assignment-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘assignment’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-assignment-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-block-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-block-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-block-or-clause-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘block-or-clause’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-block-or-clause-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-class-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘class’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-class-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-clause-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘clause’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-clause-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-def-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘def’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-def-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-def-or-class-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘def-or-class’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-def-or-class-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-elif-block-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘elif-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-elif-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-else-block-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘else-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-else-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-except-block-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘except-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-except-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-for-block-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘for-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-for-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-if-block-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘if-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-if-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-indent-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘indent’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-indent-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-minor-block-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘minor-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-minor-block-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

(defun py--beginning-of-try-block-bol-p (&optional pps)
  "If cursor is at the beginning of a ‘try-block’.
Return position, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-try-re)
         (looking-back "[^ \t]*" (line-beginning-position))
         (point))))

;; python-components-move

(defun py-backward-paragraph ()
  "Go to beginning of current paragraph.

If already at beginning, go to start of next paragraph upwards"
  (interactive)
  (backward-paragraph)(point))

(defun py-forward-paragraph ()
    "Go to end of current paragraph.

If already at end, go to end of next paragraph downwards"
  (interactive)
  (and (forward-paragraph)(point)))

;; Indentation
;; Travel current level of indentation
(defun py--travel-this-indent-backward (&optional indent)
  "Travel current INDENT backward.

With optional INDENT travel bigger or equal indentation"
  (let ((indent (or indent (current-indentation)))
	last)
    (while (and (not (bobp))
		(py-backward-statement)
		(<= indent (current-indentation))
		(setq last (point))))
    (when last (goto-char last))
    last))

(defun py-backward-indent ()
  "Go to the beginning of a section of equal indent.

If already at the beginning or before a indent, go to next indent upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let (erg)
      (setq erg (py--travel-this-indent-backward))
      (when erg (goto-char erg))
      erg)))

(defun py--travel-this-indent-backward-bol (indent)
  "Internal use.

Travel this INDENT backward until bol"
  (let (erg)
    (while (and (py-backward-statement-bol)
		(or indent (setq indent (current-indentation)))
		(eq indent (current-indentation))(setq erg (point)) (not (bobp))))
    (when erg (goto-char erg))))

(defun py-backward-indent-bol ()
  "Go to the beginning of line of a section of equal indent.

If already at the beginning or before an indent,
go to next indent in buffer upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let ((indent (when (eq (current-indentation) (current-column)) (current-column)))
	  erg)
      (setq erg (py--travel-this-indent-backward-bol indent))
      erg)))

(defun py--travel-this-indent-forward (indent)
  "Internal use.

Travel this INDENT forward"
  (let (last erg)
    (while (and (py-down-statement)
		(eq indent (current-indentation))
		(setq last (point))))
    (when last (goto-char last))
    (setq erg (py-forward-statement))
    erg))

(defun py-forward-indent (&optional stop-at-empty-line)
  "Go to the end of a section of equal indentation.

If already at the end, go down to next indent in buffer
Returns final position when moved, nil otherwise"
  (interactive "P")
  (skip-chars-forward " \t\r\n\f")
  (let (done
	(orig (line-beginning-position))
	(indent (current-indentation))
	(last (progn (back-to-indentation) (point))))
    (while (and (not (eobp)) (not done)
		(progn (forward-line 1) (back-to-indentation) (or (and (py-empty-line-p)(not stop-at-empty-line)) (and (<= indent (current-indentation))(< last (point))))))
      (unless (py-empty-line-p) (skip-chars-forward " \t\r\n\f")(setq last (point)))
      (when (or (and (py-empty-line-p)stop-at-empty-line) (and (not (py-empty-line-p))(< (current-indentation) indent)))
                 (setq done t))
      )
    (goto-char last)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (and (< orig (point))(point))))

(defun py-forward-indent-bol ()
  "Go to beginning of line following of a section of equal indentation.

If already at the end, go down to next indent in buffer
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (eobp)
    (when (py-forward-indent)
      (unless (eobp) (progn (forward-line 1) (beginning-of-line) (point))))))

;; (defun py-forward-indent-bol ()
;;   "Go to beginning of line following of a section of equal indentation.

;; If already at the end, go down to next indent in buffer
;; Returns final position when called from inside section, nil otherwise"
;;   (interactive)
;;   (unless (eobp)
;;     (let (erg indent)
;;       ;; (when (py-forward-statement)
;;       (when (py-forward-indent)
;; 	;; (save-excursion
;;       	;; (setq indent (and (py-backward-statement)(current-indentation))))
;; 	;; (setq erg (py--travel-this-indent-forward indent))
;; 	(unless (eobp) (forward-line 1) (beginning-of-line) (setq erg (point))))
;;       erg)))

(defun py-backward-expression (&optional orig done repeat)
  "Go to the beginning of a python expression.

If already at the beginning or before a expression,
go to next expression in buffer upwards

ORIG - consider orignial position or point.
DONE - transaktional argument
REPEAT - count and consider repeats"
  (interactive)
  (unless (bobp)
    (unless done (skip-chars-backward " \t\r\n\f"))
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (pps (parse-partial-sexp (point-min) (point)))
          (orig (or orig (point)))
          erg)
      (if (< py-max-specpdl-size repeat)
	  (error "‘py-backward-expression’ reached loops max")
	(cond
	 ;; comments
	 ((nth 8 pps)
	  (goto-char (nth 8 pps))
	  (py-backward-expression orig done repeat))
	 ;; lists
	 ((nth 1 pps)
	  (goto-char (nth 1 pps))
	  (skip-chars-backward py-expression-skip-chars)
	  )
	 ;; in string
	 ((nth 3 pps)
	  (goto-char (nth 8 pps)))
	 ;; after operator
	 ((and (not done) (looking-back py-operator-re (line-beginning-position)))
	  (skip-chars-backward "^ \t\r\n\f")
	  (skip-chars-backward " \t\r\n\f")
	  (py-backward-expression orig done repeat))
	 ((and (not done)
	       (< 0 (abs (skip-chars-backward py-expression-skip-chars))))
	  (setq done t)
	  (py-backward-expression orig done repeat))))
      (unless (or (eq (point) orig)(and (bobp)(eolp)))
	(setq erg (point)))
      erg)))

(defun py-forward-expression (&optional orig done repeat)
  "Go to the end of a compound python expression.

Operators are ignored.
ORIG - consider orignial position or point.
DONE - transaktional argument
REPEAT - count and consider repeats"
  (interactive)
  (unless done (skip-chars-forward " \t\r\n\f"))
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (pps (parse-partial-sexp (point-min) (point)))
          (orig (or orig (point)))
          erg)
      (if (< py-max-specpdl-size repeat)
	  (error "‘py-forward-expression’ reached loops max")
	(cond
	 ;; in comment
	 ((nth 4 pps)
	  (or (< (point) (progn (forward-comment 1) (point)))(forward-line 1))
	  (py-forward-expression orig done repeat))
	 ;; empty before comment
	 ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
	  (while (and (looking-at "[ \t]*#") (not (eobp)))
	    (forward-line 1))
	  (py-forward-expression orig done repeat))
	 ;; inside string
	 ((nth 3 pps)
	  (goto-char (nth 8 pps))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (py-forward-expression orig done repeat))
	 ((looking-at "\"\"\"\\|'''\\|\"\\|'")
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (py-forward-expression orig done repeat))
	 ;; looking at opening delimiter
	 ((eq 4 (car-safe (syntax-after (point))))
	  (goto-char (scan-sexps (point) 1))
	  (skip-chars-forward py-expression-skip-chars)
	  (setq done t))
	 ((nth 1 pps)
	  (goto-char (nth 1 pps))
	  (goto-char (scan-sexps (point) 1))
	  (skip-chars-forward py-expression-skip-chars)
	  (setq done t)
	  (py-forward-expression orig done repeat))
	 ((and (eq orig (point)) (looking-at py-operator-re))
	  (goto-char (match-end 0))
	  (py-forward-expression orig done repeat))
	 ((and (not done)
	       (< 0 (skip-chars-forward py-expression-skip-chars)))
	  (setq done t)
	  (py-forward-expression orig done repeat))
	 ;; at colon following arglist
	 ((looking-at ":[ \t]*$")
	  (forward-char 1)))
	(unless (or (eq (point) orig)(and (eobp) (bolp)))
	  (setq erg (point)))
	erg))))

(defun py-backward-partial-expression ()
  "Backward partial-expression."
  (interactive)
  (let ((orig (point)))
    (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))(not (bobp))(forward-char -1))
    (when (py--in-comment-p)
      (py-backward-comment)
      (skip-chars-backward " \t\r\n\f"))
    ;; part of py-partial-expression-forward-chars
    (when (member (char-after) (list ?\ ?\" ?' ?\) ?} ?\] ?: ?#))
      (forward-char -1))
    (skip-chars-backward py-partial-expression-stop-backward-chars)
    (when (< 0 (abs (skip-chars-backward py-partial-expression-stop-backward-chars)))
      (while (and (not (bobp)) (py--in-comment-p) (< 0 (abs (skip-chars-backward py-partial-expression-stop-backward-chars))))))
    (when (< (point) orig)
      (unless
	  (and (bobp) (member (char-after) (list ?\ ?\t ?\r ?\n ?\f)))
	(point)))))

(defun py-forward-partial-expression ()
  "Forward partial-expression.

Return position reached."
  (interactive)
  (skip-chars-forward py-partial-expression-stop-backward-chars)
  ;; group arg
  (while
      (or (and (eq (char-after) ?\()
               (eq (char-after (1+ (point))) 41))
          (and (eq (char-after) ?\[)
               (or (eq (char-after (1+ (point))) ?\])
                   (eq (char-after (+ 2 (point))) ?\]))))
    (goto-char (scan-sexps (point) 1)))
  (point))

;; Partial- or Minor Expression
;;  Line
(defun py-backward-line ()
  "Go to ‘beginning-of-line’, return position.

If already at ‘beginning-of-line’ and not at BOB,
go to beginning of previous line."
  (interactive)
  (unless (bobp)
    (let ((erg
           (if (bolp)
               (progn
                 (forward-line -1)
                 (progn (beginning-of-line)(point)))
             (progn (beginning-of-line)(point)))))
      erg)))

(defun py-forward-line ()
  "Go to ‘end-of-line’, return position.

If already at ‘end-of-line’ and not at EOB, go to end of next line."
  (interactive)
  (unless (eobp)
    (let ((orig (point)))
      (when (eolp) (forward-line 1))
      (end-of-line)
      (when (< orig (point))(point)))))

(defun py-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature symbol.

With \\[universal-argument] (programmatically, optional argument ARG), do it that many times.
IACT - if called interactively
A ‘nomenclature’ is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((case-fold-search nil)
        (orig (point))
        erg)
    (if (> arg 0)
        (while (and (not (eobp)) (> arg 0))
          ;; (setq erg (re-search-forward "\\(\\W+[_[:lower:][:digit:]ß]+\\)" nil t 1))
          (cond
           ((or (not (eq 0 (skip-chars-forward "[[:blank:][:punct:]\n\r]")))
                (not (eq 0 (skip-chars-forward "_"))))
            (when (or
                   (< 1 (skip-chars-forward "[:upper:]"))
                   (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]ß]")))
                   (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]]"))))
              (setq arg (1- arg))))
           ((or
             (< 1 (skip-chars-forward "[:upper:]"))
             (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]ß]")))
             (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]]"))))
            (setq arg (1- arg)))))
      (while (and (not (bobp)) (< arg 0))
        (when (not (eq 0 (skip-chars-backward "[[:blank:][:punct:]\n\r\f_]")))

          (forward-char -1))
        (or
         (not (eq 0 (skip-chars-backward "[:upper:]")))
         (not (eq 0 (skip-chars-backward "[[:lower:][:digit:]ß]")))
         (skip-chars-backward "[[:lower:][:digit:]ß]"))
        (setq arg (1+ arg))))
    (if (< (point) orig)
        (progn
          (when (looking-back "[[:upper:]]" (line-beginning-position))
            ;; (looking-back "[[:blank:]]"
            (forward-char -1))
          (if (looking-at "[[:alnum:]ß]")
              (setq erg (point))
            (setq erg nil)))
      (if (and (< orig (point)) (not (eobp)))
          (setq erg (point))
        (setq erg nil)))
    erg))

(defun py-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature symbol.

With optional ARG, move that many times.  If ARG is negative, move
forward.

A ‘nomenclature’ is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (setq arg (or arg 1))
  (py-forward-into-nomenclature (- arg)))

(defun py--travel-current-indent (indent &optional orig)
  "Move down until clause is closed, i.e. current indentation is reached.

Takes a list, INDENT and ORIG position."
  (unless (eobp)
    (let ((orig (or orig (point)))
          last)
      (while (and (setq last (point))(not (eobp))(py-forward-statement)
                  (save-excursion (or (<= indent (progn  (py-backward-statement)(current-indentation)))(eq last (line-beginning-position))))
                  ;; (py--end-of-statement-p)
))
      (goto-char last)
      (when (< orig last)
        last))))

(defun py-backward-block-current-column ()
"Reach next beginning of block upwards which start at current column.

Return position"
(interactive)
(let* ((orig (point))
       (cuco (current-column))
       (str (make-string cuco ?\s))
       pps erg)
  (while (and (not (bobp))(re-search-backward (concat "^" str py-block-keywords) nil t)(or (nth 8 (setq pps (parse-partial-sexp (point-min) (point)))) (nth 1 pps))))
  (back-to-indentation)
  (and (< (point) orig)(setq erg (point)))
  erg))

(defun py-backward-section ()
  "Go to next section start upward in buffer.

Return position if successful"
  (interactive)
  (let ((orig (point)))
    (while (and (re-search-backward py-section-start nil t 1)
		(nth 8 (parse-partial-sexp (point-min) (point)))))
    (when (and (looking-at py-section-start)(< (point) orig))
      (point))))

(defun py-forward-section ()
  "Go to next section end downward in buffer.

Return position if successful"
  (interactive)
  (let ((orig (point))
	last)
    (while (and (re-search-forward py-section-end nil t 1)
		(setq last (point))
		(goto-char (match-beginning 0))
		(nth 8 (parse-partial-sexp (point-min) (point)))
		(goto-char (match-end 0))))
    (and last (goto-char last))
    (when (and (looking-back py-section-end (line-beginning-position))(< orig (point)))
      (point))))

(defun py-beginning-of-assignment()
  "Go to beginning of assigment if inside.

Return position of successful, nil of not started from inside."
  (interactive)
  (let* (last
	 (erg
	  (or (py--beginning-of-assignment-p)
	      (progn
		(while (and (setq last (py-backward-statement))
			    (not (looking-at py-assignment-re))
			    ;; (not (bolp))
			    ))
		(and (looking-at py-assignment-re) last)))))
    erg))

;; (defun py--forward-assignment-intern ()
;;   (and (looking-at py-assignment-re)
;;        (goto-char (match-end 2))
;;        (skip-chars-forward " \t\r\n\f")
;;        ;; (eq (car (syntax-after (point))) 4)
;;        (progn (forward-sexp) (point))))

;; (defun py-forward-assignment()
;;   "Go to end of assigment at point if inside.

;; Return position of successful, nil of not started from inside"
;;   (interactive)
;;   (unless (eobp)
;;     (if (eq last-command 'py-backward-assignment)
;; 	;; assume at start of an assignment
;; 	(py--forward-assignment-intern)
;;       ;; ‘py-backward-assignment’ here, avoid ‘py--beginning-of-assignment-p’ a second time
;;       (let* (last
;; 	     (beg
;; 	      (or (py--beginning-of-assignment-p)
;; 		  (progn
;; 		    (while (and (setq last (py-backward-statement))
;; 				(not (looking-at py-assignment-re))
;; 				;; (not (bolp))
;; 				))
;; 		    (and (looking-at py-assignment-re) last))))
;; 	     erg)
;; 	(and beg (setq erg (py--forward-assignment-intern)))
;; 	erg))))

(defun py-up ()
  "Go to the beginning of current syntactic form in buffer.

If in string or comment, reach the beginning.
Respective if inside a list, statement, block etc.

If already at the beginning of a block, move these form upward."
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point)))
        last)
    (cond
     ((nth 8 pps)
      (while (nth 8 pps)
        (goto-char (nth 8 pps))
        (setq last (point))
        (skip-chars-backward " \t\r\n\f")
        (setq pps (parse-partial-sexp (point-min) (point))))
      (when last (goto-char last)))
     ((nth 1 pps)
      (goto-char (nth 1 pps)))
     (t (py-backward-statement)))))
     ;; ((py-beginning-of-statement-p)
     ;;  (py-backward-statement))
     ;; ((py--beginning-of-class-p)
     ;;  (py-up-class (current-indentation)))
     ;; ((py--beginning-of-def-p)
     ;;  (py-up-def (current-indentation)))
     ;; ((py--beginning-of-block-p)
     ;;  (py-up-block (current-indentation)))
     ;; ((py--beginning-of-clause-p)
     ;;  (py-backward-block))
     ;; ((py-beginning-of-statement-p)
     ;;  (py-backward-block-or-clause))


;; python-components-end-position-forms


(defun py--end-of-block-position ()
  "Return end of block position."
  (save-excursion (py-forward-block)))

(defun py--end-of-block-or-clause-position ()
  "Return end of block-or-clause position."
  (save-excursion (py-forward-block-or-clause)))

(defun py--end-of-class-position ()
  "Return end of class position."
  (save-excursion (py-forward-class)))

(defun py--end-of-clause-position ()
  "Return end of clause position."
  (save-excursion (py-forward-clause)))

(defun py--end-of-comment-position ()
  "Return end of comment position."
  (save-excursion (py-forward-comment)))

(defun py--end-of-def-position ()
  "Return end of def position."
  (save-excursion (py-forward-def)))

(defun py--end-of-def-or-class-position ()
  "Return end of def-or-class position."
  (save-excursion (py-forward-def-or-class)))

(defun py--end-of-expression-position ()
  "Return end of expression position."
  (save-excursion (py-forward-expression)))

(defun py--end-of-except-block-position ()
  "Return end of except-block position."
  (save-excursion (py-forward-except-block)))

(defun py--end-of-if-block-position ()
  "Return end of if-block position."
  (save-excursion (py-forward-if-block)))

(defun py--end-of-indent-position ()
  "Return end of indent position."
  (save-excursion (py-forward-indent)))

(defun py--end-of-line-position ()
  "Return end of line position."
  (save-excursion (py-forward-line)))

(defun py--end-of-minor-block-position ()
  "Return end of minor-block position."
  (save-excursion (py-forward-minor-block)))

(defun py--end-of-partial-expression-position ()
  "Return end of partial-expression position."
  (save-excursion (py-forward-partial-expression)))

(defun py--end-of-paragraph-position ()
  "Return end of paragraph position."
  (save-excursion (py-forward-paragraph)))

(defun py--end-of-section-position ()
  "Return end of section position."
  (save-excursion (py-forward-section)))

(defun py--end-of-statement-position ()
  "Return end of statement position."
  (save-excursion (py-forward-statement)))

(defun py--end-of-top-level-position ()
  "Return end of top-level position."
  (save-excursion (py-forward-top-level)))

(defun py--end-of-try-block-position ()
  "Return end of try-block position."
  (save-excursion (py-forward-try-block)))

(defun py--end-of-block-position-bol ()
  "Return end of block position at ‘beginning-of-line’."
  (save-excursion (py-forward-block-bol)))

(defun py--end-of-block-or-clause-position-bol ()
  "Return end of block-or-clause position at ‘beginning-of-line’."
  (save-excursion (py-forward-block-or-clause-bol)))

(defun py--end-of-class-position-bol ()
  "Return end of class position at ‘beginning-of-line’."
  (save-excursion (py-forward-class-bol)))

(defun py--end-of-clause-position-bol ()
  "Return end of clause position at ‘beginning-of-line’."
  (save-excursion (py-forward-clause-bol)))

(defun py--end-of-def-position-bol ()
  "Return end of def position at ‘beginning-of-line’."
  (save-excursion (py-forward-def-bol)))

(defun py--end-of-def-or-class-position-bol ()
  "Return end of def-or-class position at ‘beginning-of-line’."
  (save-excursion (py-forward-def-or-class-bol)))

(defun py--end-of-elif-block-position-bol ()
  "Return end of elif-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-elif-block-bol)))

(defun py--end-of-else-block-position-bol ()
  "Return end of else-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-else-block-bol)))

(defun py--end-of-except-block-position-bol ()
  "Return end of except-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-except-block-bol)))

(defun py--end-of-for-block-position-bol ()
  "Return end of for-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-for-block-bol)))

(defun py--end-of-if-block-position-bol ()
  "Return end of if-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-if-block-bol)))

(defun py--end-of-indent-position-bol ()
  "Return end of indent position at ‘beginning-of-line’."
  (save-excursion (py-forward-indent-bol)))

(defun py--end-of-minor-block-position-bol ()
  "Return end of minor-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-minor-block-bol)))

(defun py--end-of-statement-position-bol ()
  "Return end of statement position at ‘beginning-of-line’."
  (save-excursion (py-forward-statement-bol)))

(defun py--end-of-try-block-position-bol ()
  "Return end of try-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-try-block-bol)))

;; python-components-beginning-position-forms


(defun py--beginning-of-block-position ()
  "Return beginning of block position."
  (save-excursion
    (or (py--beginning-of-block-p)
        (py-backward-block))))

(defun py--beginning-of-block-or-clause-position ()
  "Return beginning of block-or-clause position."
  (save-excursion
    (or (py--beginning-of-block-or-clause-p)
        (py-backward-block-or-clause))))

(defun py--beginning-of-class-position ()
  "Return beginning of class position."
  (save-excursion
    (or (py--beginning-of-class-p)
        (py-backward-class))))

(defun py--beginning-of-clause-position ()
  "Return beginning of clause position."
  (save-excursion
    (or (py--beginning-of-clause-p)
        (py-backward-clause))))

(defun py--beginning-of-comment-position ()
  "Return beginning of comment position."
  (save-excursion
    (or (py--beginning-of-comment-p)
        (py-backward-comment))))

(defun py--beginning-of-def-position ()
  "Return beginning of def position."
  (save-excursion
    (or (py--beginning-of-def-p)
        (py-backward-def))))

(defun py--beginning-of-def-or-class-position ()
  "Return beginning of def-or-class position."
  (save-excursion
    (or (py--beginning-of-def-or-class-p)
        (py-backward-def-or-class))))

(defun py--beginning-of-expression-position ()
  "Return beginning of expression position."
  (save-excursion
    (or (py--beginning-of-expression-p)
        (py-backward-expression))))

(defun py--beginning-of-except-block-position ()
  "Return beginning of except-block position."
  (save-excursion
    (or (py--beginning-of-except-block-p)
        (py-backward-except-block))))

(defun py--beginning-of-if-block-position ()
  "Return beginning of if-block position."
  (save-excursion
    (or (py--beginning-of-if-block-p)
        (py-backward-if-block))))

(defun py--beginning-of-indent-position ()
  "Return beginning of indent position."
  (save-excursion
    (or (py--beginning-of-indent-p)
        (py-backward-indent))))

(defun py--beginning-of-line-position ()
  "Return beginning of line position."
  (save-excursion
    (or (py--beginning-of-line-p)
        (py-backward-line))))

(defun py--beginning-of-minor-block-position ()
  "Return beginning of minor-block position."
  (save-excursion
    (or (py--beginning-of-minor-block-p)
        (py-backward-minor-block))))

(defun py--beginning-of-partial-expression-position ()
  "Return beginning of partial-expression position."
  (save-excursion
    (or (py--beginning-of-partial-expression-p)
        (py-backward-partial-expression))))

(defun py--beginning-of-paragraph-position ()
  "Return beginning of paragraph position."
  (save-excursion
    (or (py--beginning-of-paragraph-p)
        (py-backward-paragraph))))

(defun py--beginning-of-section-position ()
  "Return beginning of section position."
  (save-excursion
    (or (py--beginning-of-section-p)
        (py-backward-section))))

(defun py--beginning-of-statement-position ()
  "Return beginning of statement position."
  (save-excursion
    (or (py--beginning-of-statement-p)
        (py-backward-statement))))

(defun py--beginning-of-top-level-position ()
  "Return beginning of top-level position."
  (save-excursion
    (or (py--beginning-of-top-level-p)
        (py-backward-top-level))))

(defun py--beginning-of-try-block-position ()
  "Return beginning of try-block position."
  (save-excursion
    (or (py--beginning-of-try-block-p)
        (py-backward-try-block))))

(defun py--beginning-of-block-position-bol ()
  "Return beginning of block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-block-bol-p)
        (py-backward-block-bol))))

(defun py--beginning-of-block-or-clause-position-bol ()
  "Return beginning of block-or-clause position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-block-or-clause-bol-p)
        (py-backward-block-or-clause-bol))))

(defun py--beginning-of-class-position-bol ()
  "Return beginning of class position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-class-bol-p)
        (py-backward-class-bol))))

(defun py--beginning-of-clause-position-bol ()
  "Return beginning of clause position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-clause-bol-p)
        (py-backward-clause-bol))))

(defun py--beginning-of-def-position-bol ()
  "Return beginning of def position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-def-bol-p)
        (py-backward-def-bol))))

(defun py--beginning-of-def-or-class-position-bol ()
  "Return beginning of def-or-class position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-def-or-class-bol-p)
        (py-backward-def-or-class-bol))))

(defun py--beginning-of-elif-block-position-bol ()
  "Return beginning of elif-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-elif-block-bol-p)
        (py-backward-elif-block-bol))))

(defun py--beginning-of-else-block-position-bol ()
  "Return beginning of else-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-else-block-bol-p)
        (py-backward-else-block-bol))))

(defun py--beginning-of-except-block-position-bol ()
  "Return beginning of except-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-except-block-bol-p)
        (py-backward-except-block-bol))))

(defun py--beginning-of-for-block-position-bol ()
  "Return beginning of for-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-for-block-bol-p)
        (py-backward-for-block-bol))))

(defun py--beginning-of-if-block-position-bol ()
  "Return beginning of if-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-if-block-bol-p)
        (py-backward-if-block-bol))))

(defun py--beginning-of-indent-position-bol ()
  "Return beginning of indent position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-indent-bol-p)
        (py-backward-indent-bol))))

(defun py--beginning-of-minor-block-position-bol ()
  "Return beginning of minor-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-minor-block-bol-p)
        (py-backward-minor-block-bol))))

(defun py--beginning-of-statement-position-bol ()
  "Return beginning of statement position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-statement-bol-p)
        (py-backward-statement-bol))))

(defun py--beginning-of-try-block-position-bol ()
  "Return beginning of try-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-try-block-bol-p)
        (py-backward-try-block-bol))))

;; python-components-extended-executes

(defun py--execute-prepare (form shell &optional dedicated switch beg end filename fast proc wholebuf split)
  "Update some vars."
  (save-excursion
    (let* ((form (prin1-to-string form))
           (origline (py-count-lines))
           (fast
            (or fast py-fast-process-p))
           (py-exception-buffer (current-buffer))
           (beg (unless filename
                  (prog1
                      (or beg (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
                          (funcall (intern-soft (concat "py-backward-" form)))
                          (push-mark)))))
           (end (unless filename
                  (or end (save-excursion (funcall (intern-soft (concat "py-forward-" form))))))))
      ;; (setq py-buffer-name nil)
      (if filename
            (if (file-readable-p filename)
                (py--execute-file-base (expand-file-name filename) nil nil nil origline)
              (message "%s not readable. %s" filename "Do you have write permissions?"))
        (py--execute-base beg end shell filename proc wholebuf fast dedicated split switch)))))

(defun py-execute-block-ipython (&optional dedicated fast split switch proc)
  "Send block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython-dedicated (&optional fast split switch proc)
  "Send block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython3 (&optional dedicated fast split switch proc)
  "Send block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython3-dedicated (&optional fast split switch proc)
  "Send block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-jython (&optional dedicated fast split switch proc)
  "Send block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-jython-dedicated (&optional fast split switch proc)
  "Send block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python (&optional dedicated fast split switch proc)
  "Send block at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python-dedicated (&optional fast split switch proc)
  "Send block at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python2 (&optional dedicated fast split switch proc)
  "Send block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python2-dedicated (&optional fast split switch proc)
  "Send block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python3 (&optional dedicated fast split switch proc)
  "Send block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python3-dedicated (&optional fast split switch proc)
  "Send block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-pypy (&optional dedicated fast split switch proc)
  "Send block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-pypy-dedicated (&optional fast split switch proc)
  "Send block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block (&optional shell dedicated fast split switch proc)
  "Send block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-dedicated (&optional shell fast split switch proc)
  "Send block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython3 (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython3-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-jython (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-jython-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python2 (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python2-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python3 (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python3-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-pypy (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-pypy-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause (&optional shell dedicated fast split switch proc)
  "Send block-or-clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-dedicated (&optional shell fast split switch proc)
  "Send block-or-clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython (&optional dedicated fast split switch proc)
  "Send buffer at point to a python3 interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython-dedicated (&optional fast split switch proc)
  "Send buffer at point to a python3 unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython3 (&optional dedicated fast split switch proc)
  "Send buffer at point to a python3 interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython3 dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython3-dedicated (&optional fast split switch proc)
  "Send buffer at point to a python3 unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython3 t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-jython (&optional dedicated fast split switch proc)
  "Send buffer at point to a python3 interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'jython dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-jython-dedicated (&optional fast split switch proc)
  "Send buffer at point to a python3 unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'jython t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python (&optional dedicated fast split switch proc)
  "Send buffer at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python-dedicated (&optional fast split switch proc)
  "Send buffer at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python2 (&optional dedicated fast split switch proc)
  "Send buffer at point to a python3 interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python2 dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python2-dedicated (&optional fast split switch proc)
  "Send buffer at point to a python3 unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python2 t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python3 (&optional dedicated fast split switch proc)
  "Send buffer at point to a python3 interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python3 dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python3-dedicated (&optional fast split switch proc)
  "Send buffer at point to a python3 unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python3 t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-pypy (&optional dedicated fast split switch proc)
  "Send buffer at point to a python3 interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'pypy dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-pypy-dedicated (&optional fast split switch proc)
  "Send buffer at point to a python3 unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'pypy t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer (&optional shell dedicated fast split switch proc)
  "Send buffer at point to a python3 interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer shell dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-dedicated (&optional shell fast split switch proc)
  "Send buffer at point to a python3 unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
            buffer (or (get-file-buffer filename)
                       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer shell t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-class-ipython (&optional dedicated fast split switch proc)
  "Send class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython-dedicated (&optional fast split switch proc)
  "Send class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython3 (&optional dedicated fast split switch proc)
  "Send class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython3-dedicated (&optional fast split switch proc)
  "Send class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-jython (&optional dedicated fast split switch proc)
  "Send class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-jython-dedicated (&optional fast split switch proc)
  "Send class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python (&optional dedicated fast split switch proc)
  "Send class at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python-dedicated (&optional fast split switch proc)
  "Send class at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python2 (&optional dedicated fast split switch proc)
  "Send class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python2-dedicated (&optional fast split switch proc)
  "Send class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python3 (&optional dedicated fast split switch proc)
  "Send class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python3-dedicated (&optional fast split switch proc)
  "Send class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-pypy (&optional dedicated fast split switch proc)
  "Send class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-pypy-dedicated (&optional fast split switch proc)
  "Send class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class (&optional shell dedicated fast split switch proc)
  "Send class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-dedicated (&optional shell fast split switch proc)
  "Send class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython (&optional dedicated fast split switch proc)
  "Send clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython-dedicated (&optional fast split switch proc)
  "Send clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython3 (&optional dedicated fast split switch proc)
  "Send clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython3-dedicated (&optional fast split switch proc)
  "Send clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-jython (&optional dedicated fast split switch proc)
  "Send clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-jython-dedicated (&optional fast split switch proc)
  "Send clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python (&optional dedicated fast split switch proc)
  "Send clause at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python-dedicated (&optional fast split switch proc)
  "Send clause at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python2 (&optional dedicated fast split switch proc)
  "Send clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python2-dedicated (&optional fast split switch proc)
  "Send clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python3 (&optional dedicated fast split switch proc)
  "Send clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python3-dedicated (&optional fast split switch proc)
  "Send clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-pypy (&optional dedicated fast split switch proc)
  "Send clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-pypy-dedicated (&optional fast split switch proc)
  "Send clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause (&optional shell dedicated fast split switch proc)
  "Send clause at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-dedicated (&optional shell fast split switch proc)
  "Send clause at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython (&optional dedicated fast split switch proc)
  "Send def at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython-dedicated (&optional fast split switch proc)
  "Send def at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython3 (&optional dedicated fast split switch proc)
  "Send def at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython3-dedicated (&optional fast split switch proc)
  "Send def at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-jython (&optional dedicated fast split switch proc)
  "Send def at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-jython-dedicated (&optional fast split switch proc)
  "Send def at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python (&optional dedicated fast split switch proc)
  "Send def at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python-dedicated (&optional fast split switch proc)
  "Send def at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python2 (&optional dedicated fast split switch proc)
  "Send def at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python2-dedicated (&optional fast split switch proc)
  "Send def at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python3 (&optional dedicated fast split switch proc)
  "Send def at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python3-dedicated (&optional fast split switch proc)
  "Send def at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-pypy (&optional dedicated fast split switch proc)
  "Send def at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-pypy-dedicated (&optional fast split switch proc)
  "Send def at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def (&optional shell dedicated fast split switch proc)
  "Send def at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-dedicated (&optional shell fast split switch proc)
  "Send def at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython (&optional dedicated fast split switch proc)
  "Send def-or-class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython3 (&optional dedicated fast split switch proc)
  "Send def-or-class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython3-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-jython (&optional dedicated fast split switch proc)
  "Send def-or-class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-jython-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python (&optional dedicated fast split switch proc)
  "Send def-or-class at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python2 (&optional dedicated fast split switch proc)
  "Send def-or-class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python2-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python3 (&optional dedicated fast split switch proc)
  "Send def-or-class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python3-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-pypy (&optional dedicated fast split switch proc)
  "Send def-or-class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-pypy-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class (&optional shell dedicated fast split switch proc)
  "Send def-or-class at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-dedicated (&optional shell fast split switch proc)
  "Send def-or-class at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython (&optional dedicated fast split switch proc)
  "Send expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython-dedicated (&optional fast split switch proc)
  "Send expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython3 (&optional dedicated fast split switch proc)
  "Send expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython3-dedicated (&optional fast split switch proc)
  "Send expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-jython (&optional dedicated fast split switch proc)
  "Send expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-jython-dedicated (&optional fast split switch proc)
  "Send expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python (&optional dedicated fast split switch proc)
  "Send expression at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python-dedicated (&optional fast split switch proc)
  "Send expression at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python2 (&optional dedicated fast split switch proc)
  "Send expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python2-dedicated (&optional fast split switch proc)
  "Send expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python3 (&optional dedicated fast split switch proc)
  "Send expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python3-dedicated (&optional fast split switch proc)
  "Send expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-pypy (&optional dedicated fast split switch proc)
  "Send expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-pypy-dedicated (&optional fast split switch proc)
  "Send expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression (&optional shell dedicated fast split switch proc)
  "Send expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-dedicated (&optional shell fast split switch proc)
  "Send expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython (&optional dedicated fast split switch proc)
  "Send indent at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython-dedicated (&optional fast split switch proc)
  "Send indent at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython3 (&optional dedicated fast split switch proc)
  "Send indent at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython3-dedicated (&optional fast split switch proc)
  "Send indent at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-jython (&optional dedicated fast split switch proc)
  "Send indent at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-jython-dedicated (&optional fast split switch proc)
  "Send indent at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python (&optional dedicated fast split switch proc)
  "Send indent at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python-dedicated (&optional fast split switch proc)
  "Send indent at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python2 (&optional dedicated fast split switch proc)
  "Send indent at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python2-dedicated (&optional fast split switch proc)
  "Send indent at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python3 (&optional dedicated fast split switch proc)
  "Send indent at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python3-dedicated (&optional fast split switch proc)
  "Send indent at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-pypy (&optional dedicated fast split switch proc)
  "Send indent at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-pypy-dedicated (&optional fast split switch proc)
  "Send indent at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent (&optional shell dedicated fast split switch proc)
  "Send indent at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-dedicated (&optional shell fast split switch proc)
  "Send indent at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython (&optional dedicated fast split switch proc)
  "Send line at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython-dedicated (&optional fast split switch proc)
  "Send line at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython3 (&optional dedicated fast split switch proc)
  "Send line at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython3-dedicated (&optional fast split switch proc)
  "Send line at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-jython (&optional dedicated fast split switch proc)
  "Send line at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-jython-dedicated (&optional fast split switch proc)
  "Send line at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python (&optional dedicated fast split switch proc)
  "Send line at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python-dedicated (&optional fast split switch proc)
  "Send line at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python2 (&optional dedicated fast split switch proc)
  "Send line at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python2-dedicated (&optional fast split switch proc)
  "Send line at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python3 (&optional dedicated fast split switch proc)
  "Send line at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python3-dedicated (&optional fast split switch proc)
  "Send line at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-pypy (&optional dedicated fast split switch proc)
  "Send line at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-pypy-dedicated (&optional fast split switch proc)
  "Send line at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line (&optional shell dedicated fast split switch proc)
  "Send line at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-dedicated (&optional shell fast split switch proc)
  "Send line at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython (&optional dedicated fast split switch proc)
  "Send minor-block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython-dedicated (&optional fast split switch proc)
  "Send minor-block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython3 (&optional dedicated fast split switch proc)
  "Send minor-block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython3-dedicated (&optional fast split switch proc)
  "Send minor-block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-jython (&optional dedicated fast split switch proc)
  "Send minor-block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-jython-dedicated (&optional fast split switch proc)
  "Send minor-block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python (&optional dedicated fast split switch proc)
  "Send minor-block at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python-dedicated (&optional fast split switch proc)
  "Send minor-block at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python2 (&optional dedicated fast split switch proc)
  "Send minor-block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python2-dedicated (&optional fast split switch proc)
  "Send minor-block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python3 (&optional dedicated fast split switch proc)
  "Send minor-block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python3-dedicated (&optional fast split switch proc)
  "Send minor-block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-pypy (&optional dedicated fast split switch proc)
  "Send minor-block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-pypy-dedicated (&optional fast split switch proc)
  "Send minor-block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block (&optional shell dedicated fast split switch proc)
  "Send minor-block at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-dedicated (&optional shell fast split switch proc)
  "Send minor-block at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython (&optional dedicated fast split switch proc)
  "Send paragraph at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython-dedicated (&optional fast split switch proc)
  "Send paragraph at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython3 (&optional dedicated fast split switch proc)
  "Send paragraph at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython3-dedicated (&optional fast split switch proc)
  "Send paragraph at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-jython (&optional dedicated fast split switch proc)
  "Send paragraph at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-jython-dedicated (&optional fast split switch proc)
  "Send paragraph at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python (&optional dedicated fast split switch proc)
  "Send paragraph at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python-dedicated (&optional fast split switch proc)
  "Send paragraph at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python2 (&optional dedicated fast split switch proc)
  "Send paragraph at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python2-dedicated (&optional fast split switch proc)
  "Send paragraph at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python3 (&optional dedicated fast split switch proc)
  "Send paragraph at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python3-dedicated (&optional fast split switch proc)
  "Send paragraph at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-pypy (&optional dedicated fast split switch proc)
  "Send paragraph at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-pypy-dedicated (&optional fast split switch proc)
  "Send paragraph at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph (&optional shell dedicated fast split switch proc)
  "Send paragraph at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-dedicated (&optional shell fast split switch proc)
  "Send paragraph at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython (&optional dedicated fast split switch proc)
  "Send partial-expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython3 (&optional dedicated fast split switch proc)
  "Send partial-expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython3-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-jython (&optional dedicated fast split switch proc)
  "Send partial-expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-jython-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python (&optional dedicated fast split switch proc)
  "Send partial-expression at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python2 (&optional dedicated fast split switch proc)
  "Send partial-expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python2-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python3 (&optional dedicated fast split switch proc)
  "Send partial-expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python3-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-pypy (&optional dedicated fast split switch proc)
  "Send partial-expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-pypy-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression (&optional shell dedicated fast split switch proc)
  "Send partial-expression at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-dedicated (&optional shell fast split switch proc)
  "Send partial-expression at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-region-ipython (beg end &optional dedicated fast split switch proc)
  "Send region at point to a python3 interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython-dedicated (beg end &optional fast split switch proc)
  "Send region at point to a python3 unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython3 (beg end &optional dedicated fast split switch proc)
  "Send region at point to a python3 interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython3 dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython3-dedicated (beg end &optional fast split switch proc)
  "Send region at point to a python3 unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython3 t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-jython (beg end &optional dedicated fast split switch proc)
  "Send region at point to a python3 interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'jython dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-jython-dedicated (beg end &optional fast split switch proc)
  "Send region at point to a python3 unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'jython t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python (beg end &optional dedicated fast split switch proc)
  "Send region at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python-dedicated (beg end &optional fast split switch proc)
  "Send region at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python2 (beg end &optional dedicated fast split switch proc)
  "Send region at point to a python3 interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python2 dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python2-dedicated (beg end &optional fast split switch proc)
  "Send region at point to a python3 unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python2 t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python3 (beg end &optional dedicated fast split switch proc)
  "Send region at point to a python3 interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python3 dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python3-dedicated (beg end &optional fast split switch proc)
  "Send region at point to a python3 unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python3 t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-pypy (beg end &optional dedicated fast split switch proc)
  "Send region at point to a python3 interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'pypy dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-pypy-dedicated (beg end &optional fast split switch proc)
  "Send region at point to a python3 unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'pypy t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region (beg end &optional shell dedicated fast split switch proc)
  "Send region at point to a python3 interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region shell dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-dedicated (beg end &optional shell fast split switch proc)
  "Send region at point to a python3 unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region shell t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-statement-ipython (&optional dedicated fast split switch proc)
  "Send statement at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython-dedicated (&optional fast split switch proc)
  "Send statement at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython3 (&optional dedicated fast split switch proc)
  "Send statement at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython3-dedicated (&optional fast split switch proc)
  "Send statement at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-jython (&optional dedicated fast split switch proc)
  "Send statement at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-jython-dedicated (&optional fast split switch proc)
  "Send statement at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python (&optional dedicated fast split switch proc)
  "Send statement at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python-dedicated (&optional fast split switch proc)
  "Send statement at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python2 (&optional dedicated fast split switch proc)
  "Send statement at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python2-dedicated (&optional fast split switch proc)
  "Send statement at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python3 (&optional dedicated fast split switch proc)
  "Send statement at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python3-dedicated (&optional fast split switch proc)
  "Send statement at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-pypy (&optional dedicated fast split switch proc)
  "Send statement at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-pypy-dedicated (&optional fast split switch proc)
  "Send statement at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement (&optional shell dedicated fast split switch proc)
  "Send statement at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-dedicated (&optional shell fast split switch proc)
  "Send statement at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython (&optional dedicated fast split switch proc)
  "Send top-level at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython-dedicated (&optional fast split switch proc)
  "Send top-level at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython3 (&optional dedicated fast split switch proc)
  "Send top-level at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython3-dedicated (&optional fast split switch proc)
  "Send top-level at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-jython (&optional dedicated fast split switch proc)
  "Send top-level at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-jython-dedicated (&optional fast split switch proc)
  "Send top-level at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python (&optional dedicated fast split switch proc)
  "Send top-level at point to a python3 interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python-dedicated (&optional fast split switch proc)
  "Send top-level at point to a python3 unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python2 (&optional dedicated fast split switch proc)
  "Send top-level at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python2-dedicated (&optional fast split switch proc)
  "Send top-level at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python3 (&optional dedicated fast split switch proc)
  "Send top-level at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python3-dedicated (&optional fast split switch proc)
  "Send top-level at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-pypy (&optional dedicated fast split switch proc)
  "Send top-level at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'pypy dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-pypy-dedicated (&optional fast split switch proc)
  "Send top-level at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'pypy t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level (&optional shell dedicated fast split switch proc)
  "Send top-level at point to a python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-dedicated (&optional shell fast split switch proc)
  "Send top-level at point to a python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level shell t switch nil nil nil fast proc wholebuf split)))

;; python-components-execute

(defun py-switch-to-python (eob-p)
  "Switch to the Python process buffer, maybe starting new process.

With EOB-P, go to end of buffer."
  (interactive "p")
  (pop-to-buffer (process-buffer (py-proc)) t) ;Runs python if needed.
  (when eob-p
    (goto-char (point-max))))

;;  Split-Windows-On-Execute forms
(defun py-toggle-split-windows-on-execute (&optional arg)
  "If ‘py-split-window-on-execute’ should be on or off.

optional ARG
  Returns value of ‘py-split-window-on-execute’ switched to."
  (interactive)
  (let ((arg (or arg (if py-split-window-on-execute -1 1))))
    (if (< 0 arg)
        (setq py-split-window-on-execute t)
      (setq py-split-window-on-execute nil))
    (when (called-interactively-p 'any) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
    py-split-window-on-execute))

(defun py-split-windows-on-execute-on (&optional arg)
  "Make sure, ‘py-split-window-on-execute’ according to ARG.

Returns value of ‘py-split-window-on-execute’."
  (interactive "p")
  (let ((arg (or arg 1)))
    (py-toggle-split-windows-on-execute arg))
  (when (called-interactively-p 'any) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

(defun py-split-windows-on-execute-off ()
  "Make sure, ‘py-split-window-on-execute’ is off.

Returns value of ‘py-split-window-on-execute’."
  (interactive)
  (py-toggle-split-windows-on-execute -1)
  (when (called-interactively-p 'any) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

;;  Shell-Switch-Buffers-On-Execute forms
(defun py-toggle-switch-buffers-on-execute (&optional arg)
  "If ‘py-switch-buffers-on-execute-p’ according to ARG.

  Returns value of ‘py-switch-buffers-on-execute-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-switch-buffers-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-switch-buffers-on-execute-p t)
      (setq py-switch-buffers-on-execute-p nil))
    (when (called-interactively-p 'any) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
    py-switch-buffers-on-execute-p))

(defun py-switch-buffers-on-execute-on (&optional arg)
  "Make sure, ‘py-switch-buffers-on-execute-p’ according to ARG.

Returns value of ‘py-switch-buffers-on-execute-p’."
  (interactive "p")
  (let ((arg (or arg 1)))
    (py-toggle-switch-buffers-on-execute arg))
  (when (called-interactively-p 'any) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-switch-buffers-on-execute-off ()
  "Make sure, ‘py-switch-buffers-on-execute-p’ is off.

Returns value of ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (py-toggle-switch-buffers-on-execute -1)
  (when (called-interactively-p 'any) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-guess-default-python ()
  "Defaults to \"python\", if guessing didn't succeed."
  (interactive)
  (let* ((ptn (or py-shell-name (py-choose-shell) "python"))
         (erg (if py-edit-only-p ptn (executable-find ptn))))
    (when (called-interactively-p 'any)
      (if erg
          (message "%s" ptn)
        (message "%s" "Could not detect Python on your system")))))

;;  from ipython.el
(defun py-dirstack-hook ()
  "To synchronize dir-changes."
  (make-local-variable 'shell-dirstack)
  (setq shell-dirstack nil)
  (make-local-variable 'shell-last-dir)
  (setq shell-last-dir nil)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t))

(defalias 'py-dedicated-shell 'py-shell-dedicated)
(defun py-shell-dedicated (&optional argprompt)
  "Start an interpreter in another window according to ARGPROMPT.

With optional \\[universal-argument] user is prompted by
‘py-choose-shell’ for command and options to pass to the Python
interpreter."
  (interactive "P")
  (py-shell argprompt nil t))

(defun py-kill-shell-unconditional (&optional shell)
  "With optional argument SHELL.

Otherwise kill default (I)Python shell.
Kill buffer and its process.
Receives a ‘buffer-name’ as argument"
  (interactive)
  (let ((shell (or shell (py-shell))))
    (ignore-errors (py-kill-buffer-unconditional shell))))

(defun py-kill-default-shell-unconditional ()
  "Kill buffer \"\*Python\*\" and its process."
  (interactive)
  (ignore-errors (py-kill-buffer-unconditional "*Python*")))

(defun py--report-executable (buffer)
  (let ((erg (downcase (replace-regexp-in-string
                        "<\\([0-9]+\\)>" ""
                        (replace-regexp-in-string
                         "\*" ""
                         (if
                             (string-match " " buffer)
                             (substring buffer (1+ (string-match " " buffer)))
                           buffer))))))
    (when (string-match "-" erg)
      (setq erg (substring erg 0 (string-match "-" erg))))
    erg))

(defun py--guess-buffer-name (argprompt dedicated)
  "Guess the ‘buffer-name’ core string according to ARGPROMPT DEDICATED."
  (when (and (not dedicated) argprompt
	     (eq 4 (prefix-numeric-value argprompt)))
    (read-buffer "Py-Shell buffer: "
		 (generate-new-buffer-name (py--choose-buffer-name)))))

(defun py--configured-shell (name)
  "Return the configured PATH/TO/STRING if any according to NAME."
  (if (string-match "//\\|\\\\" name)
      name
    (cond ((string-match "^[Ii]" name)
	   (or py-ipython-command name))
	  ((string-match "[Pp]ython3" name)
	   (or py-python3-command name))
	  ((string-match "[Pp]ython2" name)
	   (or py-python2-command name))
	  ((string-match "[Jj]ython" name)
	   (or py-jython-command name))
	  (t (or py-python-command name)))))

(defun py--determine-local-default ()
  (if (not (string= "" py-shell-local-path))
      (expand-file-name py-shell-local-path)
    (when py-use-local-default
      (error "Abort: ‘py-use-local-default’ is set to t but ‘py-shell-local-path’ is empty. Maybe call ‘y-toggle-local-default-use’"))))

(defun py-switch-to-shell ()
  "Switch to Python process buffer."
  (interactive)
  (pop-to-buffer (py-shell) t))

;;  Code execution commands

(defun py--store-result-maybe (erg)
  "If no error occurred and ‘py-store-result-p’ store ERG for yank."
  (and (not py-error) erg (or py-debug-p py-store-result-p) (kill-new erg)))

(defun py-current-working-directory ()
  "Return the directory of current python SHELL."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 erg)
    (if proc
	(setq erg (py-execute-string (concat "import os\;os.getcwd()") proc nil t))
      (setq erg (replace-regexp-in-string "\n" "" (shell-command-to-string (concat py-shell-name " -c \"import os; print(os.getcwd())\"")))))
    (when (called-interactively-p 'interactive)
      (message "CWD: %s" erg))
    erg))

(defun py-set-working-directory (&optional directory)
  "Set working directory according to optional DIRECTORY.

When given, to value of ‘py-default-working-directory’ otherwise"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (dir (or directory py-default-working-directory))
	 erg)
    ;; (py-execute-string (concat "import os\;os.chdir(\"" dir "\")") proc nil t)
    (py-execute-string (concat "import os\;os.chdir(\"" dir "\")") proc nil t)
    (setq erg (py-execute-string "os.getcwd()" proc nil t))
    (when (called-interactively-p 'interactive)
      (message "CWD changed to: %s" erg))
    erg))

(defun py--update-execute-directory-intern (dir proc procbuf fast)
  (let ((strg (concat "import os\;os.chdir(\"" dir "\")")))
    (if fast
	(py-fast-send-string strg proc procbuf t t)
      (py-execute-string strg proc nil t))))
;; (comint-send-string proc (concat "import os;os.chdir(\"" dir "\")\n")))

(defun py--update-execute-directory (proc procbuf execute-directory fast)
  (with-current-buffer procbuf
    (let ((cwd (py-current-working-directory)))
      (unless (string= execute-directory (concat cwd "/"))
	(py--update-execute-directory-intern (or py-execute-directory execute-directory) proc procbuf fast)))))

(defun py--close-execution (tempbuf tempfile)
  "Delete TEMPBUF and TEMPFILE."
  (unless py-debug-p
    (when tempfile (py-delete-temporary tempfile tempbuf))))

(defun py--python-send-setup-code-intern (name buffer)
  "Send setup code to BUFFER according to NAME, a string."
  (save-excursion
    (let ((setup-file (concat (py--normalize-directory py-temp-directory) "py-" name "-setup-code.py"))
	  py-return-result-p py-store-result-p)
      (unless (file-readable-p setup-file)
	(with-temp-buffer
	  (insert (eval (car (read-from-string (concat "py-" name "-setup-code")))))
	  (write-file setup-file)))
      (py--execute-file-base setup-file (get-buffer-process buffer) nil buffer)
      )))

(defun py--python-send-completion-setup-code (buffer)
  "For Python see py--python-send-setup-code.
Argument BUFFER the buffer completion code is sent to."
  (py--python-send-setup-code-intern "shell-completion" buffer))

(defun py--ipython-import-module-completion ()
  "Setup IPython v0.11 or greater.

Used by ‘py-ipython-module-completion-string’"
  (let ((setup-file (concat (py--normalize-directory py-temp-directory) "py-ipython-module-completion.py")))
    (unless (file-readable-p setup-file)
      (with-temp-buffer
	(insert py-ipython-module-completion-code)
	(write-file setup-file)))
    (py--execute-file-base setup-file nil nil (current-buffer) nil t)))

(defun py-delete-temporary (&optional file filebuf)
  (when (file-readable-p file)
    (delete-file file))
  (when (buffer-live-p filebuf)
    (set-buffer filebuf)
    (set-buffer-modified-p 'nil)
    (kill-buffer filebuf)))

(defun py--insert-offset-lines (line)
  "Fix offline amount, make error point at the correct LINE."
  (insert (make-string (- line (py-count-lines (point-min) (point))) 10)))

(defun py-execute-string-dedicated (&optional strg shell switch fast)
  "Send the argument STRG to an unique Python interpreter.

Optional SHELL SWITCH FAST
See also ‘py-execute-region’."
  (interactive)
  (let ((strg (or strg (read-from-minibuffer "String: ")))
        (shell (or shell (default-value 'py-shell-name))))
    (with-temp-buffer
      (insert strg)
      (py-execute-region (point-min) (point-max) shell t switch fast))))

(defun py--insert-execute-directory (directory &optional orig done)
  (let ((orig (or orig (point)))
        (done done))
    (if done (goto-char done) (goto-char (point-min)))
    (cond ((re-search-forward "^from __future__ import " nil t 1)
           (py-forward-statement)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          ((re-search-forward py-encoding-string-re nil t 1)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          ((re-search-forward py-shebang-regexp nil t 1)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          (t (forward-line 1)
             (unless (eq 9 (char-after)) (newline 1))
             (insert (concat "import os; os.chdir(\"" directory "\")\n"))))))

;; ‘py-execute-line’ calls void function, lp:1492054,  lp:1519859
(or (functionp 'indent-rigidly-left)
    (defun indent-rigidly--pop-undo ()
      (and (memq last-command '(indent-rigidly-left indent-rigidly-right
						    indent-rigidly-left-to-tab-stop
						    indent-rigidly-right-to-tab-stop))
	   (consp buffer-undo-list)
	   (eq (car buffer-undo-list) nil)
	   (pop buffer-undo-list)))

    (defun indent-rigidly-left (beg end)
      "Indent all lines between BEG and END leftward by one space."
      (interactive "r")
      (indent-rigidly--pop-undo)
      (indent-rigidly
       beg end
       (if (eq (current-bidi-paragraph-direction) 'right-to-left) 1 -1))))

(defun py--qualified-module-name (file)
  "Return the fully qualified Python module name for FILE.

FILE is a string.  It may be an absolute or a relative path to
any file stored inside a Python package directory, although
typically it would be a (absolute or relative) path to a Python
source code file stored inside a Python package directory.

This collects all directories names that have a __init__.py
file in them, starting with the directory of FILE and moving up."
  (let ((module-name (file-name-sans-extension (file-name-nondirectory file)))
        (dirname     (file-name-directory (expand-file-name file))))
    (while (file-exists-p (expand-file-name "__init__.py" dirname))
      (setq module-name
            (concat
             (file-name-nondirectory (directory-file-name dirname))
             "."
             module-name))
      (setq dirname (file-name-directory (directory-file-name dirname))))
    module-name))

(defun py-execute-import-or-reload (&optional shell)
  "Import the current buffer's file in a Python interpreter.

Optional SHELL
If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do ‘py-execute-buffer’
instead.

If the file local variable ‘py-master-file’ is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of ‘py-execute-import-or-reload-save-p’.

See also `\\[py-execute-region]'.

This may be preferable to `\\[py-execute-buffer]' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions."
  (interactive)
  ;; Check file local variable py-master-file
  (when py-master-file
    (let* ((filename (expand-file-name py-master-file))
           (buffer (or (get-file-buffer filename)
                       (find-file-noselect filename))))
      (set-buffer buffer)))
  (let ((py-shell-name (or shell (py-choose-shell)))
        (file (py--buffer-filename-remote-maybe)))
    (if file
        (let ((proc (or
                     (ignore-errors (get-process (file-name-directory shell)))
                     (get-buffer-process (py-shell nil nil py-dedicated-process-p shell (or shell (default-value 'py-shell-name)))))))
          ;; Maybe save some buffers
          (save-some-buffers (not py-ask-about-save) nil)
          (py--execute-file-base file proc
                                (if (string-match "\\.py$" file)
                                    (let ((m (py--qualified-module-name (expand-file-name file))))
                                      (if (string-match "python2" py-shell-name)
                                          (format "import sys\nif sys.modules.has_key('%s'):\n reload(%s)\nelse:\n import %s\n" m m m)
                                        (format "import sys,imp\nif'%s' in sys.modules:\n imp.reload(%s)\nelse:\n import %s\n" m m m)))
                                  ;; (format "execfile(r'%s')\n" file)
                                  (py-execute-file-command file))))
      (py-execute-buffer))))

;; python-components-intern

;;  Keymap

;;  Utility stuff

(defun py--computer-closing-inner-list ()
  "Compute indentation according to py-closing-list-dedents-bos."
  (if py-closing-list-dedents-bos
      (+ (current-indentation) py-indent-offset)
    (1+ (current-column))))

(defun py--compute-closing-outer-list ()
  "Compute indentation according to py-closing-list-dedents-bos."
  (if py-closing-list-dedents-bos
      (current-indentation)
    (+ (current-indentation) py-indent-offset)))

(defun py-compute-indentation-according-to-list-style (pps)
  "See ‘py-indent-list-style’

Choices are:

\\='line-up-with-first-element (default)
\\='one-level-to-beginning-of-statement
\\='one-level-from-opener

See also py-closing-list-dedents-bos"
  (goto-char (nth 1 pps))
  (cond
   ((and (looking-back py-assignment-re (line-beginning-position))
         ;; flexible-indentation-lp-328842
         (not (eq (match-beginning 0) (line-beginning-position))))
    (+ (current-indentation) py-indent-offset))
   (py-closing-list-dedents-bos
    (current-indentation))
   (t (pcase py-indent-list-style
        (`line-up-with-first-element
         (if (and (eq (car (syntax-after (point))) 4) (save-excursion (forward-char 1) (eolp)))
             ;; asdf = {
             ;;     'a':{
             ;;          'b':3,
             ;;          'c':4"
             ;;
             ;; b is at col 9
             ;; (+ (current-indentation) py-indent-offset) would yield 8
             ;; EOL-case dedent starts if larger at least 2
             (cond ((< 1 (- (1+ (current-column))(+ (current-indentation) py-indent-offset)))
                   (min (+ (current-indentation) py-indent-offset)(1+ (current-column))))
                   (t (1+ (current-column))))
           (1+ (current-column))))
        (`one-level-to-beginning-of-statement
         (+ (current-indentation) py-indent-offset))
        (`one-level-from-first-element
         (+ 1 (current-column) py-indent-offset))))))

(defun py-compute-indentation-closing-list (pps)
  (cond
   ((< 1 (nth 0 pps))
    (goto-char (nth 1 pps))
    ;; reach the outer list
    (goto-char (nth 1 (parse-partial-sexp (point-min) (point))))
    (py--computer-closing-inner-list))
   ;; just close an maybe outer list
   ((eq 1 (nth 0 pps))
    (goto-char (nth 1 pps))
    (py-compute-indentation-according-to-list-style pps))))

(defun py-compute-indentation-in-list (pps line closing orig)
  (if closing
      (py-compute-indentation-closing-list pps)
    (cond ((and (not line) (looking-back py-assignment-re (line-beginning-position)))
	   (py--fetch-indent-statement-above orig))
	  ;; (py-compute-indentation-according-to-list-style pps iact orig origline line nesting repeat indent-offset liep)
	  (t (when (looking-back "[ \t]*\\(\\s(\\)" (line-beginning-position))
	       (goto-char (match-beginning 1))
	       (setq pps (parse-partial-sexp (point-min) (point))))
	     (py-compute-indentation-according-to-list-style pps)))))

(defun py-compute-comment-indentation (pps iact orig origline closing line nesting repeat indent-offset liep)
  (cond ((nth 8 pps)
         (goto-char (nth 8 pps))
         (cond ((and line (eq (current-column) (current-indentation)))
                (current-indentation))
               ((and (eq liep (line-end-position))py-indent-honors-inline-comment)
                (current-column))
               ((py--line-backward-maybe)
                (setq line t)
                (skip-chars-backward " \t")
                (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
               (t (if py-indent-comments
                      (progn
                        (py-backward-comment)
                        (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
                    0))))
        ((and
          (looking-at (concat "[ \t]*" comment-start))
          (looking-back "^[ \t]*" (line-beginning-position))(not line)
          (eq liep (line-end-position)))
         (if py-indent-comments
             (progn
               (setq line t)
               (skip-chars-backward " \t\r\n\f")
               ;; as previous comment-line might
               ;; be wrongly unindented, travel
               ;; whole commented section
               (py-backward-comment)
               (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
           0))
        ((and
          (looking-at (concat "[ \t]*" comment-start))
          (looking-back "^[ \t]*" (line-beginning-position))
          (not (eq liep (line-end-position))))
         (current-indentation))
        ((and (eq 11 (syntax-after (point))) line py-indent-honors-inline-comment)
         (current-column))))

(defun py-compute-indentation--at-closer-maybe (pps)
  (save-excursion
    (when (looking-back "^[ \t]*\\(\\s)\\)" (line-beginning-position))
      (forward-char -1)
      (setq pps (parse-partial-sexp (point-min) (point))))
    (when (and (nth 1 pps)
               (looking-at "[ \t]*\\(\\s)\\)") (nth 0 pps))
      (cond
       ;; no indent at empty argument (list
       ((progn (skip-chars-backward " \t\r\n\f") (ignore-errors (eq 4 (car (syntax-after (1- (point)))))))
        (current-indentation))
       ;; beyond list start?
       ((ignore-errors (< (progn (unless (bobp) (forward-line -1) (line-beginning-position))) (nth 1 (setq pps (parse-partial-sexp (point-min) (point))))))
        (py-compute-indentation-according-to-list-style pps))
       (py-closing-list-dedents-bos
        (- (current-indentation) py-indent-offset))
       (t (current-indentation))))))

(defun py-compute-indentation (&optional iact orig origline closing line nesting repeat indent-offset liep)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as ‘return’,
‘raise’, ‘break’, ‘continue’, and ‘pass’ force one level of dedenting.

ORIG keeps original position
ORIGLINE keeps line where compute started
CLOSING is t when started at a char delimiting a list as \"]})\"
LINE indicates being not at origline now
NESTING is currently ignored, if executing from inside a list
REPEAT counter enables checks against ‘py-max-specpdl-size’
INDENT-OFFSET allows calculation of block-local values
LIEP stores line-end-position at point-of-interest
"
  (interactive "p")
  (save-excursion
    (save-restriction
      (widen)
      ;; in shell, narrow from previous prompt
      ;; needed by closing
      (let* ((orig (or orig (copy-marker (point))))
             (origline (or origline (py-count-lines (point-min) (point))))
             ;; closing indicates: when started, looked
             ;; at a single closing parenthesis
             ;; line: moved already a line backward
             (liep (or liep (line-end-position)))
	     (line (or line (not (eq origline (py-count-lines (point-min) (point))))))
             ;; (line line)
             (pps (progn
		    (unless (eq (current-indentation) (current-column))(skip-chars-backward " " (line-beginning-position)))
		    ;; (when (eq 5 (car (syntax-after (1- (point)))))
		    ;;   (forward-char -1))
		    (parse-partial-sexp (point-min) (point))))
             (closing
              (or closing
                  ;; returns update pps
                  ;; (and line (py-compute-indentation--at-closer-maybe pps))
                  (py-compute-indentation--at-closer-maybe pps)))
             ;; in a recursive call already
             (repeat (if repeat
                         (setq repeat (1+ repeat))
                       0))
             ;; nesting: started nesting a list
             (nesting nesting)
             (cubuf (current-buffer))
             erg indent this-line)
        (if (and (< repeat 1)
                 (and (comint-check-proc (current-buffer))
                      (re-search-backward (concat py-shell-prompt-regexp "\\|" py-ipython-output-prompt-re "\\|" py-ipython-input-prompt-re) nil t 1)))
            ;; common recursion not suitable because of prompt
            (with-temp-buffer
              ;; (switch-to-buffer (current-buffer))
              (insert-buffer-substring cubuf (match-end 0) orig)
              (python-mode)
              (setq indent (py-compute-indentation)))
          (if (< py-max-specpdl-size repeat)
              (error "‘py-compute-indentation’ reached loops max.")
            (setq nesting (nth 0 pps))
            (setq indent
                  (cond ;; closing)
                   ((bobp)
		    (cond ((eq liep (line-end-position))
                           0)
			  ;; - ((looking-at py-outdent-re)
			  ;; - (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation)))
			  ((and line (looking-at py-block-or-clause-re))
			   py-indent-offset)
                          ((looking-at py-outdent-re)
                           (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation)))
                          (t
                           (current-indentation))))
                   ;; (cond ((eq liep (line-end-position))
                   ;;        0)
                   ;;       ((looking-at py-outdent-re)
                   ;;        (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation)))
                   ;;       (t
                   ;;        (current-indentation)))
		   ;; in string
		   ((and (nth 3 pps) (nth 8 pps))
		    (cond
		     ((py--docstring-p (nth 8 pps))
		      (save-excursion
			;; (goto-char (match-beginning 0))
			(back-to-indentation)
			(if (looking-at "[uUrR]?\"\"\"\\|[uUrR]?'''")
			    (progn
			      (skip-chars-backward " \t\r\n\f")
			      (back-to-indentation)
			      (if (looking-at py-def-or-class-re)
				  (+ (current-column) py-indent-offset)
				(current-indentation)))
			  (skip-chars-backward " \t\r\n\f")
			  (back-to-indentation)
			  (current-indentation))))
		     (t 0)))
		   ((and (looking-at "\"\"\"\\|'''") (not (bobp)))
		    (py-backward-statement)
		    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
		   ;; comments
		   ((or
		     (nth 8 pps)
		     (and
		      (looking-at (concat "[ \t]*" comment-start))
		      (looking-back "^[ \t]*" (line-beginning-position))(not line))
		     (and (eq 11 (syntax-after (point))) line py-indent-honors-inline-comment))
		    (py-compute-comment-indentation pps iact orig origline closing line nesting repeat indent-offset liep))
		   ;; lists
		   ((nth 1 pps)
		    (if (< (nth 1 pps) (line-beginning-position))
                        ;; Compute according to ‘py-indent-list-style’

                        ;; Choices are:

                        ;; \\='line-up-with-first-element (default)
                        ;; \\='one-level-to-beginning-of-statement
                        ;; \\='one-level-from-opener"

                        ;; See also py-closing-list-dedents-bos
			(py-compute-indentation-in-list pps line closing orig)
		      (back-to-indentation)
		      (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep)))
		   ((and (eq (char-after) (or ?\( ?\{ ?\[)) line)
		    (1+ (current-column)))
		   ((py-preceding-line-backslashed-p)
		    (progn
		      (py-backward-statement)
		      (setq this-line (py-count-lines))
		      (if (< 1 (- origline this-line))
                          (py--fetch-indent-line-above orig)
			(if (looking-at "from +\\([^ \t\n]+\\) +import")
			    py-backslashed-lines-indent-offset
                          (if (< 20 (line-end-position))
                              8
                          (+ (current-indentation) py-continuation-offset))))))
		   ((and (looking-at py-block-closing-keywords-re)
                         (eq liep (line-end-position)))
		    (skip-chars-backward "[ \t\r\n\f]")
		    (py-backward-statement)
		    (cond ((looking-at py-extended-block-or-clause-re)
			   (+
			    ;; (if py-smart-indentation (py-guess-indent-offset) indent-offset)
			    (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset)
			    (current-indentation)))
                          ((looking-at py-block-closing-keywords-re)
			   (- (current-indentation) (or indent-offset py-indent-offset)))
                          (t (current-column))))
		   ((looking-at py-block-closing-keywords-re)
		    (if (< (line-end-position) orig)
			;; #80, Lines after return cannot be correctly indented
			(if (looking-at "return[ \\t]*$")
			    (current-indentation)
			  (- (current-indentation) (or indent-offset py-indent-offset)))
		      (py-backward-block-or-clause)
		      (current-indentation)))
		   ;; ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
		   ;; (when (py--line-backward-maybe) (setq line t))
		   ;; (car (py--clause-lookup-keyword py-elif-re -1 nil origline)))
		   ((and (looking-at py-minor-clause-re) (not line)
                         (eq liep (line-end-position)))

		    (cond
                     ((looking-at py-case-re)
                      (py--backward-regexp 'py-match-case-re) (+ (current-indentation) py-indent-offset))
                     ((looking-at py-outdent-re)
		      ;; (and (py--backward-regexp 'py-block-or-clause-re) (current-indentation)))
		      (and (py--go-to-keyword 'py-block-or-clause-re (current-indentation) '< t) (current-indentation)))
		     ((bobp) 0)
		     (t (save-excursion
			  ;; (skip-chars-backward " \t\r\n\f")
			  (if (py-backward-block)
			      ;; (py--backward-regexp 'py-block-or-clause-re)
			      (+ py-indent-offset (current-indentation))
			    0)))))
		   ((looking-at py-extended-block-or-clause-re)
		    (cond ((and (not line)
				(eq liep (line-end-position)))
			   (when (py--line-backward-maybe) (setq line t))
			   (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
                          (t (+
			      (cond (indent-offset)
				    (py-smart-indentation
				     (py-guess-indent-offset))
				    (t py-indent-offset))
			      (current-indentation)))))
		   ((and
		     (< (line-end-position) liep)
		     (eq (current-column) (current-indentation)))
		    (and
		     (looking-at py-assignment-re)
		     (goto-char (match-end 0)))
		    ;; multiline-assignment
		    (if (and nesting (looking-at " *[[{(]") (not (looking-at ".+[]})][ \t]*$")))
			(+ (current-indentation) (or indent-offset py-indent-offset))
		      (current-indentation)))
		   ((looking-at py-assignment-re)
		    (py-backward-statement)
		    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
		   ((and (< (current-indentation) (current-column))(not line))
		    (back-to-indentation)
		    (unless line
		      (setq nesting (nth 0 (parse-partial-sexp (point-min) (point)))))
		    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
		   ((and (not (py--beginning-of-statement-p)) (not (and line (eq 11 (syntax-after (point))))))
		    (if (bobp)
			(current-column)
		      (if (eq (point) orig)
                          (progn
			    (when (py--line-backward-maybe) (setq line t))
			    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
			(py-backward-statement)
			(py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))))
		   ((or (py--statement-opens-block-p py-extended-block-or-clause-re) (looking-at "@"))
		    (if (< (py-count-lines) origline)
			(+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation))
		      (skip-chars-backward " \t\r\n\f")
		      (setq line t)
		      (back-to-indentation)
		      (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep)))
		   ((and py-empty-line-closes-p (py--after-empty-line))
		    (progn (py-backward-statement)
			   (- (current-indentation) (or indent-offset py-indent-offset))))
		   ;; still at orignial line
		   ((and (eq liep (line-end-position))
                         (save-excursion
			   (and (setq erg (py--go-to-keyword 'py-extended-block-or-clause-re (* py-indent-offset 99)))
				;; maybe Result: (nil nil nil), which evaluates to ‘t’
				(not (bobp))
				(if (and (not indent-offset) py-smart-indentation) (setq indent-offset (py-guess-indent-offset)) t)
				(ignore-errors (< orig (or (py-forward-block-or-clause) (point)))))))
		    (+ (car erg) (if py-smart-indentation
				     (or indent-offset (py-guess-indent-offset))
				   (or indent-offset py-indent-offset))))
		   ((and (not line)
                         (eq liep (line-end-position))
                         (py--beginning-of-statement-p))
		    (py-backward-statement)
		    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
		   (t (current-indentation))))
            (when py-verbose-p (message "%s" indent))
            indent))))))

(defun py--uncomment-intern (beg end)
  (uncomment-region beg end)
  (when py-uncomment-indents-p
    (py-indent-region beg end)))

(defun py-uncomment (&optional beg)
  "Uncomment commented lines at point.

If region is active, restrict uncommenting at region "
  (interactive "*")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (region-beginning) (region-end)))
      (let* (last
             (beg (or beg (save-excursion
                            (while (and (py-backward-comment) (setq last (point))(prog1 (forward-line -1)(end-of-line))))
                            last))))
        (and (py-forward-comment))
        (py--uncomment-intern beg (point))))))

(defun py-load-named-shells ()
  (interactive)
  (dolist (ele py-known-shells)
    (let ((erg (py-install-named-shells-fix-doc ele)))
      (eval (fset (car (read-from-string ele)) (car
						(read-from-string (concat "(lambda (&optional dedicated args) \"Start a `" erg "' interpreter.
Optional DEDICATED: with \\\\[universal-argument] start in a new
dedicated shell.
Optional ARGS overriding `py-" ele "-command-args'.

Calls ‘py-shell’
\"
  (interactive \"p\") (py-shell dedicated args nil \""ele"\"))")))))))
  (when (functionp (car (read-from-string (car-safe py-known-shells))))
    (when py-verbose-p (message "py-load-named-shells: %s" "installed named-shells"))))

;; (py-load-named-shells)

(defun py-load-file (file-name)
  "Load a Python file FILE-NAME into the Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive "f")
  (py--execute-file-base file-name (get-buffer-process (get-buffer (py-shell)))))

;;  Hooks
;;  arrange to kill temp files when Emacs exists

(when py--warn-tmp-files-left-p
  (add-hook 'python-mode-hook 'py--warn-tmp-files-left))

(defun py-guess-pdb-path ()
  "If py-pdb-path isn't set, find location of pdb.py. "
  (interactive)
  (let ((ele (split-string (shell-command-to-string "whereis python")))
        erg)
    (while (or (not erg)(string= "" erg))
      (when (and (string-match "^/" (car ele)) (not (string-match "/man" (car ele))))
        (setq erg (shell-command-to-string (concat "find " (car ele) " -type f -name \"pdb.py\""))))
      (setq ele (cdr ele)))
    (if erg
        (message "%s" erg)
      (message "%s" "pdb.py not found, please customize ‘py-pdb-path’"))
    erg))

(if py-mode-output-map
    nil
  (setq py-mode-output-map (make-sparse-keymap))
  (define-key py-mode-output-map [button2]  'py-mouseto-exception)
  (define-key py-mode-output-map "\C-c\C-c" 'py-goto-exception)
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Python Output* buffer being read-only
  (mapc #' (lambda (key)
             (define-key py-mode-output-map key
               #'(lambda () (interactive) (beep))))
           (where-is-internal 'self-insert-command)))

(defun py-toggle-comment-auto-fill (&optional arg)
  "Toggles comment-auto-fill mode"
  (interactive "P")
  (if (or (and arg (< 0 (prefix-numeric-value arg)))
	  (and (boundp 'py-comment-auto-fill-p)(not py-comment-auto-fill-p)))
      (progn
        (set (make-local-variable 'py-comment-auto-fill-p) t)
        (setq fill-column py-comment-fill-column)
        (auto-fill-mode 1))
    (set (make-local-variable 'py-comment-auto-fill-p) nil)
    (auto-fill-mode -1)))

(defun py-comment-auto-fill-on ()
  (interactive)
  (py-toggle-comment-auto-fill 1))

(defun py-comment-auto-fill-off ()
  (interactive)
  (py-toggle-comment-auto-fill -1))

(defun py--set-auto-fill-values ()
  "Internal use by ‘py--run-auto-fill-timer’"
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (cond ((and (nth 4 pps)(numberp py-comment-fill-column))
           (setq fill-column py-comment-fill-column))
          ((and (nth 3 pps)(numberp py-docstring-fill-column))
           (setq fill-column py-docstring-fill-column))
          (t (setq fill-column py-fill-column-orig)))))

(defun py--run-auto-fill-timer ()
  "Set fill-column to values according to environment.

‘py-docstring-fill-column’ resp. to ‘py-comment-fill-column’."
  (when py-auto-fill-mode
    (unless py-autofill-timer
      (setq py-autofill-timer
            (run-with-idle-timer
             py-autofill-timer-delay t
             'py--set-auto-fill-values)))))

;;  unconditional Hooks
;;  (orgstruct-mode 1)
(declare-function py-complete "pycomplete" ())
(defun py-complete-auto ()
  "Auto-complete function using py-complete. "
  ;; disable company
  ;; (when company-mode (company-mode))
  (let ((modified (buffer-chars-modified-tick)))
    ;; don't try completion if buffer wasn't modified
    (unless (eq modified py-complete-last-modified)
      (if py-auto-completion-mode-p
          (if (string= "*PythonCompletions*" (buffer-name (current-buffer)))
              (sit-for 0.1 t)
            (if
                (eq py-auto-completion-buffer (current-buffer))
                ;; not after whitespace, TAB or newline
                (unless (member (char-before) (list 32 9 10))
                  (py-complete)
                  (setq py-complete-last-modified (buffer-chars-modified-tick)))
              (setq py-auto-completion-mode-p nil
                    py-auto-completion-buffer nil)
              (cancel-timer py--auto-complete-timer)))))))

;;  End-of- p

;;  Opens
(defun py--statement-opens-block-p (&optional regexp)
  "Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. "
  (let* ((regexp (or regexp py-block-or-clause-re))
         (erg (py--statement-opens-base regexp)))
    erg))

(defun py--statement-opens-base (regexp)
  (let ((orig (point))
        erg)
    (save-excursion
      (back-to-indentation)
      (py-forward-statement)
      (py-backward-statement)
      (when (and
             (<= (line-beginning-position) orig)(looking-back "^[ \t]*" (line-beginning-position))(looking-at regexp))
        (setq erg (point))))
    erg))

(defun py--statement-opens-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-clause-re))

(defun py--statement-opens-block-or-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-block-or-clause-re))

(defun py--statement-opens-class-p ()
  "If the statement opens a functions or class.

Return ‘t’, nil otherwise. "
  (py--statement-opens-base py-class-re))

(defun py--statement-opens-def-p ()
  "If the statement opens a functions or class.
Return ‘t’, nil otherwise. "
  (py--statement-opens-base py-def-re))

(defun py--statement-opens-def-or-class-p ()
  "If the statement opens a functions or class definition.
Return ‘t’, nil otherwise. "
  (py--statement-opens-base py-def-or-class-re))

(defun py--down-top-level (&optional regexp)
  "Go to the end of a top-level form.

When already at end, go to EOB."
  (end-of-line)
  (while (and (py--forward-regexp (or regexp "^[[:graph:]]"))
	      (save-excursion
		(beginning-of-line)
		(or
		 (looking-at py-clause-re)
		 (looking-at comment-start)))))
  (beginning-of-line)
  (and (looking-at regexp) (point)))

(defun py--end-of-paragraph (regexp)
  (let* ((regexp (if (symbolp regexp) (symbol-value regexp)
                   regexp)))
    (while (and (not (eobp)) (re-search-forward regexp nil 'move 1) (nth 8 (parse-partial-sexp (point-min) (point)))))))

(defun py--look-downward-for-beginning (regexp)
  "When above any beginning of FORM, search downward. "
  (let* ((orig (point))
         (erg orig)
         pps)
    (while (and (not (eobp)) (re-search-forward regexp nil t 1) (setq erg (match-beginning 0)) (setq pps (parse-partial-sexp (point-min) (point)))
                (or (nth 8 pps) (nth 1 pps))))
    (cond ((not (or (nth 8 pps) (nth 1 pps) (or (looking-at comment-start))))
           (when (ignore-errors (< orig erg))
             erg)))))

(defun py-look-downward-for-clause (&optional ind orig regexp)
  "If beginning of other clause exists downward in current block.

If succesful return position. "
  (interactive)
  (unless (eobp)
    (let ((ind (or ind
                   (save-excursion
                     (py-backward-statement)
                     (if (py--statement-opens-block-p)
                         (current-indentation)
                       (- (current-indentation) py-indent-offset)))))
          (orig (or orig (point)))
          (regexp (or regexp py-extended-block-or-clause-re))
          erg)
      (end-of-line)
      (when (re-search-forward regexp nil t 1)
        (when (nth 8 (parse-partial-sexp (point-min) (point)))
          (while (and (re-search-forward regexp nil t 1)
                      (nth 8 (parse-partial-sexp (point-min) (point))))))
        ;; (setq last (point))
        (back-to-indentation)
        (unless (and (looking-at py-clause-re)
                     (not (nth 8 (parse-partial-sexp (point-min) (point)))) (eq (current-indentation) ind))
          (progn (setq ind (current-indentation))
                 (while (and (py-forward-statement-bol)(not (looking-at py-clause-re))(<= ind (current-indentation)))))
          (if (and (looking-at py-clause-re)
                   (not (nth 8 (parse-partial-sexp (point-min) (point))))
                   (< orig (point)))
              (setq erg (point))
            (goto-char orig))))
      erg)))

(defun py-current-defun ()
  "Go to the outermost method or class definition in current scope.

Python value for ‘add-log-current-defun-function’.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables ‘py-current-defun-show’ and ‘py-current-defun-delay’."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (let ((erg (when (py-backward-def-or-class)
                   (forward-word 1)
                   (skip-chars-forward " \t")
                   (prin1-to-string (symbol-at-point)))))
        (when (and erg py-current-defun-show)
          (push-mark (point) t t) (skip-chars-forward "^ (")
          (exchange-point-and-mark)
          (sit-for py-current-defun-delay t))
        erg))))

(defun py--join-words-wrapping (words separator prefix line-length)
  (let ((lines ())
        (current-line prefix))
    (while words
      (let* ((word (car words))
             (maybe-line (concat current-line word separator)))
        (if (> (length maybe-line) line-length)
            (setq lines (cons (substring current-line 0 -1) lines)
                  current-line (concat prefix word separator " "))
          (setq current-line (concat maybe-line " "))))
      (setq words (cdr words)))
    (setq lines (cons (substring current-line 0 (- 0 (length separator) 1)) lines))
    (mapconcat 'identity (nreverse lines) "\n")))

(defun py-sort-imports ()
  "Sort multiline imports.

Put point inside the parentheses of a multiline import and hit
\\[py-sort-imports] to sort the imports lexicographically"
  (interactive)
  (save-excursion
    (let ((open-paren (ignore-errors (save-excursion (progn (up-list -1) (point)))))
          (close-paren (ignore-errors (save-excursion (progn (up-list 1) (point)))))
          sorted-imports)
      (when (and open-paren close-paren)
        (goto-char (1+ open-paren))
        (skip-chars-forward " \n\t")
        (setq sorted-imports
              (sort
               (delete-dups
                (split-string (buffer-substring
                               (point)
                               (save-excursion (goto-char (1- close-paren))
                                               (skip-chars-backward " \n\t")
                                               (point)))
                              ", *\\(\n *\\)?"))
               ;; XXX Should this sort case insensitively?
               'string-lessp))
        ;; Remove empty strings.
        (delete-region open-paren close-paren)
        (goto-char open-paren)
        (insert "(\n")
        (insert (py--join-words-wrapping (remove "" sorted-imports) "," "    " 78))
        (insert ")")))))

(defun py--in-literal (&optional lim)
  "Return non-nil if point is in a Python literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  (let* ((lim (or lim (point-min)))
         (state (parse-partial-sexp lim (point))))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment))))

(defconst py-help-address "python-mode@python.org"
  "List dealing with usage and developing python-mode.

Also accepts submission of bug reports, whilst a ticket at
‘https://gitlab.com/python-mode-devs/python-mode/-/issues’
is preferable for that. ")

;;  Utilities

(defun py-install-local-shells (&optional local)
  "Builds Python-shell commands from executable found in LOCAL.

If LOCAL is empty, shell-command ‘find’ searches beneath current directory.
Eval resulting buffer to install it, see customizable ‘py-extensions’. "
  (interactive)
  (let* ((local-dir (if local
                        (expand-file-name local)
                      (read-from-minibuffer "Virtualenv directory: " default-directory)))
         (path-separator (if (string-match "/" local-dir)
                             "/"
                           "\\" t))
         (shells (split-string (shell-command-to-string (concat "find " local-dir " -maxdepth 9 -type f -executable -name \"*python\""))))
         prefix end orig curexe aktpath)
    (set-buffer (get-buffer-create py-extensions))
    (erase-buffer)
    (dolist (elt shells)
      (setq prefix "")
      (setq curexe (substring elt (1+ (string-match "/[^/]+$" elt))))
      (setq aktpath (substring elt 0 (1+ (string-match "/[^/]+$" elt))))
      (dolist (prf (split-string aktpath (regexp-quote path-separator)))
        (unless (string= "" prf)
          (setq prefix (concat prefix (substring prf 0 1)))))
      (setq orig (point))
      (insert py-shell-template)
      (setq end (point))
      (goto-char orig)
      (when (re-search-forward "\\<NAME\\>" end t 1)
        (replace-match (concat prefix "-" (substring elt (1+ (save-match-data (string-match "/[^/]+$" elt)))))t))
      (goto-char orig)
      (while (search-forward "DOCNAME" end t 1)
        (replace-match (if (string= "ipython" curexe)
                           "IPython"
                         (capitalize curexe)) t))
      (goto-char orig)
      (when (search-forward "FULLNAME" end t 1)
        (replace-match elt t))
      (goto-char (point-max)))
    (emacs-lisp-mode)
    (if (file-readable-p (concat py-install-directory "/" py-extensions))
        (find-file (concat py-install-directory "/" py-extensions)))))

(defun py--until-found (search-string liste)
  "Search liste for search-string until found. "
  (let ((liste liste) element)
    (while liste
      (if (member search-string (car liste))
          (setq element (car liste) liste nil))
      (setq liste (cdr liste)))
    (when element
      (while (and element (not (numberp element)))
        (if (member search-string (car element))
            (setq element (car element))
          (setq element (cdr element))))
      element)))

(defun py--report-end-marker (process)
  ;; (message "py--report-end-marker in %s" (current-buffer))
  (if (derived-mode-p 'comint-mode)
      (if (bound-and-true-p comint-last-prompt)
	  (car-safe comint-last-prompt)
	(dotimes (_ 3) (when (not (bound-and-true-p comint-last-prompt))(sit-for 1 t)))
	(and (bound-and-true-p comint-last-prompt)
	     (car-safe comint-last-prompt)))
    (if (markerp (process-mark process))
	(process-mark process)
      (progn
	(dotimes (_ 3) (when (not (markerp (process-mark process)))(sit-for 1 t)))
	(process-mark process)))))

(defun py-which-def-or-class (&optional orig)
  "Returns concatenated ‘def’ and ‘class’ names.

In hierarchical order, if cursor is inside.

Returns \"???\" otherwise
Used by variable ‘which-func-functions’ "
  (interactive)
  (let* ((orig (or orig (point)))
         (backindent 99999)
         (re py-def-or-class-re
          ;; (concat py-def-or-class-re "\\([[:alnum:]_]+\\)")
          )
         erg forward indent backward limit)
    (if
        (and (looking-at re)
             (not (nth 8 (parse-partial-sexp (point-min) (point)))))
        (progn
          (setq erg (list (match-string-no-properties 2)))
          (setq backindent (current-indentation)))
      ;; maybe inside a definition's symbol
      (or (eolp) (and (looking-at "[[:alnum:]]")(forward-word 1))))
    (if
        (and (not (and erg (eq 0 (current-indentation))))
             (setq limit (py-backward-top-level))
             (looking-at re))
        (progn
          (push (match-string-no-properties 2)  erg)
          (setq indent (current-indentation)))
      (goto-char orig)
      (while (and
              (re-search-backward py-def-or-class-re limit t 1)
              (< (current-indentation) backindent)
              (setq backindent (current-indentation))
              (setq backward (point))
              (or (< 0 (current-indentation))
                  (nth 8 (parse-partial-sexp (point-min) (point))))))
      (when (and backward
                 (goto-char backward)
                 (looking-at re))
        (push (match-string-no-properties 2)  erg)
        (setq indent (current-indentation))))
    ;; (goto-char orig))
    (if erg
        (progn
          (end-of-line)
          (while (and (re-search-forward py-def-or-class-re nil t 1)
                      (<= (point) orig)
                      (< indent (current-indentation))
                      (or
                       (nth 8 (parse-partial-sexp (point-min) (point)))
                       (setq forward (point)))))
          (if forward
              (progn
                (goto-char forward)
                (save-excursion
                  (back-to-indentation)
                  (and (looking-at re)
                       (setq erg (list (car erg) (match-string-no-properties 2)))
                       ;; (< (py-forward-def-or-class) orig)
                       ;; if match was beyond definition, nil
                       ;; (setq erg nil)
)))
            (goto-char orig))))
    (if erg
        (if (< 1 (length erg))
            (setq erg (mapconcat 'identity erg "."))
          (setq erg (car erg)))
      (setq erg "???"))
    (goto-char orig)
    erg))

(defun py--fetch-first-python-buffer ()
  "Returns first (I)Python-buffer found in ‘buffer-list’"
  (let ((buli (buffer-list))
        erg)
    (while (and buli (not erg))
      (if (string-match "Python" (prin1-to-string (car buli)))
          (setq erg (car buli))
        (setq buli (cdr buli))))
    erg))

(defun py-unload-python-el ()
  "Unloads python-mode delivered by shipped python.el

Removes python-skeleton forms from abbrevs.
These would interfere when inserting forms heading a block"
  (interactive)
  (let (done)
    (when (featurep 'python) (unload-feature 'python t))
    (when (file-readable-p abbrev-file-name)
      (find-file abbrev-file-name)
      (goto-char (point-min))
      (while (re-search-forward "^.+python-skeleton.+$" nil t 1)
        (setq done t)
        (delete-region (match-beginning 0) (1+ (match-end 0))))
      (when done (write-file abbrev-file-name)
            ;; now reload
            (read-abbrev-file abbrev-file-name))
      (kill-buffer (file-name-nondirectory abbrev-file-name)))))

(defmacro py--kill-buffer-unconditional (buffer)
  "Kill buffer unconditional, kill buffer-process if existing. "
  `(let ((proc (get-buffer-process ,buffer))
         kill-buffer-query-functions)
     (ignore-errors
       (and proc (kill-process proc))
       (set-buffer ,buffer)
       (set-buffer-modified-p 'nil)
       (kill-buffer (current-buffer)))))

(defun py-down-top-level ()
  "Go to beginning of next top-level form downward.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg)
    (while (and (not (eobp))
                (progn (end-of-line)
                       (re-search-forward "^[[:alpha:]_'\"]" nil 'move 1))
                (nth 8 (parse-partial-sexp (point-min) (point)))))
    (when (and (not (eobp)) (< orig (point)))
      (goto-char (match-beginning 0))
        (setq erg (point)))
    erg))

(defun py-forward-top-level-bol ()
  "Go to end of top-level form at point, stop at next beginning-of-line.

Returns position successful, nil otherwise"
  (interactive)
  (let (erg)
    (py-forward-top-level)
    (unless (or (eobp) (bolp))
      (forward-line 1)
      (beginning-of-line)
      (setq erg (point)))
    erg))

(defun py-down ()
  "Go to beginning one level below.

Of compound statement or definition at point.

When inside a string, list or comment, jump to its end.
Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        (pps (parse-partial-sexp (point-min) (point))))
    (cond ((eq (syntax-class (syntax-after (point))) 4)
           (forward-sexp))
          ((nth 4 pps)
           (py-forward-comment))
          ((nth 3 pps)
           (goto-char (nth 8 pps))
           (forward-sexp))
          ((nth 1 pps)
           (goto-char (nth 1 pps))
           (forward-sexp))
          ((or (eq (car (syntax-after orig)) 15)(eq (car (syntax-after orig)) 4))
           (forward-sexp))
          ((py--beginning-of-class-p)
           (py-forward-class))
          ((py--beginning-of-def-p)
           (py-forward-def))
          ((py--beginning-of-block-p)
           (py-forward-block))
          ((py--beginning-of-clause-p)
           (py-forward-clause))
          (t
           (py-forward-statement)))
    (and (< orig (point)) (point))))


(defun py--thing-at-point (form &optional mark-decorators)
  "Returns buffer-substring of string-argument FORM as cons.

Text properties are stripped.
If PY-MARK-DECORATORS, ‘def’- and ‘class’-forms include decorators
If BOL is t, from beginning-of-line"
  (interactive)
  (let* ((begform (intern-soft (concat "py-backward-" form)))
         (endform (intern-soft (concat "py-forward-" form)))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (and mark-decorators
         (and (setq erg (py-backward-decorator))
              (setq beg erg)))
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (if (and beg end (<= beg orig) (<= orig end))
        (buffer-substring-no-properties beg end)
      nil)))

(defun py--thing-at-point-bol (form &optional mark-decorators)
  (let* ((begform (intern-soft (concat "py-backward-" form "-bol")))
         (endform (intern-soft (concat "py-forward-" form "-bol")))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-bol-p")))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when mark-decorators
      (save-excursion
        (when (setq erg (py-backward-decorator))
          (setq beg erg))))
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (cons beg end)))

(defun py--mark-base (form &optional mark-decorators)
  "Returns boundaries of FORM, a cons.

If PY-MARK-DECORATORS, ‘def’- and ‘class’-forms include decorators
If BOL is t, mark from beginning-of-line"
  (let* ((begform (intern-soft (concat "py-backward-" form)))
         (endform (intern-soft (concat "py-forward-" form)))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (and mark-decorators
         (and (setq erg (py-backward-decorator))
              (setq beg erg)))
    (push-mark)
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (if (and beg end (<= beg orig) (<= orig end))
        (progn
	  (cons beg end)
	  (exchange-point-and-mark))
      nil)))

(defun py--mark-base-bol (form &optional mark-decorators)
  (let* ((begform (intern-soft (concat "py-backward-" form "-bol")))
         (endform (intern-soft (concat "py-forward-" form "-bol")))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-bol-p")))
         beg end erg)
    (if (functionp begcheckform)
	(or (setq beg (funcall begcheckform))
	    (if (functionp begform)
		(setq beg (funcall begform))
	      (error (concat "py--mark-base-bol: " begform " don't exist!" ))))
      (error (concat "py--mark-base-bol: " begcheckform " don't exist!" )))
    (when mark-decorators
      (save-excursion
        (when (setq erg (py-backward-decorator))
          (setq beg erg))))
    (if (functionp endform)
	(setq end (funcall endform))
      (error (concat "py--mark-base-bol: " endform " don't exist!" )))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (cons beg end)))

(defun py-mark-base (form &optional mark-decorators)
  "Calls py--mark-base, returns bounds of form, a cons. "
  (let* ((bounds (py--mark-base form mark-decorators))
         (beg (car bounds)))
    (push-mark beg t t)
    bounds))

(defun py-backward-same-level-intern (indent)
  (while (and
          (py-backward-statement)
          (< indent (current-indentation) ))))

(defun py-backward-same-level ()
  "Go form backward keeping indent level if possible.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block,
go to previous beginning of at point.
If no further element at same level, go one level up."
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (erg (cond ((nth 8 pps) (goto-char (nth 8 pps)))
                    ((nth 1 pps) (goto-char (nth 1 pps)))
                    (t (if (eq (current-column) (current-indentation))
                           (py-backward-same-level-intern (current-indentation))
                         (back-to-indentation)
                         (py-backward-same-level))))))
    erg))

;; (defun py-forward-same-level ()
;;   "Go form forward keeping indent level if possible.

;; If inside a delimited form --string or list-- go to its beginning.
;; If not at beginning of a statement or block, go to its beginning.
;; If at beginning of a statement or block, go to previous beginning.
;; If no further element at same level, go one level up."
;;   (interactive)
;;   (let (erg)
;;     (unless (py-beginning-of-statement-p)
;;       (py-backward-statement))
;;     (setq erg (py-down (current-indentation)))
;;     erg))

(defun py--end-of-buffer-p ()
  "Returns position, if cursor is at the end of buffer, nil otherwise. "
  (when (eobp)(point)))

(defun py-sectionize-region (&optional beg end)
  "Markup code in region as section.

Use current region unless optional args BEG END are delivered."
  (interactive "*")
  (let ((beg (or beg (region-beginning)))
        (end (or (and end (copy-marker end)) (copy-marker (region-end)))))
    (save-excursion
      (goto-char beg)
      (unless (py-empty-line-p) (split-line))
      (beginning-of-line)
      (insert py-section-start)
      (goto-char end)
      (unless (py-empty-line-p) (newline 1))
      (insert py-section-end))))

(defun py-execute-section-prepare (&optional shell)
  "Execute section at point. "
  (save-excursion
    (let ((start (when (or (py--beginning-of-section-p)
                           (py-backward-section))
                   (forward-line 1)
                   (beginning-of-line)
                   (point))))
      (if (and start (py-forward-section))
          (progn
            (beginning-of-line)
            (skip-chars-backward " \t\r\n\f")
            (if shell
                (funcall (car (read-from-string (concat "py-execute-region-" shell))) start (point))
              (py-execute-region start (point))))
        (error "Can't see ‘py-section-start’ resp. ‘py-section-end’")))))

(defun py--narrow-prepare (name)
  "Used internally. "
  (save-excursion
    (let ((start (cond ((string= name "statement")
                        (if (py--beginning-of-statement-p)
                            (point)
                          (py-backward-statement-bol)))
                       ((funcall (car (read-from-string (concat "py--statement-opens-" name "-p")))))
                       (t (funcall (car (read-from-string (concat "py-backward-" name "-bol"))))))))
      (funcall (car (read-from-string (concat "py-forward-" name))))
      (narrow-to-region (point) start))))

(defun py--forms-report-result (erg &optional iact)
  (let ((res (ignore-errors (buffer-substring-no-properties (car-safe erg) (cdr-safe erg)))))
    (when (and res iact)
      (goto-char (car-safe erg))
      (set-mark (point))
      (goto-char (cdr-safe erg)))
    res))

(defun py-toggle-shell-fontification (msg)
  "Toggles value of ‘py-shell-fontify-p’. "
  (interactive "p")

  (if (setq py-shell-fontify-p (not py-shell-fontify-p))
      (progn
	(py-shell-font-lock-turn-on))
    (py-shell-font-lock-turn-off))
    (when msg (message "py-shell-fontify-p set to: %s" py-shell-fontify-p)))

(defun py-toggle-execute-use-temp-file ()
  (interactive)
  (setq py--execute-use-temp-file-p (not py--execute-use-temp-file-p)))

(defun py--close-intern (regexp)
  "Core function, internal used only. "
  (let ((cui (car (py--go-to-keyword regexp))))
    (message "%s" cui)
    (py--end-base regexp (point))
    (forward-line 1)
    (if py-close-provides-newline
        (unless (py-empty-line-p) (split-line))
      (fixup-whitespace))
    (indent-to-column cui)
    cui))

(defun py--backward-regexp-fast (regexp)
  "Search backward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (let (last)
      (while (and
              (re-search-backward regexp nil 'move 1)
              (setq last (match-beginning 0))
              (nth 8 (parse-partial-sexp (point-min) (point)))))
      (unless (nth 8 (parse-partial-sexp (point-min) (point)))
        last))))

(defun py-indent-and-forward (&optional indent)
  "Indent current line according to mode, move one line forward.

If optional INDENT is given, use it"
  (interactive "*")
  (beginning-of-line)
  (when (member (char-after) (list 32 9 10 12 13)) (delete-region (point) (progn (skip-chars-forward " \t\r\n\f")(point))))
  (indent-to (or indent (py-compute-indentation)))
  (if (eobp)
      (newline-and-indent)
    (forward-line 1))
  (back-to-indentation))

(defun py--indent-line-by-line (beg end)
  "Indent every line until end to max reasonable extend.

Starts from second line of region specified
BEG END deliver the boundaries of region to work within"
  (goto-char beg)
  (py-indent-and-forward)
  ;; (forward-line 1)
  (while (< (line-end-position) end)
    (if (py-empty-line-p)
	(forward-line 1)
      (py-indent-and-forward)))
  (unless (py-empty-line-p) (py-indent-and-forward)))

(defun py-indent-region (&optional beg end no-check)
  "Reindent a region delimited by BEG END.

In case first line accepts an indent, keep the remaining
lines relative.
Otherwise lines in region get outmost indent,
same with optional argument

In order to shift a chunk of code, start with second line.

Optional BEG: used by tests
Optional END: used by tests
Optional NO-CHECK: used by tests
"
  (interactive "*")
  (or no-check (use-region-p) (error "Don't see an active region"))
  (let ((end (copy-marker (or end (region-end)))))
    (goto-char (or beg (region-beginning)))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t\r\n\f")
    (py--indent-line-by-line beg end)))

(defun py-find-imports ()
  "Find top-level imports.

Returns imports"
  (interactive)
  (let (imports erg)
    (save-excursion
      (if (eq major-mode 'comint-mode)
	  (progn
	    (re-search-backward comint-prompt-regexp nil t 1)
	    (goto-char (match-end 0))
	    (while (re-search-forward
		    "import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
	      (setq imports
		    (concat
		     imports
		     (replace-regexp-in-string
		      "[\\]\r?\n?\s*" ""
		      (buffer-substring-no-properties (match-beginning 0) (point))) ";")))
	    (when (ignore-errors (string-match ";" imports))
	      (setq imports (split-string imports ";" t))
	      (dolist (ele imports)
		(and (string-match "import" ele)
		     (if erg
			 (setq erg (concat erg ";" ele))
		       (setq erg ele)))
		(setq imports erg))))
	(goto-char (point-min))
	(while (re-search-forward
		"^import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
	  (unless (py--end-of-statement-p)
	    (py-forward-statement))
	  (setq imports
		(concat
		 imports
		 (replace-regexp-in-string
		  "[\\]\r*\n*\s*" ""
		  (buffer-substring-no-properties (match-beginning 0) (point))) ";")))))
    ;; (and imports
    ;; (setq imports (replace-regexp-in-string ";$" "" imports)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" imports))
    imports))

(defun py-kill-buffer-unconditional (&optional buffer)
  "Kill buffer unconditional, kill buffer-process if existing."
  (interactive
   (list (current-buffer)))
  (let ((buffer (or (and (bufferp buffer) buffer)
		    (get-buffer (current-buffer))))
	proc kill-buffer-query-functions)
    (if (buffer-live-p buffer)
        (progn
          (setq proc (get-buffer-process buffer))
          (and proc (kill-process proc))
          (set-buffer buffer)
          (set-buffer-modified-p 'nil)
          (kill-buffer (current-buffer)))
      (message "Can't see a buffer %s" buffer))))

;; python-components-copy-forms


(defun py-copy-block ()
  "Copy block at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "block")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-block-or-clause ()
  "Copy block-or-clause at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "block-or-clause")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-buffer ()
  "Copy buffer at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "buffer")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-class ()
  "Copy class at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "class")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-clause ()
  "Copy clause at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "clause")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-def ()
  "Copy def at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "def")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-def-or-class ()
  "Copy def-or-class at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "def-or-class")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-expression ()
  "Copy expression at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "expression")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-indent ()
  "Copy indent at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "indent")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-line ()
  "Copy line at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "line")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-minor-block ()
  "Copy minor-block at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "minor-block")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-paragraph ()
  "Copy paragraph at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "paragraph")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-partial-expression ()
  "Copy partial-expression at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "partial-expression")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-region ()
  "Copy region at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "region")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-statement ()
  "Copy statement at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "statement")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-top-level ()
  "Copy top-level at point.

Store data in kill ring, so it might yanked back."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "top-level")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-block-bol ()
  "Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "block")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-block-or-clause-bol ()
  "Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "block-or-clause")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-buffer-bol ()
  "Delete buffer bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "buffer")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-class-bol ()
  "Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "class")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-clause-bol ()
  "Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "clause")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-def-bol ()
  "Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "def")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-def-or-class-bol ()
  "Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "def-or-class")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-expression-bol ()
  "Delete expression bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "expression")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-indent-bol ()
  "Delete indent bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "indent")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-line-bol ()
  "Delete line bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "line")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-minor-block-bol ()
  "Delete minor-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "minor-block")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-paragraph-bol ()
  "Delete paragraph bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "paragraph")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-partial-expression-bol ()
  "Delete partial-expression bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "partial-expression")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-region-bol ()
  "Delete region bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "region")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-statement-bol ()
  "Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "statement")))
      (copy-region-as-kill (car erg) (cdr erg)))))

(defun py-copy-top-level-bol ()
  "Delete top-level bol at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (save-excursion
    (let ((erg (py--mark-base-bol "top-level")))
      (copy-region-as-kill (car erg) (cdr erg)))))

;; python-components-delete-forms


(defun py-delete-block ()
  "Delete BLOCK at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block-or-clause ()
  "Delete BLOCK-OR-CLAUSE at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "block-or-clause")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-class (&optional arg)
  "Delete CLASS at point until ‘beginning-of-line’.

Don't store data in kill ring.
With ARG \\[universal-argument] or ‘py-mark-decorators’ set to t, ‘decorators’ are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "class" py-mark-decorators)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-clause ()
  "Delete CLAUSE at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "clause")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def (&optional arg)
  "Delete DEF at point until ‘beginning-of-line’.

Don't store data in kill ring.
With ARG \\[universal-argument] or ‘py-mark-decorators’ set to t, ‘decorators’ are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "def" py-mark-decorators)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def-or-class (&optional arg)
  "Delete DEF-OR-CLASS at point until ‘beginning-of-line’.

Don't store data in kill ring.
With ARG \\[universal-argument] or ‘py-mark-decorators’ set to t, ‘decorators’ are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "def-or-class" py-mark-decorators)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-elif-block ()
  "Delete ELIF-BLOCK at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "elif-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-else-block ()
  "Delete ELSE-BLOCK at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "else-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-except-block ()
  "Delete EXCEPT-BLOCK at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "except-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-for-block ()
  "Delete FOR-BLOCK at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "for-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-if-block ()
  "Delete IF-BLOCK at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "if-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-indent ()
  "Delete INDENT at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "indent")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-minor-block ()
  "Delete MINOR-BLOCK at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "minor-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-statement ()
  "Delete STATEMENT at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "statement")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-try-block ()
  "Delete TRY-BLOCK at point until ‘beginning-of-line’.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "try-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-comment ()
  "Delete COMMENT at point.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "comment")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-line ()
  "Delete LINE at point.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "line")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-paragraph ()
  "Delete PARAGRAPH at point.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "paragraph")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-expression ()
  "Delete EXPRESSION at point.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-partial-expression ()
  "Delete PARTIAL-EXPRESSION at point.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "partial-expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-section ()
  "Delete SECTION at point.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "section")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-top-level ()
  "Delete TOP-LEVEL at point.

Don't store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "top-level")))
    (delete-region (car erg) (cdr erg))))

;; python-components-mark-forms


(defun py-mark-comment ()
  "Mark comment at point.

Return beginning and end positions of marked area, a cons."
  (interactive)
  (py--mark-base "comment")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))

(defun py-mark-expression ()
  "Mark expression at point.

Return beginning and end positions of marked area, a cons."
  (interactive)
  (py--mark-base "expression")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))

(defun py-mark-line ()
  "Mark line at point.

Return beginning and end positions of marked area, a cons."
  (interactive)
  (py--mark-base "line")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))

(defun py-mark-paragraph ()
  "Mark paragraph at point.

Return beginning and end positions of marked area, a cons."
  (interactive)
  (py--mark-base "paragraph")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))

(defun py-mark-partial-expression ()
  "Mark partial-expression at point.

Return beginning and end positions of marked area, a cons."
  (interactive)
  (py--mark-base "partial-expression")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))

(defun py-mark-section ()
  "Mark section at point.

Return beginning and end positions of marked area, a cons."
  (interactive)
  (py--mark-base "section")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))

(defun py-mark-top-level ()
  "Mark top-level at point.

Return beginning and end positions of marked area, a cons."
  (interactive)
  (py--mark-base "top-level")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))

(defun py-mark-assignment ()
  "Mark assignment, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "assignment")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-block ()
  "Mark block, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "block")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-block-or-clause ()
  "Mark block-or-clause, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "block-or-clause")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-class (&optional arg)
  "Mark class, take beginning of line positions. 

With ARG \\[universal-argument] or ‘py-mark-decorators’ set to t, decorators are marked too.
Return beginning and end positions of region, a cons."
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators)))
    (py--mark-base-bol "class" py-mark-decorators))
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-clause ()
  "Mark clause, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "clause")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-def (&optional arg)
  "Mark def, take beginning of line positions. 

With ARG \\[universal-argument] or ‘py-mark-decorators’ set to t, decorators are marked too.
Return beginning and end positions of region, a cons."
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators)))
    (py--mark-base-bol "def" py-mark-decorators))
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-def-or-class (&optional arg)
  "Mark def-or-class, take beginning of line positions. 

With ARG \\[universal-argument] or ‘py-mark-decorators’ set to t, decorators are marked too.
Return beginning and end positions of region, a cons."
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators)))
    (py--mark-base-bol "def-or-class" py-mark-decorators))
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-elif-block ()
  "Mark elif-block, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "elif-block")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-else-block ()
  "Mark else-block, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "else-block")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-except-block ()
  "Mark except-block, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "except-block")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-for-block ()
  "Mark for-block, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "for-block")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-if-block ()
  "Mark if-block, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "if-block")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-indent ()
  "Mark indent, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "indent")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-minor-block ()
  "Mark minor-block, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "minor-block")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-statement ()
  "Mark statement, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "statement")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
(defun py-mark-try-block ()
  "Mark try-block, take beginning of line positions. 

Return beginning and end positions of region, a cons."
  (interactive)
  (py--mark-base-bol "try-block")
  (exchange-point-and-mark)
  (cons (region-beginning) (region-end)))
;; python-components-close-forms


(defun py-close-block ()
  "Close block at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and ‘py-close-block-provides-newline’ non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-block-re))

(defun py-close-class ()
  "Close class at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and ‘py-close-block-provides-newline’ non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-class-re))

(defun py-close-clause ()
  "Close clause at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and ‘py-close-block-provides-newline’ non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-clause-re))

(defun py-close-block-or-clause ()
  "Close block-or-clause at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and ‘py-close-block-provides-newline’ non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-block-or-clause-re))

(defun py-close-def ()
  "Close def at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and ‘py-close-block-provides-newline’ non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-def-re))

(defun py-close-def-or-class ()
  "Close def-or-class at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and ‘py-close-block-provides-newline’ non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-def-or-class-re))

(defun py-close-minor-block ()
  "Close minor-block at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and ‘py-close-block-provides-newline’ non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-minor-block-re))

(defun py-close-statement ()
  "Close statement at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and ‘py-close-block-provides-newline’ non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-statement-re))

;; python-components-kill-forms


(defun py-kill-comment ()
  "Delete comment at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "comment")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-line ()
  "Delete line at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "line")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-paragraph ()
  "Delete paragraph at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "paragraph")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-expression ()
  "Delete expression at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-partial-expression ()
  "Delete partial-expression at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "partial-expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-section ()
  "Delete section at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "section")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-top-level ()
  "Delete top-level at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "top-level")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block ()
  "Delete block at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block-or-clause ()
  "Delete block-or-clause at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "block-or-clause")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-class ()
  "Delete class at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-clause ()
  "Delete clause at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "clause")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-def ()
  "Delete def at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "def")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-def-or-class ()
  "Delete def-or-class at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "def-or-class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-elif-block ()
  "Delete elif-block at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "elif-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-else-block ()
  "Delete else-block at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "else-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-except-block ()
  "Delete except-block at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "except-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-for-block ()
  "Delete for-block at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "for-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-if-block ()
  "Delete if-block at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "if-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-indent ()
  "Delete indent at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "indent")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-minor-block ()
  "Delete minor-block at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "minor-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-statement ()
  "Delete statement at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "statement")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-try-block ()
  "Delete try-block at point.

Stores data in kill ring. Might be yanked back using `C-y'."
  (interactive "*")
  (let ((erg (py--mark-base-bol "try-block")))
    (kill-region (car erg) (cdr erg))))

;; python-components-forms-code

(defun py-block (&optional decorators)
  "When called interactively, mark Block at point.

From a programm, return source of Block at point, a string.

Optional arg DECORATORS: include decorators when called at def or class.
Also honors setting of ‘py-mark-decorators’"
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "block" (or decorators py-mark-decorators))
    (py--thing-at-point "block" (or decorators py-mark-decorators))))

(defun py-block-or-clause (&optional decorators)
  "When called interactively, mark Block-Or-Clause at point.

From a programm, return source of Block-Or-Clause at point, a string.

Optional arg DECORATORS: include decorators when called at def or class.
Also honors setting of ‘py-mark-decorators’"
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "block-or-clause" (or decorators py-mark-decorators))
    (py--thing-at-point "block-or-clause" (or decorators py-mark-decorators))))

(defun py-buffer ()
  "When called interactively, mark Buffer at point.

From a programm, return source of Buffer at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "buffer")
    (py--thing-at-point "buffer")))

(defun py-class (&optional decorators)
  "When called interactively, mark Class at point.

From a programm, return source of Class at point, a string.

Optional arg DECORATORS: include decorators when called at def or class.
Also honors setting of ‘py-mark-decorators’"
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "class" (or decorators py-mark-decorators))
    (py--thing-at-point "class" (or decorators py-mark-decorators))))

(defun py-clause ()
  "When called interactively, mark Clause at point.

From a programm, return source of Clause at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "clause")
    (py--thing-at-point "clause")))

(defun py-def (&optional decorators)
  "When called interactively, mark Def at point.

From a programm, return source of Def at point, a string.

Optional arg DECORATORS: include decorators when called at def or class.
Also honors setting of ‘py-mark-decorators’"
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "def" (or decorators py-mark-decorators))
    (py--thing-at-point "def" (or decorators py-mark-decorators))))

(defun py-def-or-class (&optional decorators)
  "When called interactively, mark Def-Or-Class at point.

From a programm, return source of Def-Or-Class at point, a string.

Optional arg DECORATORS: include decorators when called at def or class.
Also honors setting of ‘py-mark-decorators’"
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "def-or-class" (or decorators py-mark-decorators))
    (py--thing-at-point "def-or-class" (or decorators py-mark-decorators))))

(defun py-expression ()
  "When called interactively, mark Expression at point.

From a programm, return source of Expression at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "expression")
    (py--thing-at-point "expression")))

(defun py-indent ()
  "When called interactively, mark Indent at point.

From a programm, return source of Indent at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "indent")
    (py--thing-at-point "indent")))

(defun py-line ()
  "When called interactively, mark Line at point.

From a programm, return source of Line at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "line")
    (py--thing-at-point "line")))

(defun py-minor-block ()
  "When called interactively, mark Minor-Block at point.

From a programm, return source of Minor-Block at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "minor-block")
    (py--thing-at-point "minor-block")))

(defun py-paragraph ()
  "When called interactively, mark Paragraph at point.

From a programm, return source of Paragraph at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "paragraph")
    (py--thing-at-point "paragraph")))

(defun py-partial-expression ()
  "When called interactively, mark Partial-Expression at point.

From a programm, return source of Partial-Expression at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "partial-expression")
    (py--thing-at-point "partial-expression")))

(defun py-region ()
  "When called interactively, mark Region at point.

From a programm, return source of Region at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "region")
    (py--thing-at-point "region")))

(defun py-statement ()
  "When called interactively, mark Statement at point.

From a programm, return source of Statement at point, a string."
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "statement")
    (py--thing-at-point "statement")))

(defun py-top-level (&optional decorators)
  "When called interactively, mark Top-Level at point.

From a programm, return source of Top-Level at point, a string.

Optional arg DECORATORS: include decorators when called at def or class.
Also honors setting of ‘py-mark-decorators’"
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base "top-level" (or decorators py-mark-decorators))
    (py--thing-at-point "top-level" (or decorators py-mark-decorators))))

;; python-components-forms-code.el ends here
;; python-components-booleans-end-forms


(defun py--end-of-comment-p ()
  "If cursor is at the end of a comment.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-comment)
      (py-forward-comment)
      (when (eq orig (point))
        orig))))

(defun py--end-of-expression-p ()
  "If cursor is at the end of a expression.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-expression)
      (py-forward-expression)
      (when (eq orig (point))
        orig))))

(defun py--end-of-line-p ()
  "If cursor is at the end of a line.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-line)
      (py-forward-line)
      (when (eq orig (point))
        orig))))

(defun py--end-of-paragraph-p ()
  "If cursor is at the end of a paragraph.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-paragraph)
      (py-forward-paragraph)
      (when (eq orig (point))
        orig))))

(defun py--end-of-partial-expression-p ()
  "If cursor is at the end of a partial-expression.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-partial-expression)
      (py-forward-partial-expression)
      (when (eq orig (point))
        orig))))

(defun py--end-of-section-p ()
  "If cursor is at the end of a section.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-section)
      (py-forward-section)
      (when (eq orig (point))
        orig))))

(defun py--end-of-top-level-p ()
  "If cursor is at the end of a top-level.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-top-level)
      (py-forward-top-level)
      (when (eq orig (point))
        orig))))

(defun py--end-of-assignment-bol-p ()
  "If at ‘beginning-of-line’ at the end of a assignment.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-assignment-bol)
      (py-forward-assignment-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-block-bol-p ()
  "If at ‘beginning-of-line’ at the end of a block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-block-bol)
      (py-forward-block-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-block-or-clause-bol-p ()
  "If at ‘beginning-of-line’ at the end of a block-or-clause.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-block-or-clause-bol)
      (py-forward-block-or-clause-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-class-bol-p ()
  "If at ‘beginning-of-line’ at the end of a class.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-class-bol)
      (py-forward-class-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-clause-bol-p ()
  "If at ‘beginning-of-line’ at the end of a clause.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-clause-bol)
      (py-forward-clause-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-def-bol-p ()
  "If at ‘beginning-of-line’ at the end of a def.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-def-bol)
      (py-forward-def-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-def-or-class-bol-p ()
  "If at ‘beginning-of-line’ at the end of a def-or-class.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-def-or-class-bol)
      (py-forward-def-or-class-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-elif-block-bol-p ()
  "If at ‘beginning-of-line’ at the end of a elif-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-elif-block-bol)
      (py-forward-elif-block-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-else-block-bol-p ()
  "If at ‘beginning-of-line’ at the end of a else-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-else-block-bol)
      (py-forward-else-block-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-except-block-bol-p ()
  "If at ‘beginning-of-line’ at the end of a except-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-except-block-bol)
      (py-forward-except-block-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-for-block-bol-p ()
  "If at ‘beginning-of-line’ at the end of a for-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-for-block-bol)
      (py-forward-for-block-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-if-block-bol-p ()
  "If at ‘beginning-of-line’ at the end of a if-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-if-block-bol)
      (py-forward-if-block-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-indent-bol-p ()
  "If at ‘beginning-of-line’ at the end of a indent.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-indent-bol)
      (py-forward-indent-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-minor-block-bol-p ()
  "If at ‘beginning-of-line’ at the end of a minor-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-minor-block-bol)
      (py-forward-minor-block-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-try-block-bol-p ()
  "If at ‘beginning-of-line’ at the end of a try-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-try-block-bol)
      (py-forward-try-block-bol)
      (when (eq orig (point))
        orig))))

(defun py--end-of-assignment-p ()
  "If cursor is at the end of a assignment.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-assignment)
      (py-forward-assignment)
      (when (eq orig (point))
        orig))))

(defun py--end-of-block-p ()
  "If cursor is at the end of a block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-block)
      (py-forward-block)
      (when (eq orig (point))
        orig))))

(defun py--end-of-block-or-clause-p ()
  "If cursor is at the end of a block-or-clause.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-block-or-clause)
      (py-forward-block-or-clause)
      (when (eq orig (point))
        orig))))

(defun py--end-of-class-p ()
  "If cursor is at the end of a class.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-class)
      (py-forward-class)
      (when (eq orig (point))
        orig))))

(defun py--end-of-clause-p ()
  "If cursor is at the end of a clause.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-clause)
      (py-forward-clause)
      (when (eq orig (point))
        orig))))

(defun py--end-of-def-p ()
  "If cursor is at the end of a def.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-def)
      (py-forward-def)
      (when (eq orig (point))
        orig))))

(defun py--end-of-def-or-class-p ()
  "If cursor is at the end of a def-or-class.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-def-or-class)
      (py-forward-def-or-class)
      (when (eq orig (point))
        orig))))

(defun py--end-of-elif-block-p ()
  "If cursor is at the end of a elif-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-elif-block)
      (py-forward-elif-block)
      (when (eq orig (point))
        orig))))

(defun py--end-of-else-block-p ()
  "If cursor is at the end of a else-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-else-block)
      (py-forward-else-block)
      (when (eq orig (point))
        orig))))

(defun py--end-of-except-block-p ()
  "If cursor is at the end of a except-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-except-block)
      (py-forward-except-block)
      (when (eq orig (point))
        orig))))

(defun py--end-of-for-block-p ()
  "If cursor is at the end of a for-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-for-block)
      (py-forward-for-block)
      (when (eq orig (point))
        orig))))

(defun py--end-of-if-block-p ()
  "If cursor is at the end of a if-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-if-block)
      (py-forward-if-block)
      (when (eq orig (point))
        orig))))

(defun py--end-of-indent-p ()
  "If cursor is at the end of a indent.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-indent)
      (py-forward-indent)
      (when (eq orig (point))
        orig))))

(defun py--end-of-minor-block-p ()
  "If cursor is at the end of a minor-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-minor-block)
      (py-forward-minor-block)
      (when (eq orig (point))
        orig))))

(defun py--end-of-try-block-p ()
  "If cursor is at the end of a try-block.
Return position, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-try-block)
      (py-forward-try-block)
      (when (eq orig (point))
        orig))))

;; python-components-exec-forms

;; Execute forms at point

(defun py-execute-try-block ()
  "Send try-block at point to Python default interpreter."
  (interactive)
  (let ((beg (prog1
                 (or (py--beginning-of-try-block-p)
                     (save-excursion
                       (py-backward-try-block)))))
        (end (save-excursion
               (py-forward-try-block))))
    (py-execute-region beg end)))

(defun py-execute-if-block ()
  "Send if-block at point to Python default interpreter."
  (interactive)
  (let ((beg (prog1
                 (or (py--beginning-of-if-block-p)
                     (save-excursion
                       (py-backward-if-block)))))
        (end (save-excursion
               (py-forward-if-block))))
    (py-execute-region beg end)))

(defun py-execute-for-block ()
  "Send for-block at point to Python default interpreter."
  (interactive)
  (let ((beg (prog1
                 (or (py--beginning-of-for-block-p)
                     (save-excursion
                       (py-backward-for-block)))))
        (end (save-excursion
               (py-forward-for-block))))
    (py-execute-region beg end)))

;; python-components-switches

;;  Smart indentation
(defun py-toggle-smart-indentation (&optional arg)
  "Toggle ‘py-smart-indentation’ - on with positiv ARG.

Returns value of ‘py-smart-indentation’ switched to."
  (interactive)
  (let ((arg (or arg (if py-smart-indentation -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-smart-indentation t)
          (py-guess-indent-offset))
      (setq py-smart-indentation nil)
      (setq py-indent-offset (default-value 'py-indent-offset)))
    (when (called-interactively-p 'any) (message "py-smart-indentation: %s" py-smart-indentation))
    py-smart-indentation))

(defun py-smart-indentation-on (&optional arg)
  "Toggle‘py-smart-indentation’ - on with positive ARG.

Returns value of ‘py-smart-indentation’."
  (interactive "p")
  (let ((arg (or arg 1)))
    (py-toggle-smart-indentation arg))
  (when (called-interactively-p 'any) (message "py-smart-indentation: %s" py-smart-indentation))
  py-smart-indentation)

(defun py-smart-indentation-off (&optional arg)
  "Toggle ‘py-smart-indentation’ according to ARG.

Returns value of ‘py-smart-indentation’."
  (interactive "p")
  (let ((arg (if arg (- arg) -1)))
    (py-toggle-smart-indentation arg))
  (when (called-interactively-p 'any) (message "py-smart-indentation: %s" py-smart-indentation))
  py-smart-indentation)

(defun py-toggle-sexp-function ()
  "Opens customization."
  (interactive)
  (customize-variable 'py-sexp-function))

;; Autopair mode
;; py-autopair-mode forms
(declare-function autopair-mode "autopair" ())
(defun py-toggle-autopair-mode ()
  "If ‘py-autopair-mode’ should be on or off.

  Returns value of ‘py-autopair-mode’ switched to."
  (interactive)
  (and (py-autopair-check)
       (declare-function autopair-mode "autopair-mode" ())
       (setq py-autopair-mode (autopair-mode (if autopair-mode 0 1)))))

(defun py-autopair-mode-on ()
  "Make sure, py-autopair-mode' is on.

Returns value of ‘py-autopair-mode’."
  (interactive)
  (and (py-autopair-check)
       (setq py-autopair-mode (autopair-mode 1))))

(defun py-autopair-mode-off ()
  "Make sure, py-autopair-mode' is off.

Returns value of ‘py-autopair-mode’."
  (interactive)
  (setq py-autopair-mode (autopair-mode -1)))

;;  py-switch-buffers-on-execute-p forms
(defun py-toggle-switch-buffers-on-execute-p (&optional arg)
  "Toggle ‘py-switch-buffers-on-execute-p’ according to ARG.

  Returns value of ‘py-switch-buffers-on-execute-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-switch-buffers-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-switch-buffers-on-execute-p t)
      (setq py-switch-buffers-on-execute-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-switch-buffers-on-execute-p: %s" py-switch-buffers-on-execute-p))
    py-switch-buffers-on-execute-p))

(defun py-switch-buffers-on-execute-p-on (&optional arg)
  "Toggle ‘py-switch-buffers-on-execute-p’ according to ARG.

Returns value of ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-switch-buffers-on-execute-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-switch-buffers-on-execute-p: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-switch-buffers-on-execute-p-off ()
  "Make sure, ‘py-switch-buffers-on-execute-p’ is off.

Returns value of ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (py-toggle-switch-buffers-on-execute-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-switch-buffers-on-execute-p: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

;;  py-split-window-on-execute forms
(defun py-toggle-split-window-on-execute (&optional arg)
  "Toggle ‘py-split-window-on-execute’ according to ARG.

  Returns value of ‘py-split-window-on-execute’ switched to."
  (interactive)
  (let ((arg (or arg (if py-split-window-on-execute -1 1))))
    (if (< 0 arg)
        (setq py-split-window-on-execute t)
      (setq py-split-window-on-execute nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
    py-split-window-on-execute))

(defun py-split-window-on-execute-on (&optional arg)
  "Toggle ‘py-split-window-on-execute’ according to ARG.

Returns value of ‘py-split-window-on-execute’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-split-window-on-execute arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

(defun py-split-window-on-execute-off ()
  "Make sure, ‘py-split-window-on-execute’ is off.

Returns value of ‘py-split-window-on-execute’."
  (interactive)
  (py-toggle-split-window-on-execute -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

;;  py-fontify-shell-buffer-p forms
(defun py-toggle-fontify-shell-buffer-p (&optional arg)
  "Toggle ‘py-fontify-shell-buffer-p’ according to ARG.

  Returns value of ‘py-fontify-shell-buffer-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-fontify-shell-buffer-p -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-fontify-shell-buffer-p t)
          (set (make-local-variable 'font-lock-defaults)
             '(python-font-lock-keywords nil nil nil nil
                                         (font-lock-syntactic-keywords
                                          . py-font-lock-syntactic-keywords)))
          (unless (looking-at comint-prompt-regexp)
            (when (re-search-backward comint-prompt-regexp nil t 1)
              (font-lock-fontify-region (line-beginning-position) (point-max)))))
      (setq py-fontify-shell-buffer-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-fontify-shell-buffer-p: %s" py-fontify-shell-buffer-p))
    py-fontify-shell-buffer-p))

(defun py-fontify-shell-buffer-p-on (&optional arg)
  "Toggle ‘py-fontify-shell-buffer-p’ according to ARG.

Returns value of ‘py-fontify-shell-buffer-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-fontify-shell-buffer-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-fontify-shell-buffer-p: %s" py-fontify-shell-buffer-p))
  py-fontify-shell-buffer-p)

(defun py-fontify-shell-buffer-p-off ()
  "Make sure, ‘py-fontify-shell-buffer-p’ is off.

Returns value of ‘py-fontify-shell-buffer-p’."
  (interactive)
  (py-toggle-fontify-shell-buffer-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-fontify-shell-buffer-p: %s" py-fontify-shell-buffer-p))
  py-fontify-shell-buffer-p)

;;  python-mode-v5-behavior-p forms
(defun py-toggle-python-mode-v5-behavior-p (&optional arg)
  "Toggle `python-mode-v5-behavior-p' according to ARG.

  Returns value of `python-mode-v5-behavior-p' switched to."
  (interactive)
  (let ((arg (or arg (if python-mode-v5-behavior-p -1 1))))
    (if (< 0 arg)
        (setq python-mode-v5-behavior-p t)
      (setq python-mode-v5-behavior-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p))
    python-mode-v5-behavior-p))

(defun py-python-mode-v5-behavior-p-on (&optional arg)
  "To `python-mode-v5-behavior-p' according to ARG.

Returns value of `python-mode-v5-behavior-p'."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-python-mode-v5-behavior-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p))
  python-mode-v5-behavior-p)

(defun py-python-mode-v5-behavior-p-off ()
  "Make sure, `python-mode-v5-behavior-p' is off.

Returns value of `python-mode-v5-behavior-p'."
  (interactive)
  (py-toggle-python-mode-v5-behavior-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p))
  python-mode-v5-behavior-p)

;;  py-jump-on-exception forms
(defun py-toggle-jump-on-exception (&optional arg)
  "Toggle ‘py-jump-on-exception’ according to ARG.

  Returns value of ‘py-jump-on-exception’ switched to."
  (interactive)
  (let ((arg (or arg (if py-jump-on-exception -1 1))))
    (if (< 0 arg)
        (setq py-jump-on-exception t)
      (setq py-jump-on-exception nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-jump-on-exception: %s" py-jump-on-exception))
    py-jump-on-exception))

(defun py-jump-on-exception-on (&optional arg)
  "Toggle py-jump-on-exception' according to ARG.

Returns value of ‘py-jump-on-exception’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-jump-on-exception arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-jump-on-exception: %s" py-jump-on-exception))
  py-jump-on-exception)

(defun py-jump-on-exception-off ()
  "Make sure, ‘py-jump-on-exception’ is off.

Returns value of ‘py-jump-on-exception’."
  (interactive)
  (py-toggle-jump-on-exception -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-jump-on-exception: %s" py-jump-on-exception))
  py-jump-on-exception)

;;  py-use-current-dir-when-execute-p forms
(defun py-toggle-use-current-dir-when-execute-p (&optional arg)
  "Toggle ‘py-use-current-dir-when-execute-p’ according to ARG.

  Returns value of ‘py-use-current-dir-when-execute-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-use-current-dir-when-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-use-current-dir-when-execute-p t)
      (setq py-use-current-dir-when-execute-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-use-current-dir-when-execute-p: %s" py-use-current-dir-when-execute-p))
    py-use-current-dir-when-execute-p))

(defun py-use-current-dir-when-execute-p-on (&optional arg)
  "Toggle py-use-current-dir-when-execute-p' according to ARG.

Returns value of ‘py-use-current-dir-when-execute-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-use-current-dir-when-execute-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-use-current-dir-when-execute-p: %s" py-use-current-dir-when-execute-p))
  py-use-current-dir-when-execute-p)

(defun py-use-current-dir-when-execute-p-off ()
  "Make sure, ‘py-use-current-dir-when-execute-p’ is off.

Returns value of ‘py-use-current-dir-when-execute-p’."
  (interactive)
  (py-toggle-use-current-dir-when-execute-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-use-current-dir-when-execute-p: %s" py-use-current-dir-when-execute-p))
  py-use-current-dir-when-execute-p)

;;  py-electric-comment-p forms
(defun py-toggle-electric-comment-p (&optional arg)
  "Toggle ‘py-electric-comment-p’ according to ARG.

  Returns value of ‘py-electric-comment-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-electric-comment-p -1 1))))
    (if (< 0 arg)
        (setq py-electric-comment-p t)
      (setq py-electric-comment-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-electric-comment-p: %s" py-electric-comment-p))
    py-electric-comment-p))

(defun py-electric-comment-p-on (&optional arg)
  "Toggle py-electric-comment-p' according to ARG.

Returns value of ‘py-electric-comment-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-electric-comment-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-electric-comment-p: %s" py-electric-comment-p))
  py-electric-comment-p)

(defun py-electric-comment-p-off ()
  "Make sure, ‘py-electric-comment-p’ is off.

Returns value of ‘py-electric-comment-p’."
  (interactive)
  (py-toggle-electric-comment-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-electric-comment-p: %s" py-electric-comment-p))
  py-electric-comment-p)

;;  py-underscore-word-syntax-p forms
(defun py-toggle-underscore-word-syntax-p (&optional arg)
  "Toggle ‘py-underscore-word-syntax-p’ according to ARG.

  Returns value of ‘py-underscore-word-syntax-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-underscore-word-syntax-p -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-underscore-word-syntax-p t)
          (modify-syntax-entry ?\_ "w" python-mode-syntax-table))
      (setq py-underscore-word-syntax-p nil)
      (modify-syntax-entry ?\_ "_" python-mode-syntax-table))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
    py-underscore-word-syntax-p))

(defun py-underscore-word-syntax-p-on (&optional arg)
  "Toggle py-underscore-word-syntax-p' according to ARG.

Returns value of ‘py-underscore-word-syntax-p’."
  (interactive)
  (let ((arg (or arg 1)))
    (py-toggle-underscore-word-syntax-p arg))
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
  py-underscore-word-syntax-p)

(defun py-underscore-word-syntax-p-off ()
  "Make sure, ‘py-underscore-word-syntax-p’ is off.

Returns value of ‘py-underscore-word-syntax-p’."
  (interactive)
  (py-toggle-underscore-word-syntax-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
  py-underscore-word-syntax-p)

;; py-toggle-underscore-word-syntax-p must be known already
;; circular: py-toggle-underscore-word-syntax-p sets and calls it
(defcustom py-underscore-word-syntax-p t
  "If underscore chars should be of ‘syntax-class’ word.

I.e. not of ‘symbol’.

Underscores in word-class like ‘forward-word’ travel the indentifiers.
Default is t.

See bug report at launchpad, lp:940812"
  :type 'boolean
  :tag "py-underscore-word-syntax-p"
  :group 'python-mode
  :set (lambda (symbol value)
         (set-default symbol value)
         (py-toggle-underscore-word-syntax-p (if value 1 0))))

;; python-components-edit

(defun py-insert-default-shebang ()
  "Insert in buffer shebang of installed default Python."
  (interactive "*")
  (let* ((erg (if py-edit-only-p
                  py-shell-name
                (executable-find py-shell-name)))
         (sheb (concat "#! " erg)))
    (insert sheb)))

(defun py--top-level-form-p ()
  "Return non-nil, if line start with a top level form."
  (save-excursion
    (beginning-of-line)
    (unless
	;; in string
	(nth 3 (parse-partial-sexp (point-min) (point)))
      (and (eq (current-indentation)  0)
	   (looking-at "[[:alpha:]_]+")
	   ;; (or (looking-at py-def-or-class-re)
           ;;     (looking-at py-block-or-clause-re)
	   ;;     (looking-at py-assignment-re))
	   ))))

(defun py-indent-line-outmost (&optional arg)
  "Indent the current line to the outmost reasonable indent.

With optional \\[universal-argument] ARG, unconditionally insert an indent of
‘py-indent-offset’ length."
  (interactive "*P")
  (cond
   ((eq 4 (prefix-numeric-value arg))
    (if indent-tabs-mode
        (insert (make-string 1 9))
      (insert (make-string py-indent-offset 32))))
   ;;
   (t
    (let* ((need (py-compute-indentation (point)))
           (cui (current-indentation))
           (cuc (current-column)))
      (if (and (eq need cui)
               (not (eq cuc cui)))
          (back-to-indentation)
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to need))))))

(defun py--re-indent-line ()
  "Re-indent the current line."
  (beginning-of-line)
  (delete-region (point)
                 (progn (skip-chars-forward " \t\r\n\f")
                        (point)))
  (indent-to (py-compute-indentation)))

;; TODO: the following function can fall into an infinite loop.
;; See https://gitlab.com/python-mode-devs/python-mode/-/issues/99
(defun py--indent-fix-region-intern (beg end)
  "Used when ‘py-tab-indents-region-p’ is non-nil.

Requires BEG, END as the boundery of region"
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (narrow-to-region beg end)
      (goto-char beg)
      (let ((end (copy-marker end)))
	(forward-line 1)
	(narrow-to-region (line-beginning-position) end)
	(py--re-indent-line)
	(while (< (line-end-position) end)
          (forward-line 1)
          (py--re-indent-line))))))

(defun py-indent-current-line (need)
  "Indent current line to NEED."
  (beginning-of-line)
  (delete-horizontal-space)
  (indent-to need))

;; TODO: Add docstring.
;; What is the intent of the this utility function?
;; What is the purpose of each argument?
(defun py--indent-line-intern (need cui indent col &optional beg end region dedent)
  (let (erg)
    (if py-tab-indent
	(progn
	  (and py-tab-indents-region-p region
	       (py--indent-fix-region-intern beg end))
	  (cond
	   ((bolp)
	    (if (and py-tab-shifts-region-p region)
                (while (< (current-indentation) need)
                  (py-shift-region-right 1))
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (indent-to need)))
           ;;
	   ((< need cui)
	    (if (and py-tab-shifts-region-p region)
		(progn
		  (when (eq (point) (region-end))
		    (exchange-point-and-mark))
		  (while (< 0 (current-indentation))
		    (py-shift-region-left 1)))
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (indent-to need)))
           ;;
	   ((eq need cui)
	    (if (or dedent
		    (eq this-command last-command)
		    (eq this-command 'py-indent-line))
		(if (and py-tab-shifts-region-p region)
		    (while (and (goto-char beg) (< 0 (current-indentation)))
		      (py-shift-region-left 1))
		  (beginning-of-line)
		  (delete-horizontal-space)
		  (if (<= (line-beginning-position) (+ (point) (- col cui)))
		      (forward-char (- col cui))
		    (beginning-of-line)))))
           ;;
	   ((< cui need)
	    (if (and py-tab-shifts-region-p region)
                (py-shift-region-right 1)
              (beginning-of-line)
              (delete-horizontal-space)
              ;; indent one indent only if goal < need
              (setq erg (+ (* (/ cui indent) indent) indent))
              (if (< need erg)
                  (indent-to need)
                (indent-to erg))
              (forward-char (- col cui))))
           ;;
	   (t
	    (if (and py-tab-shifts-region-p region)
                (while (< (current-indentation) need)
                  (py-shift-region-right 1))
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (indent-to need)
	      (back-to-indentation)
	      (if (<= (line-beginning-position) (+ (point) (- col cui)))
		  (forward-char (- col cui))
		(beginning-of-line))))))
      (insert-tab))))

(defun py--indent-line-or-region-base (beg end region cui need arg this-indent-offset col &optional dedent)
  (cond ((eq 4 (prefix-numeric-value arg))
	 (if (and (eq cui (current-indentation))
		  (<= need cui))
	     (if indent-tabs-mode (insert "\t")(insert (make-string py-indent-offset 32)))
	   (beginning-of-line)
	   (delete-horizontal-space)
	   (indent-to (+ need py-indent-offset))))
	((not (eq 1 (prefix-numeric-value arg)))
	 (py-smart-indentation-off)
	 (py--indent-line-intern need cui this-indent-offset col beg end region dedent))
	(t (py--indent-line-intern need cui this-indent-offset col beg end region dedent))))

(defun py--calculate-indent-backwards (cui indent-offset)
  "Return the next reasonable indent lower than current indentation.

Requires current indent as CUI
Requires current indent-offset as INDENT-OFFSET"
  (if (< 0 (% cui py-indent-offset))
      ;; not correctly indented at all
      (/ cui indent-offset)
    (- cui indent-offset)))

(defun py-indent-line (&optional arg dedent)
  "Indent the current line according ARG.

When called interactivly with \\[universal-argument],
ignore dedenting rules for block closing statements
\(e.g. return, raise, break, continue, pass)

An optional \\[universal-argument] followed by a numeric argument
neither 1 nor 4 will switch off ‘py-smart-indentation’ for this execution.
This permits to correct allowed but unwanted indents. Similar to
‘py-toggle-smart-indentation’ resp. ‘py-smart-indentation-off’ followed by TAB.

OUTMOST-ONLY stops circling possible indent.

When ‘py-tab-shifts-region-p’ is t, not just the current line,
but the region is shiftet that way.

If ‘py-tab-indents-region-p’ is t and first TAB doesn't shift
--as indent is at outmost reasonable--, ‘indent-region’ is called.

Optional arg DEDENT: force dedent.

\\[quoted-insert] TAB inserts a literal TAB-character."
  (interactive "P")
  (unless (eq this-command last-command)
    (setq py-already-guessed-indent-offset nil))
  (let ((orig (copy-marker (point)))
	;; TAB-leaves-point-in-the-wrong-lp-1178453-test
	(region (use-region-p))
        cui
	outmost
	col
	beg
	end
	need
	this-indent-offset)
    (and region
	 (setq beg (region-beginning))
	 (setq end (region-end))
	 (goto-char beg))
    (setq cui (current-indentation))
    (setq col (current-column))
    (setq this-indent-offset
	  (cond ((and py-smart-indentation (not (eq this-command last-command)))
		 (py-guess-indent-offset))
		((and py-smart-indentation (eq this-command last-command) py-already-guessed-indent-offset)
		 py-already-guessed-indent-offset)
		(t (default-value 'py-indent-offset))))
    (setq outmost (py-compute-indentation nil nil nil nil nil nil nil this-indent-offset))
    ;; now choose the indent
    (unless (and (not dedent)(not (eq this-command last-command))(eq outmost (current-indentation)))
      (setq need
	    (cond ((eq this-command last-command)
		     (if (bolp)
			 ;; jump forward to max indent
			 outmost
		       (py--calculate-indent-backwards cui this-indent-offset)))
		  ;; (py--calculate-indent-backwards cui this-indent-offset)))))
		  (t
		   outmost
		   )))
      (py--indent-line-or-region-base beg end region cui need arg this-indent-offset col dedent)
      (and region (or py-tab-shifts-region-p
		      py-tab-indents-region-p)
	   (not (eq (point) orig))
	   (exchange-point-and-mark))
      (current-indentation))))

(defun py--delete-trailing-whitespace (orig)
  "Delete trailing whitespace.

Either ‘py-newline-delete-trailing-whitespace-p’
or `
py-trailing-whitespace-smart-delete-p' must be t.

Start from position ORIG"
  (when (or py-newline-delete-trailing-whitespace-p py-trailing-whitespace-smart-delete-p)
    (let ((pos (copy-marker (point))))
      (save-excursion
	(goto-char orig)
	(if (py-empty-line-p)
	    (if (py---emacs-version-greater-23)
		(delete-trailing-whitespace (line-beginning-position) pos)
	      (save-restriction
		(narrow-to-region (line-beginning-position) pos)
		(delete-trailing-whitespace)))
	  (skip-chars-backward " \t")
	  (if (py---emacs-version-greater-23)
	      (delete-trailing-whitespace (line-beginning-position) pos)
	    (save-restriction
	      (narrow-to-region (point) pos)
	      (delete-trailing-whitespace))))))))

(defun py-newline-and-indent ()
  "Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines."
  (interactive "*")
  (let* ((orig (point))
	 ;; lp:1280982, deliberatly dedented by user
	 (this-dedent
	  (when
	      ;; (and (or (eq 10 (char-after))(eobp))(looking-back "^[ \t]*" (line-beginning-position)))
	      (looking-back "^[ \t]+" (line-beginning-position))
	    (current-column)))
	 erg)
    (newline 1)
    (py--delete-trailing-whitespace orig)
    (setq erg
	  (cond (this-dedent
		 (indent-to-column this-dedent))
		((and py-empty-line-closes-p (or (eq this-command last-command)(py--after-empty-line)))
		 (indent-to-column (save-excursion (py-backward-statement)(- (current-indentation) py-indent-offset))))
		(t
		 (fixup-whitespace)
		 (indent-to-column (py-compute-indentation)))))
    erg))

(defun py-newline-and-dedent ()
  "Add a newline and indent to one level below current.
Returns column."
  (interactive "*")
  (let ((cui (current-indentation)))
    (newline 1)
    (when (< 0 cui)
      (indent-to (- (py-compute-indentation) py-indent-offset)))))

(defun py-toggle-indent-tabs-mode ()
  "Toggle ‘indent-tabs-mode’.

Returns value of ‘indent-tabs-mode’ switched to."
  (interactive)
  (when
      (setq indent-tabs-mode (not indent-tabs-mode))
    (setq tab-width py-indent-offset))
  (when (and py-verbose-p (called-interactively-p 'any)) (message "indent-tabs-mode %s  py-indent-offset %s" indent-tabs-mode py-indent-offset))
  indent-tabs-mode)

(defun py-indent-tabs-mode (arg)
  "With positive ARG switch ‘indent-tabs-mode’ on.

With negative ARG switch ‘indent-tabs-mode’ off.
Returns value of ‘indent-tabs-mode’ switched to.

If IACT is provided, message result"
  (interactive "p")
  (if (< 0 arg)
      (progn
        (setq indent-tabs-mode t)
        (setq tab-width py-indent-offset))
    (setq indent-tabs-mode nil))
  (when (and py-verbose-p (called-interactively-p 'any)) (message "indent-tabs-mode %s   py-indent-offset %s" indent-tabs-mode py-indent-offset))
  indent-tabs-mode)

(defun py-indent-tabs-mode-on (arg)
  "Switch ‘indent-tabs-mode’ according to ARG."
  (interactive "p")
  (py-indent-tabs-mode (abs arg)))

(defun py-indent-tabs-mode-off (arg)
  "Switch ‘indent-tabs-mode’ according to ARG."
  (interactive "p")
  (py-indent-tabs-mode (- (abs arg))))

;;  Guess indent offset

(defun py--comment-indent-function ()
  "Python version of ‘comment-indent-function’."
  ;; This is required when filladapt is turned off.  Without it, when
  ;; filladapt is not used, comments which start in column zero
  ;; cascade one character to the right
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position)))
      (and comment-start-skip
           (re-search-forward comment-start-skip eol t)
           (setq eol (match-beginning 0)))
      (goto-char eol)
      (skip-chars-backward " \t")
      (max comment-column (+ (current-column) (if (bolp) 0 1))))))

;; ;

;;  Declarations start
(defun py--bounds-of-declarations ()
  "Bounds of consecutive multitude of assigments resp. statements around point.

Indented same level, which don't open blocks.
Typically declarations resp. initialisations of variables following
a class or function definition.
See also ‘py--bounds-of-statements’"
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py--beginning-of-statement-p)
                          (py-backward-statement))
                        (unless (py--beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (line-beginning-position))
      ;; look upward first
      (while (and
              (or 
                (py--beginning-of-statement-p)
                  (py-backward-statement))
              (py-backward-statement)
              (not (py--beginning-of-block-p))
              (eq (current-indentation) orig-indent))
        (setq beg (line-beginning-position)))
      (goto-char orig)
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py--beginning-of-block-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char beg)
      (if (and beg end)
          (progn
            (cons beg end))
        nil))))

(defun py-backward-declarations ()
  "Got to the beginning of assigments resp. statements.

Move in current level which don't open blocks."
  (interactive)
  (let* ((bounds (py--bounds-of-declarations))
         (erg (car bounds)))
    (when erg (goto-char erg))
    erg))

(defun py-forward-declarations ()
  "Got to the end of assigments resp. statements.

Move in current level which don't open blocks."
  (interactive)
  (let* ((bounds (py--bounds-of-declarations))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    erg))

(defun py-declarations ()
  "Forms in current level.

Forms don't open blocks or start with a keyword.

See also ‘py-statements’."
  (interactive)
  (let* ((bounds (py--bounds-of-declarations))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (exchange-point-and-mark))))

(defun py-kill-declarations ()
  "Delete variables declared in current level.

Store deleted variables in ‘kill-ring’"
  (interactive "*")
  (let* ((bounds (py--bounds-of-declarations))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))
;;  Declarations end

;;  Statements start
(defun py--bounds-of-statements ()
  "Bounds of consecutive multitude of statements around point.

Indented same level, which don't open blocks."
  (interactive)
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py--beginning-of-statement-p)
                          (py-backward-statement))
                        (unless (py--beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (point))
      (while (and (setq last beg)
                  (setq beg
                        (when (py-backward-statement)
                          (line-beginning-position)))
		  ;; backward-statement shouldn't stop in string
                  ;; (not (py-in-string-p))
                  (not (py--beginning-of-block-p))
                  (eq (current-indentation) orig-indent)))
      (setq beg last)
      (goto-char orig)
      (setq end (line-end-position))
      (while (and (setq last (py--end-of-statement-position))
                  (setq end (py-down-statement))
                  (not (py--beginning-of-block-p))
                  ;; (not (looking-at py-keywords))
                  ;; (not (looking-at "pdb\."))
                  ;; (not (py-in-string-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char orig)
      (if (and beg end)
          (progn
            (when (called-interactively-p 'any) (message "%s %s" beg end))
            (cons beg end))
        nil))))

(defun py-backward-statements ()
  "Got to the beginning of statements in current level which don't open blocks."
  (interactive)
  (let* ((bounds (py--bounds-of-statements))
         (erg (car bounds)))
    (when erg (goto-char erg))
    erg))

(defun py-forward-statements ()
  "Got to the end of statements in current level which don't open blocks."
  (interactive)
  (let* ((bounds (py--bounds-of-statements))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    erg))

(defun py-statements ()
  "Copy and mark simple statements level.

These statements don't open blocks.

More general than ‘py-declarations’."
  (interactive)
  (let* ((bounds (py--bounds-of-statements))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (exchange-point-and-mark))))

(defun py-kill-statements ()
  "Delete statements declared in current level.

Store deleted statements in ‘kill-ring’"
  (interactive "*")
  (let* ((bounds (py--bounds-of-statements))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))

(defun py-insert-super ()
  "Insert a function \"super()\" from current environment.

As example given in Python v3.1 documentation » The Python Standard Library »

class C(B):
    def method(self, arg):
        super().method(arg) # This does the same thing as:
                               # super(C, self).method(arg)

Returns the string inserted."
  (interactive "*")
  (let* ((orig (point))
         (funcname (progn
                     (py-backward-def)
                     (when (looking-at (concat py-def-re " *\\([^(]+\\) *(\\(?:[^),]*\\),? *\\([^)]*\\))"))
                       (match-string-no-properties 2))))
         (args (match-string-no-properties 3))
         (ver (py-which-python))
         classname erg)
    (if (< ver 3)
        (progn
          (py-backward-class)
          (when (looking-at (concat py-class-re " *\\([^( ]+\\)"))
            (setq classname (match-string-no-properties 2)))
          (goto-char orig)
          (setq erg (concat "super(" classname ", self)." funcname "(" args ")"))
          ;; super(C, self).method(arg)"
          (insert erg))
      (goto-char orig)
      (setq erg (concat "super()." funcname "(" args ")"))
      (insert erg))
    erg))

;; Comments
(defun py-delete-comments-in-def-or-class ()
  "Delete all commented lines in def-or-class at point."
  (interactive "*")
  (save-excursion
    (let ((beg (py--beginning-of-def-or-class-position))
          (end (py--end-of-def-or-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-class ()
  "Delete all commented lines in class at point."
  (interactive "*")
  (save-excursion
    (let ((beg (py--beginning-of-class-position))
          (end (py--end-of-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-block ()
  "Delete all commented lines in block at point."
  (interactive "*")
  (save-excursion
    (let ((beg (py--beginning-of-block-position))
          (end (py--end-of-block-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-region (beg end)
  "Delete all commented lines in region delimited by BEG END."
  (interactive "r*")
  (save-excursion
    (py--delete-comments-intern beg end)))

(defun py--delete-comments-intern (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (while (and (< (line-end-position) end) (not (eobp)))
      (beginning-of-line)
      (if (looking-at (concat "[ \t]*" comment-start))
          (delete-region (point) (1+ (line-end-position)))
        (forward-line 1)))))

;; Edit docstring
(defun py--edit-set-vars ()
  (save-excursion
    (let ((py--editbeg (when (use-region-p) (region-beginning)))
	  (py--editend (when (use-region-p) (region-end)))
	  (pps (parse-partial-sexp (point-min) (point))))
      (when (nth 3 pps)
	(setq py--editbeg (or py--editbeg (progn (goto-char (nth 8 pps))
						 (skip-chars-forward (char-to-string (char-after)))(push-mark) (point))))
	(setq py--editend (or py--editend
			      (progn (goto-char (nth 8 pps))
				     (forward-sexp)
				     (skip-chars-backward (char-to-string (char-before)))
				     (point)))))
      (cons (copy-marker py--editbeg) (copy-marker py--editend)))))

(defun py--write-edit ()
  "When edit is finished, write docstring back to orginal buffer."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "[\"']" nil t 1)
    (or (py-escaped-p)
	(replace-match (concat "\\\\" (match-string-no-properties 0)))))
  (jump-to-register py--edit-register)
  ;; (py-restore-window-configuration)
  (delete-region py--docbeg py--docend)
  (insert-buffer-substring py-edit-buffer))

(defun py-edit--intern (buffer-name mode &optional beg end prefix suffix)
  "Edit string or active region in ‘python-mode’.

arg BUFFER-NAME: a string.
arg MODE: which buffer-mode used in edit-buffer"
  (interactive "*")
  (save-excursion
    (save-restriction
      (window-configuration-to-register py--edit-register)
      (setq py--oldbuf (current-buffer))
      (let* ((orig (point))
	     (bounds (or (and beg end) (py--edit-set-vars)))
	     relpos editstrg)
	(setq py--docbeg (or beg (car bounds)))
	(setq py--docend (or end (cdr bounds)))
	;; store relative position in editstrg
	(setq relpos (1+ (- orig py--docbeg)))
	(setq editstrg (buffer-substring py--docbeg py--docend))
	(set-buffer (get-buffer-create buffer-name))
	(erase-buffer)
	(switch-to-buffer (current-buffer))
	(when prefix (insert prefix))
	(insert editstrg)
	(when suffix (insert suffix))
	(funcall mode)
	(local-set-key [(control c) (control c)] 'py--write-edit)
	(goto-char relpos)
	(message "%s" "Type C-c C-c writes contents back")))))

(defun py-edit-docstring ()
  "Edit docstring or active region in ‘python-mode’."
  (interactive "*")
  (py-edit--intern "Edit docstring" 'python-mode))

(defun py-unpretty-assignment ()
  "Revoke prettyprint, write assignment in a shortest way."
  (interactive "*")
  (save-excursion
    (let* ((beg (py-beginning-of-assignment))
	   (end (copy-marker (py-forward-assignment)))
	   last)
      (goto-char beg)
      (while (and (not (eobp))(re-search-forward "^\\([ \t]*\\)\[\]\"'{}]" end t 1) (setq last (copy-marker (point))))
	(save-excursion (goto-char (match-end 1))
			(when (eq (current-column) (current-indentation)) (delete-region (point) (progn (skip-chars-backward " \t\r\n\f") (point)))))
	(when last (goto-char last))))))

(defun py--prettyprint-assignment-intern (beg end name buffer)
  (let ((proc (get-buffer-process buffer))
	erg)
    ;; (py-send-string "import pprint" proc nil t)
    (py-fast-send-string "import json" proc buffer)
    ;; send the dict/assigment
    (py-fast-send-string (buffer-substring-no-properties beg end) proc buffer)
    ;; do pretty-print
    ;; print(json.dumps(neudict4, indent=4))
    (setq erg (py-fast-send-string (concat "print(json.dumps("name", indent=5))") proc buffer t))
    (goto-char beg)
    (skip-chars-forward "^{")
    (delete-region (point) (progn (forward-sexp) (point)))
    (insert erg)))

(defun py-prettyprint-assignment ()
  "Prettyprint assignment in ‘python-mode’."
  (interactive "*")
  (window-configuration-to-register py--windows-config-register)
  (save-excursion
    (let* ((beg (py-beginning-of-assignment))
	   (name (py-expression))
	   (end (py-forward-assignment))
	   (proc-buf (py-shell nil nil "Fast Intern Utility Re-Use")))
      (py--prettyprint-assignment-intern beg end name proc-buf)))
  (py-restore-window-configuration))

;; python-components-named-shells

(defun ipython (&optional argprompt args buffer fast exception-buffer split)
  "Start an IPython interpreter.

With optional \\[universal-argument] get a new dedicated shell."
  (interactive "p")
  (py-shell argprompt args nil "ipython" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))

;; (defun ipython2.7 (&optional argprompt args buffer fast exception-buffer split)
;;   "Start an IPython2.7 interpreter.

;; With optional \\[universal-argument] get a new dedicated shell."
;;   (interactive "p")
;;   (py-shell argprompt args nil "ipython2.7" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))

(defun ipython3 (&optional argprompt args buffer fast exception-buffer split)
  "Start an IPython3 interpreter.

With optional \\[universal-argument] get a new dedicated shell."
  (interactive "p")
  (py-shell argprompt args nil "ipython3" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))

(defun jython (&optional argprompt args buffer fast exception-buffer split)
  "Start an Jython interpreter.

With optional \\[universal-argument] get a new dedicated shell."
  (interactive "p")
  (py-shell argprompt args nil "jython" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))

(defun python (&optional argprompt args buffer fast exception-buffer split)
  "Start an Python interpreter.

With optional \\[universal-argument] get a new dedicated shell."
  (interactive "p")
  (py-shell argprompt args nil "python" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))

(defun python2 (&optional argprompt args buffer fast exception-buffer split)
  "Start an Python2 interpreter.

With optional \\[universal-argument] get a new dedicated shell."
  (interactive "p")
  (py-shell argprompt args nil "python2" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))

(defun python3 (&optional argprompt args buffer fast exception-buffer split)
  "Start an Python3 interpreter.

With optional \\[universal-argument] get a new dedicated shell."
  (interactive "p")
  (py-shell argprompt args nil "python3" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))

(defun pypy (&optional argprompt args buffer fast exception-buffer split)
  "Start an Pypy interpreter.

With optional \\[universal-argument] get a new dedicated shell."
  (interactive "p")
  (py-shell argprompt args nil "pypy" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))

(defun isympy3 (&optional argprompt args buffer fast exception-buffer split)
  "Start an Pypy interpreter.

With optional \\[universal-argument] get a new dedicated shell."
  (interactive "p")
  (py-shell argprompt args nil "isympy3" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))

;; python-components-font-lock

(defmacro py-rx (&rest regexps)
  "Python mode specialized rx macro.
This variant of ‘rx’ supports common Python named REGEXPS."
  `(rx-let ((sp-bsnl (or space (and ?\\ ?\n)))
            (sp-nl (or space (and (? ?\\) ?\n)))
            (block-start       (seq symbol-start
                                    (or "def" "class" "if" "elif" "else" "try"
                                        "except" "finally" "for" "while" "with"
                                        ;; Python 3.10+ PEP634
                                        "match" "case"
                                        ;; Python 3.5+ PEP492
                                        (and "async" (+ space)
                                             (or "def" "for" "with")))
                                    symbol-end))
            (dedenter          (seq symbol-start
                                    (or "elif" "else" "except" "finally")
                                    symbol-end))
            (block-ender       (seq symbol-start
                                    (or
                                     "break" "continue" "pass" "raise" "return")
                                    symbol-end))
            (decorator         (seq line-start (* space) ?@ (any letter ?_)
                                    (* (any word ?_))))
            (defun             (seq symbol-start
                                    (or "def" "class"
                                        ;; Python 3.5+ PEP492
                                        (and "async" (+ space) "def"))
                                    symbol-end))
            (if-name-main      (seq line-start "if" (+ space) "__name__"
                                    (+ space) "==" (+ space)
                                    (any ?' ?\") "__main__" (any ?' ?\")
                                    (* space) ?:))
            (symbol-name       (seq (any letter ?_) (* (any word ?_))))
            (assignment-target (seq (? ?*)
                                    (* symbol-name ?.) symbol-name
                                    (? ?\[ (+ (not ?\])) ?\])))
            (grouped-assignment-target (seq (? ?*)
                                            (* symbol-name ?.) (group symbol-name)
                                            (? ?\[ (+ (not ?\])) ?\])))
            (open-paren        (or "{" "[" "("))
            (close-paren       (or "}" "]" ")"))
            (simple-operator   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))
            (not-simple-operator (not (or simple-operator ?\n)))
            (operator          (or "==" ">="
                                   "**" "//" "<<" ">>" "<=" "!="
                                   "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                   "=" "%"))
            (assignment-operator (or "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                     ">>=" "<<=" "&=" "^=" "|="
                                     "="))
            (string-delimiter  (seq
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by an
                                    ;; escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"\"\"" "\"" "'''" "'"))))
            (coding-cookie (seq line-start ?# (* space)
                                (or
                                 ;; # coding=<encoding name>
                                 (: "coding" (or ?: ?=) (* space)
                                    (group-n 1 (+ (or word ?-))))
                                 ;; # -*- coding: <encoding name> -*-
                                 (: "-*-" (* space) "coding:" (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) "-*-")
                                 ;; # vim: set fileencoding=<encoding name> :
                                 (: "vim:" (* space) "set" (+ space)
                                    "fileencoding" (* space) ?= (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) ":"))))
            (bytes-escape-sequence
             (seq (not "\\")
                  (group (or "\\\\" "\\'" "\\a" "\\b" "\\f"
                             "\\n" "\\r" "\\t" "\\v"
                             (seq "\\" (** 1 3 (in "0-7")))
                             (seq "\\x" hex hex)))))
            (string-escape-sequence
             (or bytes-escape-sequence
                 (seq (not "\\")
                      (or (group-n 1 "\\u" (= 4 hex))
                          (group-n 1 "\\U" (= 8 hex))
                          (group-n 1 "\\N{" (*? anychar) "}"))))))
     (rx ,@regexps)))

;; lifted from python.el
(defun py-font-lock-assignment-matcher (regexp)
  "Font lock matcher for assignments based on REGEXP.
Search for next occurrence if REGEXP matched within a ‘paren’
context (to avoid, e.g., default values for arguments or passing
arguments by name being treated as assignments) or is followed by
an '=' sign (to avoid '==' being treated as an assignment.  Set
point to the position one character before the end of the
occurrence found so that subsequent searches can detect the '='
sign in chained assignment."
  (lambda (limit)
    (cl-loop while (re-search-forward regexp limit t)
             unless (or
                     ;; (python-syntax-context 'paren)
                     (nth 1 (parse-partial-sexp (point-min) (point)))
                        (equal (char-after) ?=))
               return (progn (backward-char) t))))

(defconst python-font-lock-keywords
  ;; Keywords
  `(,(rx symbol-start
         (or
          "if" "and" "del"  "not" "while" "as" "elif" "global"
          "or" "async with" "with" "assert" "else"  "pass" "yield" "break"
          "exec" "in" "continue" "finally" "is" "except" "raise"
          "return"  "async for" "for" "lambda" "await" "match" "case")
         symbol-end)
    (,(rx symbol-start (or "async def" "def" "class") symbol-end) . py-def-class-face)
    (,(rx symbol-start (or "import" "from") symbol-end) . py-import-from-face)
    (,(rx symbol-start (or "try" "if") symbol-end) . py-try-if-face)
    ;; functions
    (,(rx symbol-start "def" (1+ space) (group (seq (any letter ?_) (* (any word ?_)))))
     ;; (1 font-lock-function-name-face))
     (1 py-def-face))
    (,(rx symbol-start "async def" (1+ space) (group (seq (any letter ?_) (* (any word ?_)))))
     ;; (1 font-lock-function-name-face))
     (1 py-def-face))
    ;; classes
    (,(rx symbol-start (group "class") (1+ space) (group (seq (any letter ?_) (* (any word ?_)))))
     (1 py-def-class-face) (2 py-class-name-face))
    (,(rx symbol-start
          (or"Ellipsis" "True" "False" "None"  "__debug__" "NotImplemented") symbol-end) . py-pseudo-keyword-face)
    ;; Decorators.
    (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                            (0+ "." (1+ (or word ?_)))))
     (1 py-decorators-face))
    (,(rx symbol-start (or "cls" "self")
          symbol-end) . py-object-reference-face)

    ;; Exceptions
    (,(rx word-start
          (or "ArithmeticError" "AssertionError" "AttributeError"
              "BaseException" "BufferError" "BytesWarning" "DeprecationWarning"
              "EOFError" "EnvironmentError" "Exception" "FloatingPointError"
              "FutureWarning" "GeneratorExit" "IOError" "ImportError"
              "ImportWarning" "IndentationError" "IndexError" "KeyError"
              "KeyboardInterrupt" "LookupError" "MemoryError" "NameError" "NoResultFound"
              "NotImplementedError" "OSError" "OverflowError"
              "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
              "RuntimeWarning" "StandardError" "StopIteration" "SyntaxError"
              "SyntaxWarning" "SystemError" "SystemExit" "TabError" "TypeError"
              "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
              "UnicodeError" "UnicodeTranslateError" "UnicodeWarning"
              "UserWarning" "ValueError" "Warning" "ZeroDivisionError"
              ;; OSError subclasses
              "BlockIOError" "ChildProcessError" "ConnectionError"
              "BrokenPipError" "ConnectionAbortedError"
              "ConnectionRefusedError" "ConnectionResetError"
              "FileExistsError" "FileNotFoundError" "InterruptedError"
              "IsADirectoryError" "NotADirectoryError" "PermissionError"
              "ProcessLookupError" "TimeoutError")
          word-end)
     . py-exception-name-face)
    ;; Builtins
    (,(rx
       (or space line-start (not (any ".")))
       symbol-start
       (group (or "_" "__doc__" "__import__" "__name__" "__package__" "abs" "all"
                  "any" "apply" "basestring" "bin" "bool" "buffer" "bytearray"
                  "bytes" "callable" "chr" "classmethod" "cmp" "coerce" "compile"
                  "complex" "delattr" "dict" "dir" "divmod" "enumerate" "eval"
                  "execfile" "filter" "float" "format" "frozenset"
                  "getattr" "globals" "hasattr" "hash" "help" "hex" "id" "input"
                  "int" "intern" "isinstance" "issubclass" "iter" "len" "list"
                  "locals" "long" "map" "max" "min" "next" "object" "oct" "open"
                  "ord" "pow" "property" "range" "raw_input" "reduce"
                  "reload" "repr" "reversed" "round" "set" "setattr" "slice"
                  "sorted" "staticmethod" "str" "sum" "super" "tuple" "type"
                  "unichr" "unicode" "vars" "xrange" "zip"))
       symbol-end)
     . (1 py-builtins-face))
    ;; lifted from python.el
        ;; multiple assignment
    ;; (note that type hints are not allowed for multiple assignments)
    ;;   a, b, c = 1, 2, 3
    ;;   a, *b, c = 1, 2, 3, 4, 5
    ;;   [a, b] = (1, 2)
    ;;   (l[1], l[2]) = (10, 11)
    ;;   (a, b, c, *d) = *x, y = 5, 6, 7, 8, 9
    ;;   (a,) = 'foo'
    ;;   (*a,) = ['foo', 'bar', 'baz']
    ;;   d.x, d.y[0], *d.z = 'a', 'b', 'c', 'd', 'e'
    ;; and variants thereof
    ;; the cases
    ;;   (a) = 5
    ;;   [a] = 5,
    ;;   [*a] = 5, 6
    ;; are handled separately below
    (,(py-font-lock-assignment-matcher
        (py-rx (? (or "[" "(") (* sp-nl))
                   grouped-assignment-target (* sp-nl) ?, (* sp-nl)
                   (* assignment-target (* sp-nl) ?, (* sp-nl))
                   (? assignment-target (* sp-nl))
                   (? ?, (* sp-nl))
                   (? (or ")" "]") (* sp-bsnl))
                   (group assignment-operator)))
     (1 py-variable-name-face)
     (2 'font-lock-operator-face)
     (,(py-rx grouped-assignment-target)
      (progn
        (goto-char (match-end 1))       ; go back after the first symbol
        (match-beginning 2))            ; limit the search until the assignment
      nil
      (1 py-variable-name-face)))
    ;; single assignment with type hints, e.g.
    ;;   a: int = 5
    ;;   b: Tuple[Optional[int], Union[Sequence[str], str]] = (None, 'foo')
    ;;   c: Collection = {1, 2, 3}
    ;;   d: Mapping[int, str] = {1: 'bar', 2: 'baz'}
    (,(py-font-lock-assignment-matcher
       (py-rx (or line-start ?\;) (* sp-bsnl)
                  grouped-assignment-target (* sp-bsnl)
                  (? ?: (* sp-bsnl) (+ not-simple-operator) (* sp-bsnl))
                  (group assignment-operator)))
     (1 py-variable-name-face)
     (2 'font-lock-operator-face))
    ;; special cases
    ;;   (a) = 5
    ;;   [a] = 5,
    ;;   [*a] = 5, 6
    (,(py-font-lock-assignment-matcher
       (py-rx (or line-start ?\; ?=) (* sp-bsnl)
                  (or "[" "(") (* sp-nl)
                  grouped-assignment-target (* sp-nl)
                  (or ")" "]") (* sp-bsnl)
                  (group assignment-operator)))
     (1 py-variable-name-face)
     (2 'font-lock-operator-face))
    ;; https://emacs.stackexchange.com/questions/55184/
    ;; how-to-highlight-in-different-colors-for-variables-inside-fstring-on-python-mo
    ;;
    ;; this is the full string.
    ;; group 1 is the quote type and a closing quote is matched
    ;; group 2 is the string part
    ;; ("f\\(['\"]\\{1,3\\}\\)\\([^\\1]+?\\)\\1"
    ;;  ;; these are the {keywords}
    ;;  ("{[^}]*?}"
    ;;   ;; Pre-match form
    ;;   (progn (goto-char (match-beginning 0)) (match-end 0))
    ;;   ;; Post-match form
    ;;   (goto-char (match-end 0))
    ;;   ;; face for this match
    ;;   ;; (0 font-lock-variable-name-face t)))
    ;;   (0 py-variable-name-face t)))
    ;; Numbers
    ;;        (,(rx symbol-start (or (1+ digit) (1+ hex-digit)) symbol-end) . py-number-face)
    ("\\_<[[:digit:]]+\\_>" . py-number-face))
     ;; ,(rx symbol-start (1+ digit) symbol-end)

  "Keywords matching font-lock")

;; python-components-menu
(defun py-define-menu (map)
  (easy-menu-define py-menu map "Py"
    `("Python"
      ("Interpreter"
       ["Ipython" ipython
	:help " ‘ipython’
Start an IPython interpreter."]

       ["Ipython2\.7" ipython2\.7
	:help " `ipython2\.7'"]

       ["Ipython3" ipython3
	:help " `ipython3'
Start an IPython3 interpreter."]

       ["Jython" jython
	:help " ‘jython’
Start an Jython interpreter."]

       ["Python" python
	:help " ‘python’
Start an Python interpreter."]

       ["Python2" python2
	:help " `python2'
Start an Python2 interpreter."]

       ["Python3" python3
	:help " `python3'
Start an Python3 interpreter."]
       ["SymPy" isympy3
	:help " `isympy3'
Start an SymPy interpreter."])

      ("Edit"
       ("Shift"
	("Shift right"
	 ["Shift block right" py-shift-block-right
	  :help " ‘py-shift-block-right’
Indent block by COUNT spaces."]

	 ["Shift block or clause right" py-shift-block-or-clause-right
	  :help " ‘py-shift-block-or-clause-right’
Indent block-or-clause by COUNT spaces."]

	 ["Shift class right" py-shift-class-right
	  :help " ‘py-shift-class-right’
Indent class by COUNT spaces."]

	 ["Shift clause right" py-shift-clause-right
	  :help " ‘py-shift-clause-right’
Indent clause by COUNT spaces."]

	 ["Shift comment right" py-shift-comment-right
	  :help " ‘py-shift-comment-right’
Indent comment by COUNT spaces."]

	 ["Shift def right" py-shift-def-right
	  :help " ‘py-shift-def-right’
Indent def by COUNT spaces."]

	 ["Shift def or class right" py-shift-def-or-class-right
	  :help " ‘py-shift-def-or-class-right’
Indent def-or-class by COUNT spaces."]

	 ["Shift indent right" py-shift-indent-right
	  :help " ‘py-shift-indent-right’
Indent indent by COUNT spaces."]

	 ["Shift minor block right" py-shift-minor-block-right
	  :help " ‘py-shift-minor-block-right’
Indent minor-block by COUNT spaces."]

	 ["Shift paragraph right" py-shift-paragraph-right
	  :help " ‘py-shift-paragraph-right’
Indent paragraph by COUNT spaces."]

	 ["Shift region right" py-shift-region-right
	  :help " ‘py-shift-region-right’
Indent region by COUNT spaces."]

	 ["Shift statement right" py-shift-statement-right
	  :help " ‘py-shift-statement-right’
Indent statement by COUNT spaces."]

	 ["Shift top level right" py-shift-top-level-right
	  :help " ‘py-shift-top-level-right’
Indent top-level by COUNT spaces."])
	("Shift left"
	 ["Shift block left" py-shift-block-left
	  :help " ‘py-shift-block-left’
Dedent block by COUNT spaces."]

	 ["Shift block or clause left" py-shift-block-or-clause-left
	  :help " ‘py-shift-block-or-clause-left’
Dedent block-or-clause by COUNT spaces."]

	 ["Shift class left" py-shift-class-left
	  :help " ‘py-shift-class-left’
Dedent class by COUNT spaces."]

	 ["Shift clause left" py-shift-clause-left
	  :help " ‘py-shift-clause-left’
Dedent clause by COUNT spaces."]

	 ["Shift comment left" py-shift-comment-left
	  :help " ‘py-shift-comment-left’
Dedent comment by COUNT spaces."]

	 ["Shift def left" py-shift-def-left
	  :help " ‘py-shift-def-left’
Dedent def by COUNT spaces."]

	 ["Shift def or class left" py-shift-def-or-class-left
	  :help " ‘py-shift-def-or-class-left’
Dedent def-or-class by COUNT spaces."]

	 ["Shift indent left" py-shift-indent-left
	  :help " ‘py-shift-indent-left’
Dedent indent by COUNT spaces."]

	 ["Shift minor block left" py-shift-minor-block-left
	  :help " ‘py-shift-minor-block-left’
Dedent minor-block by COUNT spaces."]

	 ["Shift paragraph left" py-shift-paragraph-left
	  :help " ‘py-shift-paragraph-left’
Dedent paragraph by COUNT spaces."]

	 ["Shift region left" py-shift-region-left
	  :help " ‘py-shift-region-left’
Dedent region by COUNT spaces."]

	 ["Shift statement left" py-shift-statement-left
	  :help " ‘py-shift-statement-left’
Dedent statement by COUNT spaces."]))
       ("Mark"
	["Mark block" py-mark-block
	 :help " ‘py-mark-block’
Mark block, take beginning of line positions."]

	["Mark block or clause" py-mark-block-or-clause
	 :help " ‘py-mark-block-or-clause’
Mark block-or-clause, take beginning of line positions."]

	["Mark class" py-mark-class
	 :help " ‘py-mark-class’
Mark class, take beginning of line positions."]

	["Mark clause" py-mark-clause
	 :help " ‘py-mark-clause’
Mark clause, take beginning of line positions."]

	["Mark comment" py-mark-comment
	 :help " ‘py-mark-comment’
Mark comment at point."]

	["Mark def" py-mark-def
	 :help " ‘py-mark-def’
Mark def, take beginning of line positions."]

	["Mark def or class" py-mark-def-or-class
	 :help " ‘py-mark-def-or-class’
Mark def-or-class, take beginning of line positions."]

	["Mark expression" py-mark-expression
	 :help " ‘py-mark-expression’
Mark expression at point."]

	["Mark except block" py-mark-except-block
	 :help " ‘py-mark-except-block’
Mark except-block, take beginning of line positions."]

	["Mark if block" py-mark-if-block
	 :help " ‘py-mark-if-block’
Mark if-block, take beginning of line positions."]

	["Mark indent" py-mark-indent
	 :help " ‘py-mark-indent’
Mark indent, take beginning of line positions."]

	["Mark line" py-mark-line
	 :help " ‘py-mark-line’
Mark line at point."]

	["Mark minor block" py-mark-minor-block
	 :help " ‘py-mark-minor-block’
Mark minor-block, take beginning of line positions."]

	["Mark partial expression" py-mark-partial-expression
	 :help " ‘py-mark-partial-expression’
Mark partial-expression at point."]

	["Mark paragraph" py-mark-paragraph
	 :help " ‘py-mark-paragraph’
Mark paragraph at point."]

	["Mark section" py-mark-section
	 :help " ‘py-mark-section’
Mark section at point."]

	["Mark statement" py-mark-statement
	 :help " ‘py-mark-statement’
Mark statement, take beginning of line positions."]

	["Mark top level" py-mark-top-level
	 :help " ‘py-mark-top-level’
Mark top-level, take beginning of line positions."]

	["Mark try block" py-mark-try-block
	 :help " ‘py-mark-try-block’
Mark try-block, take beginning of line positions."])
       ("Copy"
	["Copy block" py-copy-block
	 :help " ‘py-copy-block’
Copy block at point."]

	["Copy block or clause" py-copy-block-or-clause
	 :help " ‘py-copy-block-or-clause’
Copy block-or-clause at point."]

	["Copy class" py-copy-class
	 :help " ‘py-copy-class’
Copy class at point."]

	["Copy clause" py-copy-clause
	 :help " ‘py-copy-clause’
Copy clause at point."]

	["Copy comment" py-copy-comment
	 :help " ‘py-copy-comment’"]

	["Copy def" py-copy-def
	 :help " ‘py-copy-def’
Copy def at point."]

	["Copy def or class" py-copy-def-or-class
	 :help " ‘py-copy-def-or-class’
Copy def-or-class at point."]

	["Copy expression" py-copy-expression
	 :help " ‘py-copy-expression’
Copy expression at point."]

	["Copy except block" py-copy-except-block
	 :help " ‘py-copy-except-block’"]

	["Copy if block" py-copy-if-block
	 :help " ‘py-copy-if-block’"]

	["Copy indent" py-copy-indent
	 :help " ‘py-copy-indent’
Copy indent at point."]

	["Copy line" py-copy-line
	 :help " ‘py-copy-line’
Copy line at point."]

	["Copy minor block" py-copy-minor-block
	 :help " ‘py-copy-minor-block’
Copy minor-block at point."]

	["Copy partial expression" py-copy-partial-expression
	 :help " ‘py-copy-partial-expression’
Copy partial-expression at point."]

	["Copy paragraph" py-copy-paragraph
	 :help " ‘py-copy-paragraph’
Copy paragraph at point."]

	["Copy section" py-copy-section
	 :help " ‘py-copy-section’"]

	["Copy statement" py-copy-statement
	 :help " ‘py-copy-statement’
Copy statement at point."]

	["Copy top level" py-copy-top-level
	 :help " ‘py-copy-top-level’
Copy top-level at point."])
       ("Kill"
	["Kill block" py-kill-block
	 :help " ‘py-kill-block’
Delete block at point."]

	["Kill block or clause" py-kill-block-or-clause
	 :help " ‘py-kill-block-or-clause’
Delete block-or-clause at point."]

	["Kill class" py-kill-class
	 :help " ‘py-kill-class’
Delete class at point."]

	["Kill clause" py-kill-clause
	 :help " ‘py-kill-clause’
Delete clause at point."]

	["Kill comment" py-kill-comment
	 :help " ‘py-kill-comment’
Delete comment at point."]

	["Kill def" py-kill-def
	 :help " ‘py-kill-def’
Delete def at point."]

	["Kill def or class" py-kill-def-or-class
	 :help " ‘py-kill-def-or-class’
Delete def-or-class at point."]

	["Kill expression" py-kill-expression
	 :help " ‘py-kill-expression’
Delete expression at point."]

	["Kill except block" py-kill-except-block
	 :help " ‘py-kill-except-block’
Delete except-block at point."]

	["Kill if block" py-kill-if-block
	 :help " ‘py-kill-if-block’
Delete if-block at point."]

	["Kill indent" py-kill-indent
	 :help " ‘py-kill-indent’
Delete indent at point."]

	["Kill line" py-kill-line
	 :help " ‘py-kill-line’
Delete line at point."]

	["Kill minor block" py-kill-minor-block
	 :help " ‘py-kill-minor-block’
Delete minor-block at point."]

	["Kill partial expression" py-kill-partial-expression
	 :help " ‘py-kill-partial-expression’
Delete partial-expression at point."]

	["Kill paragraph" py-kill-paragraph
	 :help " ‘py-kill-paragraph’
Delete paragraph at point."]

	["Kill section" py-kill-section
	 :help " ‘py-kill-section’
Delete section at point."]

	["Kill statement" py-kill-statement
	 :help " ‘py-kill-statement’
Delete statement at point."]

	["Kill top level" py-kill-top-level
	 :help " ‘py-kill-top-level’
Delete top-level at point."]

	["Kill try block" py-kill-try-block
	 :help " ‘py-kill-try-block’
Delete try-block at point."])
       ("Delete"
	["Delete block" py-delete-block
	 :help " ‘py-delete-block’
Delete BLOCK at point until beginning-of-line."]

	["Delete block or clause" py-delete-block-or-clause
	 :help " ‘py-delete-block-or-clause’
Delete BLOCK-OR-CLAUSE at point until beginning-of-line."]

	["Delete class" py-delete-class
	 :help " ‘py-delete-class’
Delete CLASS at point until beginning-of-line."]

	["Delete clause" py-delete-clause
	 :help " ‘py-delete-clause’
Delete CLAUSE at point until beginning-of-line."]

	["Delete comment" py-delete-comment
	 :help " ‘py-delete-comment’
Delete COMMENT at point."]

	["Delete def" py-delete-def
	 :help " ‘py-delete-def’
Delete DEF at point until beginning-of-line."]

	["Delete def or class" py-delete-def-or-class
	 :help " ‘py-delete-def-or-class’
Delete DEF-OR-CLASS at point until beginning-of-line."]

	["Delete expression" py-delete-expression
	 :help " ‘py-delete-expression’
Delete EXPRESSION at point."]

	["Delete except block" py-delete-except-block
	 :help " ‘py-delete-except-block’
Delete EXCEPT-BLOCK at point until beginning-of-line."]

	["Delete if block" py-delete-if-block
	 :help " ‘py-delete-if-block’
Delete IF-BLOCK at point until beginning-of-line."]

	["Delete indent" py-delete-indent
	 :help " ‘py-delete-indent’
Delete INDENT at point until beginning-of-line."]

	["Delete line" py-delete-line
	 :help " ‘py-delete-line’
Delete LINE at point."]

	["Delete minor block" py-delete-minor-block
	 :help " ‘py-delete-minor-block’
Delete MINOR-BLOCK at point until beginning-of-line."]

	["Delete partial expression" py-delete-partial-expression
	 :help " ‘py-delete-partial-expression’
Delete PARTIAL-EXPRESSION at point."]

	["Delete paragraph" py-delete-paragraph
	 :help " ‘py-delete-paragraph’
Delete PARAGRAPH at point."]

	["Delete section" py-delete-section
	 :help " ‘py-delete-section’
Delete SECTION at point."]

	["Delete statement" py-delete-statement
	 :help " ‘py-delete-statement’
Delete STATEMENT at point until beginning-of-line."]

	["Delete top level" py-delete-top-level
	 :help " ‘py-delete-top-level’
Delete TOP-LEVEL at point."]

	["Delete try block" py-delete-try-block
	 :help " ‘py-delete-try-block’
Delete TRY-BLOCK at point until beginning-of-line."])
       ("Comment"
	["Comment block" py-comment-block
	 :help " ‘py-comment-block’
Comments block at point."]

	["Comment block or clause" py-comment-block-or-clause
	 :help " ‘py-comment-block-or-clause’
Comments block-or-clause at point."]

	["Comment class" py-comment-class
	 :help " ‘py-comment-class’
Comments class at point."]

	["Comment clause" py-comment-clause
	 :help " ‘py-comment-clause’
Comments clause at point."]

	["Comment def" py-comment-def
	 :help " ‘py-comment-def’
Comments def at point."]

	["Comment def or class" py-comment-def-or-class
	 :help " ‘py-comment-def-or-class’
Comments def-or-class at point."]

	["Comment indent" py-comment-indent
	 :help " ‘py-comment-indent’
Comments indent at point."]

	["Comment minor block" py-comment-minor-block
	 :help " ‘py-comment-minor-block’
Comments minor-block at point."]

	["Comment section" py-comment-section
	 :help " ‘py-comment-section’
Comments section at point."]

	["Comment statement" py-comment-statement
	 :help " ‘py-comment-statement’
Comments statement at point."]

	["Comment top level" py-comment-top-level
	 :help " ‘py-comment-top-level’
Comments top-level at point."]))
      ("Move"
       ("Backward"

	["Backward def or class" py-backward-def-or-class
	 :help " ‘py-backward-def-or-class’
Go to beginning of def-or-class."]

	["Backward class" py-backward-class
	 :help " ‘py-backward-class’
Go to beginning of class."]

	["Backward def" py-backward-def
	 :help " ‘py-backward-def’
Go to beginning of def."]

	["Backward block" py-backward-block
	 :help " ‘py-backward-block’
Go to beginning of ‘block’."]

	["Backward statement" py-backward-statement
	 :help " ‘py-backward-statement’
Go to the initial line of a simple statement."]

	["Backward indent" py-backward-indent
	 :help " ‘py-backward-indent’
Go to the beginning of a section of equal indent."]

	["Backward top level" py-backward-top-level
	 :help " ‘py-backward-top-level’
Go up to beginning of statments until level of indentation is null."]

	("Other"
	 ["Backward section" py-backward-section
	  :help " ‘py-backward-section’
Go to next section start upward in buffer."]

	 ["Backward expression" py-backward-expression
	  :help " ‘py-backward-expression’"]

	 ["Backward partial expression" py-backward-partial-expression
	  :help " ‘py-backward-partial-expression’"]

	 ["Backward assignment" py-backward-assignment
	  :help " ‘py-backward-assignment’"]

	 ["Backward block or clause" py-backward-block-or-clause
	  :help " ‘py-backward-block-or-clause’
Go to beginning of ‘block-or-clause’."]

	 ["Backward clause" py-backward-clause
	  :help " ‘py-backward-clause’
Go to beginning of ‘clause’."]

	 ["Backward elif block" py-backward-elif-block
	  :help " ‘py-backward-elif-block’
Go to beginning of ‘elif-block’."]

	 ["Backward else block" py-backward-else-block
	  :help " ‘py-backward-else-block’
Go to beginning of ‘else-block’."]

	 ["Backward except block" py-backward-except-block
	  :help " ‘py-backward-except-block’
Go to beginning of ‘except-block’."]

	 ["Backward if block" py-backward-if-block
	  :help " ‘py-backward-if-block’
Go to beginning of ‘if-block’."]

	 ["Backward minor block" py-backward-minor-block
	  :help " ‘py-backward-minor-block’
Go to beginning of ‘minor-block’."]

	 ["Backward try block" py-backward-try-block
	  :help " ‘py-backward-try-block’
Go to beginning of ‘try-block’."]))
       ("Forward"
	["Forward def or class" py-forward-def-or-class
	 :help " ‘py-forward-def-or-class’
Go to end of def-or-class."]

	["Forward class" py-forward-class
	 :help " ‘py-forward-class’
Go to end of class."]

	["Forward def" py-forward-def
	 :help " ‘py-forward-def’
Go to end of def."]

	["Forward block" py-forward-block
	 :help " ‘py-forward-block’
Go to end of block."]

	["Forward statement" py-forward-statement
	 :help " ‘py-forward-statement’
Go to the last char of current statement."]

	["Forward indent" py-forward-indent
	 :help " ‘py-forward-indent’
Go to the end of a section of equal indentation."]

	["Forward top level" py-forward-top-level
	 :help " ‘py-forward-top-level’
Go to end of top-level form at point."]

	("Other"
	 ["Forward section" py-forward-section
	  :help " ‘py-forward-section’
Go to next section end downward in buffer."]

	 ["Forward expression" py-forward-expression
	  :help " ‘py-forward-expression’"]

	 ["Forward partial expression" py-forward-partial-expression
	  :help " ‘py-forward-partial-expression’"]

	 ["Forward assignment" py-forward-assignment
	  :help " ‘py-forward-assignment’"]

	 ["Forward block or clause" py-forward-block-or-clause
	  :help " ‘py-forward-block-or-clause’
Go to end of block-or-clause."]

	 ["Forward clause" py-forward-clause
	  :help " ‘py-forward-clause’
Go to end of clause."]

	 ["Forward for block" py-forward-for-block
	 :help " ‘py-forward-for-block’
Go to end of for-block."]

	 ["Forward elif block" py-forward-elif-block
	  :help " ‘py-forward-elif-block’
Go to end of elif-block."]

	 ["Forward else block" py-forward-else-block
	  :help " ‘py-forward-else-block’
Go to end of else-block."]

	 ["Forward except block" py-forward-except-block
	  :help " ‘py-forward-except-block’
Go to end of except-block."]

	 ["Forward if block" py-forward-if-block
	  :help " ‘py-forward-if-block’
Go to end of if-block."]

	 ["Forward minor block" py-forward-minor-block
	  :help " ‘py-forward-minor-block’
Go to end of minor-block."]
	 ["Forward try block" py-forward-try-block
	  :help " ‘py-forward-try-block’
Go to end of try-block."]))
       ("BOL-forms"
	("Backward"
	 ["Backward block bol" py-backward-block-bol
	  :help " ‘py-backward-block-bol’
Go to beginning of ‘block’, go to BOL."]

	 ["Backward block or clause bol" py-backward-block-or-clause-bol
	  :help " ‘py-backward-block-or-clause-bol’
Go to beginning of ‘block-or-clause’, go to BOL."]

	 ["Backward class bol" py-backward-class-bol
	  :help " ‘py-backward-class-bol’
Go to beginning of class, go to BOL."]

	 ["Backward clause bol" py-backward-clause-bol
	  :help " ‘py-backward-clause-bol’
Go to beginning of ‘clause’, go to BOL."]

	 ["Backward def bol" py-backward-def-bol
	  :help " ‘py-backward-def-bol’
Go to beginning of def, go to BOL."]

	 ["Backward def or class bol" py-backward-def-or-class-bol
	  :help " ‘py-backward-def-or-class-bol’
Go to beginning of def-or-class, go to BOL."]

	 ["Backward elif block bol" py-backward-elif-block-bol
	  :help " ‘py-backward-elif-block-bol’
Go to beginning of ‘elif-block’, go to BOL."]

	 ["Backward else block bol" py-backward-else-block-bol
	  :help " ‘py-backward-else-block-bol’
Go to beginning of ‘else-block’, go to BOL."]

	 ["Backward except block bol" py-backward-except-block-bol
	  :help " ‘py-backward-except-block-bol’
Go to beginning of ‘except-block’, go to BOL."]

	 ["Backward expression bol" py-backward-expression-bol
	  :help " ‘py-backward-expression-bol’"]

	 ["Backward for block bol" py-backward-for-block-bol
	  :help " ‘py-backward-for-block-bol’
Go to beginning of ‘for-block’, go to BOL."]

	 ["Backward if block bol" py-backward-if-block-bol
	  :help " ‘py-backward-if-block-bol’
Go to beginning of ‘if-block’, go to BOL."]

	 ["Backward indent bol" py-backward-indent-bol
	  :help " ‘py-backward-indent-bol’
Go to the beginning of line of a section of equal indent."]

	 ["Backward minor block bol" py-backward-minor-block-bol
	  :help " ‘py-backward-minor-block-bol’
Go to beginning of ‘minor-block’, go to BOL."]

	 ["Backward partial expression bol" py-backward-partial-expression-bol
	  :help " ‘py-backward-partial-expression-bol’"]

	 ["Backward section bol" py-backward-section-bol
	  :help " ‘py-backward-section-bol’"]

	 ["Backward statement bol" py-backward-statement-bol
	  :help " ‘py-backward-statement-bol’
Goto beginning of line where statement starts."]

	 ["Backward try block bol" py-backward-try-block-bol
	  :help " ‘py-backward-try-block-bol’
Go to beginning of ‘try-block’, go to BOL."])
	("Forward"
	 ["Forward block bol" py-forward-block-bol
	  :help " ‘py-forward-block-bol’
Goto beginning of line following end of block."]

	 ["Forward block or clause bol" py-forward-block-or-clause-bol
	  :help " ‘py-forward-block-or-clause-bol’
Goto beginning of line following end of block-or-clause."]

	 ["Forward class bol" py-forward-class-bol
	  :help " ‘py-forward-class-bol’
Goto beginning of line following end of class."]

	 ["Forward clause bol" py-forward-clause-bol
	  :help " ‘py-forward-clause-bol’
Goto beginning of line following end of clause."]

	 ["Forward def bol" py-forward-def-bol
	  :help " ‘py-forward-def-bol’
Goto beginning of line following end of def."]

	 ["Forward def or class bol" py-forward-def-or-class-bol
	  :help " ‘py-forward-def-or-class-bol’
Goto beginning of line following end of def-or-class."]

	 ["Forward elif block bol" py-forward-elif-block-bol
	  :help " ‘py-forward-elif-block-bol’
Goto beginning of line following end of elif-block."]

	 ["Forward else block bol" py-forward-else-block-bol
	  :help " ‘py-forward-else-block-bol’
Goto beginning of line following end of else-block."]

	 ["Forward except block bol" py-forward-except-block-bol
	  :help " ‘py-forward-except-block-bol’
Goto beginning of line following end of except-block."]

	 ["Forward expression bol" py-forward-expression-bol
	  :help " ‘py-forward-expression-bol’"]

	 ["Forward for block bol" py-forward-for-block-bol
	  :help " ‘py-forward-for-block-bol’
Goto beginning of line following end of for-block."]

	 ["Forward if block bol" py-forward-if-block-bol
	  :help " ‘py-forward-if-block-bol’
Goto beginning of line following end of if-block."]

	 ["Forward indent bol" py-forward-indent-bol
	  :help " ‘py-forward-indent-bol’
Go to beginning of line following of a section of equal indentation."]

	 ["Forward minor block bol" py-forward-minor-block-bol
	  :help " ‘py-forward-minor-block-bol’
Goto beginning of line following end of minor-block."]

	 ["Forward partial expression bol" py-forward-partial-expression-bol
	  :help " ‘py-forward-partial-expression-bol’"]

	 ["Forward section bol" py-forward-section-bol
	  :help " ‘py-forward-section-bol’"]

	 ["Forward statement bol" py-forward-statement-bol
	  :help " ‘py-forward-statement-bol’
Go to the beginning-of-line following current statement."]

	 ["Forward top level bol" py-forward-top-level-bol
	  :help " ‘py-forward-top-level-bol’
Go to end of top-level form at point, stop at next beginning-of-line."]

	 ["Forward try block bol" py-forward-try-block-bol
	  :help " ‘py-forward-try-block-bol’
Goto beginning of line following end of try-block."]))
       ("Up/Down"
	["Up" py-up
	 :help " ‘py-up’
Go up or to beginning of form if inside."]

	["Down" py-down
	 :help " ‘py-down’
Go to beginning one level below of compound statement or definition at point."]))
      ("Send"
       ["Execute block" py-execute-block
	:help " ‘py-execute-block’
Send block at point to interpreter."]

       ["Execute block or clause" py-execute-block-or-clause
	:help " ‘py-execute-block-or-clause’
Send block-or-clause at point to interpreter."]

       ["Execute buffer" py-execute-buffer
	:help " ‘py-execute-buffer’
:around advice: `ad-Advice-py-execute-buffer'"]

       ["Execute class" py-execute-class
	:help " ‘py-execute-class’
Send class at point to interpreter."]

       ["Execute clause" py-execute-clause
	:help " ‘py-execute-clause’
Send clause at point to interpreter."]

       ["Execute def" py-execute-def
	:help " ‘py-execute-def’
Send def at point to interpreter."]

       ["Execute def or class" py-execute-def-or-class
	:help " ‘py-execute-def-or-class’
Send def-or-class at point to interpreter."]

       ["Execute expression" py-execute-expression
	:help " ‘py-execute-expression’
Send expression at point to interpreter."]

       ["Execute indent" py-execute-indent
	:help " ‘py-execute-indent’
Send indent at point to interpreter."]

       ["Execute line" py-execute-line
	:help " ‘py-execute-line’
Send line at point to interpreter."]

       ["Execute minor block" py-execute-minor-block
	:help " ‘py-execute-minor-block’
Send minor-block at point to interpreter."]

       ["Execute paragraph" py-execute-paragraph
	:help " ‘py-execute-paragraph’
Send paragraph at point to interpreter."]

       ["Execute partial expression" py-execute-partial-expression
	:help " ‘py-execute-partial-expression’
Send partial-expression at point to interpreter."]

       ["Execute region" py-execute-region
	:help " ‘py-execute-region’
Send region at point to interpreter."]

       ["Execute statement" py-execute-statement
	:help " ‘py-execute-statement’
Send statement at point to interpreter."]

       ["Execute top level" py-execute-top-level
	:help " ‘py-execute-top-level’
Send top-level at point to interpreter."]
       ("Other"
	("IPython"
	 ["Execute block ipython" py-execute-block-ipython
	  :help " ‘py-execute-block-ipython’
Send block at point to IPython interpreter."]

	 ["Execute block or clause ipython" py-execute-block-or-clause-ipython
	  :help " ‘py-execute-block-or-clause-ipython’
Send block-or-clause at point to IPython interpreter."]

	 ["Execute buffer ipython" py-execute-buffer-ipython
	  :help " ‘py-execute-buffer-ipython’
Send buffer at point to IPython interpreter."]

	 ["Execute class ipython" py-execute-class-ipython
	  :help " ‘py-execute-class-ipython’
Send class at point to IPython interpreter."]

	 ["Execute clause ipython" py-execute-clause-ipython
	  :help " ‘py-execute-clause-ipython’
Send clause at point to IPython interpreter."]

	 ["Execute def ipython" py-execute-def-ipython
	  :help " ‘py-execute-def-ipython’
Send def at point to IPython interpreter."]

	 ["Execute def or class ipython" py-execute-def-or-class-ipython
	  :help " ‘py-execute-def-or-class-ipython’
Send def-or-class at point to IPython interpreter."]

	 ["Execute expression ipython" py-execute-expression-ipython
	  :help " ‘py-execute-expression-ipython’
Send expression at point to IPython interpreter."]

	 ["Execute indent ipython" py-execute-indent-ipython
	  :help " ‘py-execute-indent-ipython’
Send indent at point to IPython interpreter."]

	 ["Execute line ipython" py-execute-line-ipython
	  :help " ‘py-execute-line-ipython’
Send line at point to IPython interpreter."]

	 ["Execute minor block ipython" py-execute-minor-block-ipython
	  :help " ‘py-execute-minor-block-ipython’
Send minor-block at point to IPython interpreter."]

	 ["Execute paragraph ipython" py-execute-paragraph-ipython
	  :help " ‘py-execute-paragraph-ipython’
Send paragraph at point to IPython interpreter."]

	 ["Execute partial expression ipython" py-execute-partial-expression-ipython
	  :help " ‘py-execute-partial-expression-ipython’
Send partial-expression at point to IPython interpreter."]

	 ["Execute region ipython" py-execute-region-ipython
	  :help " ‘py-execute-region-ipython’
Send region at point to IPython interpreter."]

	 ["Execute statement ipython" py-execute-statement-ipython
	  :help " ‘py-execute-statement-ipython’
Send statement at point to IPython interpreter."]

	 ["Execute top level ipython" py-execute-top-level-ipython
	  :help " ‘py-execute-top-level-ipython’
Send top-level at point to IPython interpreter."])
	("IPython2"
	 ["Execute block ipython2" py-execute-block-ipython2
	  :help " `py-execute-block-ipython2'"]

	 ["Execute block or clause ipython2" py-execute-block-or-clause-ipython2
	  :help " `py-execute-block-or-clause-ipython2'"]

	 ["Execute buffer ipython2" py-execute-buffer-ipython2
	  :help " `py-execute-buffer-ipython2'"]

	 ["Execute class ipython2" py-execute-class-ipython2
	  :help " `py-execute-class-ipython2'"]

	 ["Execute clause ipython2" py-execute-clause-ipython2
	  :help " `py-execute-clause-ipython2'"]

	 ["Execute def ipython2" py-execute-def-ipython2
	  :help " `py-execute-def-ipython2'"]

	 ["Execute def or class ipython2" py-execute-def-or-class-ipython2
	  :help " `py-execute-def-or-class-ipython2'"]

	 ["Execute expression ipython2" py-execute-expression-ipython2
	  :help " `py-execute-expression-ipython2'"]

	 ["Execute indent ipython2" py-execute-indent-ipython2
	  :help " `py-execute-indent-ipython2'"]

	 ["Execute line ipython2" py-execute-line-ipython2
	  :help " `py-execute-line-ipython2'"]

	 ["Execute minor block ipython2" py-execute-minor-block-ipython2
	  :help " `py-execute-minor-block-ipython2'"]

	 ["Execute paragraph ipython2" py-execute-paragraph-ipython2
	  :help " `py-execute-paragraph-ipython2'"]

	 ["Execute partial expression ipython2" py-execute-partial-expression-ipython2
	  :help " `py-execute-partial-expression-ipython2'"]

	 ["Execute region ipython2" py-execute-region-ipython2
	  :help " `py-execute-region-ipython2'"]

	 ["Execute statement ipython2" py-execute-statement-ipython2
	  :help " `py-execute-statement-ipython2'"]

	 ["Execute top level ipython2" py-execute-top-level-ipython2
	  :help " `py-execute-top-level-ipython2'"])
	("IPython3"
	 ["Execute block ipython3" py-execute-block-ipython3
	  :help " `py-execute-block-ipython3'
Send block at point to IPython interpreter."]

	 ["Execute block or clause ipython3" py-execute-block-or-clause-ipython3
	  :help " `py-execute-block-or-clause-ipython3'
Send block-or-clause at point to IPython interpreter."]

	 ["Execute buffer ipython3" py-execute-buffer-ipython3
	  :help " `py-execute-buffer-ipython3'
Send buffer at point to IPython interpreter."]

	 ["Execute class ipython3" py-execute-class-ipython3
	  :help " `py-execute-class-ipython3'
Send class at point to IPython interpreter."]

	 ["Execute clause ipython3" py-execute-clause-ipython3
	  :help " `py-execute-clause-ipython3'
Send clause at point to IPython interpreter."]

	 ["Execute def ipython3" py-execute-def-ipython3
	  :help " `py-execute-def-ipython3'
Send def at point to IPython interpreter."]

	 ["Execute def or class ipython3" py-execute-def-or-class-ipython3
	  :help " `py-execute-def-or-class-ipython3'
Send def-or-class at point to IPython interpreter."]

	 ["Execute expression ipython3" py-execute-expression-ipython3
	  :help " `py-execute-expression-ipython3'
Send expression at point to IPython interpreter."]

	 ["Execute indent ipython3" py-execute-indent-ipython3
	  :help " `py-execute-indent-ipython3'
Send indent at point to IPython interpreter."]

	 ["Execute line ipython3" py-execute-line-ipython3
	  :help " `py-execute-line-ipython3'
Send line at point to IPython interpreter."]

	 ["Execute minor block ipython3" py-execute-minor-block-ipython3
	  :help " `py-execute-minor-block-ipython3'
Send minor-block at point to IPython interpreter."]

	 ["Execute paragraph ipython3" py-execute-paragraph-ipython3
	  :help " `py-execute-paragraph-ipython3'
Send paragraph at point to IPython interpreter."]

	 ["Execute partial expression ipython3" py-execute-partial-expression-ipython3
	  :help " `py-execute-partial-expression-ipython3'
Send partial-expression at point to IPython interpreter."]

	 ["Execute region ipython3" py-execute-region-ipython3
	  :help " `py-execute-region-ipython3'
Send region at point to IPython interpreter."]

	 ["Execute statement ipython3" py-execute-statement-ipython3
	  :help " `py-execute-statement-ipython3'
Send statement at point to IPython interpreter."]

	 ["Execute top level ipython3" py-execute-top-level-ipython3
	  :help " `py-execute-top-level-ipython3'
Send top-level at point to IPython interpreter."])
	("Jython"
	 ["Execute block jython" py-execute-block-jython
	  :help " ‘py-execute-block-jython’
Send block at point to Jython interpreter."]

	 ["Execute block or clause jython" py-execute-block-or-clause-jython
	  :help " ‘py-execute-block-or-clause-jython’
Send block-or-clause at point to Jython interpreter."]

	 ["Execute buffer jython" py-execute-buffer-jython
	  :help " ‘py-execute-buffer-jython’
Send buffer at point to Jython interpreter."]

	 ["Execute class jython" py-execute-class-jython
	  :help " ‘py-execute-class-jython’
Send class at point to Jython interpreter."]

	 ["Execute clause jython" py-execute-clause-jython
	  :help " ‘py-execute-clause-jython’
Send clause at point to Jython interpreter."]

	 ["Execute def jython" py-execute-def-jython
	  :help " ‘py-execute-def-jython’
Send def at point to Jython interpreter."]

	 ["Execute def or class jython" py-execute-def-or-class-jython
	  :help " ‘py-execute-def-or-class-jython’
Send def-or-class at point to Jython interpreter."]

	 ["Execute expression jython" py-execute-expression-jython
	  :help " ‘py-execute-expression-jython’
Send expression at point to Jython interpreter."]

	 ["Execute indent jython" py-execute-indent-jython
	  :help " ‘py-execute-indent-jython’
Send indent at point to Jython interpreter."]

	 ["Execute line jython" py-execute-line-jython
	  :help " ‘py-execute-line-jython’
Send line at point to Jython interpreter."]

	 ["Execute minor block jython" py-execute-minor-block-jython
	  :help " ‘py-execute-minor-block-jython’
Send minor-block at point to Jython interpreter."]

	 ["Execute paragraph jython" py-execute-paragraph-jython
	  :help " ‘py-execute-paragraph-jython’
Send paragraph at point to Jython interpreter."]

	 ["Execute partial expression jython" py-execute-partial-expression-jython
	  :help " ‘py-execute-partial-expression-jython’
Send partial-expression at point to Jython interpreter."]

	 ["Execute region jython" py-execute-region-jython
	  :help " ‘py-execute-region-jython’
Send region at point to Jython interpreter."]

	 ["Execute statement jython" py-execute-statement-jython
	  :help " ‘py-execute-statement-jython’
Send statement at point to Jython interpreter."]

	 ["Execute top level jython" py-execute-top-level-jython
	  :help " ‘py-execute-top-level-jython’
Send top-level at point to Jython interpreter."])
	("Python"
	 ["Execute block python" py-execute-block-python
	  :help " ‘py-execute-block-python’
Send block at point to default interpreter."]

	 ["Execute block or clause python" py-execute-block-or-clause-python
	  :help " ‘py-execute-block-or-clause-python’
Send block-or-clause at point to default interpreter."]

	 ["Execute buffer python" py-execute-buffer-python
	  :help " ‘py-execute-buffer-python’
Send buffer at point to default interpreter."]

	 ["Execute class python" py-execute-class-python
	  :help " ‘py-execute-class-python’
Send class at point to default interpreter."]

	 ["Execute clause python" py-execute-clause-python
	  :help " ‘py-execute-clause-python’
Send clause at point to default interpreter."]

	 ["Execute def python" py-execute-def-python
	  :help " ‘py-execute-def-python’
Send def at point to default interpreter."]

	 ["Execute def or class python" py-execute-def-or-class-python
	  :help " ‘py-execute-def-or-class-python’
Send def-or-class at point to default interpreter."]

	 ["Execute expression python" py-execute-expression-python
	  :help " ‘py-execute-expression-python’
Send expression at point to default interpreter."]

	 ["Execute indent python" py-execute-indent-python
	  :help " ‘py-execute-indent-python’
Send indent at point to default interpreter."]

	 ["Execute line python" py-execute-line-python
	  :help " ‘py-execute-line-python’
Send line at point to default interpreter."]

	 ["Execute minor block python" py-execute-minor-block-python
	  :help " ‘py-execute-minor-block-python’
Send minor-block at point to default interpreter."]

	 ["Execute paragraph python" py-execute-paragraph-python
	  :help " ‘py-execute-paragraph-python’
Send paragraph at point to default interpreter."]

	 ["Execute partial expression python" py-execute-partial-expression-python
	  :help " ‘py-execute-partial-expression-python’
Send partial-expression at point to default interpreter."]

	 ["Execute region python" py-execute-region-python
	  :help " ‘py-execute-region-python’
Send region at point to default interpreter."]

	 ["Execute statement python" py-execute-statement-python
	  :help " ‘py-execute-statement-python’
Send statement at point to default interpreter."]

	 ["Execute top level python" py-execute-top-level-python
	  :help " ‘py-execute-top-level-python’
Send top-level at point to default interpreter."])
	("Python2"
	 ["Execute block python2" py-execute-block-python2
	  :help " `py-execute-block-python2'
Send block at point to Python2 interpreter."]

	 ["Execute block or clause python2" py-execute-block-or-clause-python2
	  :help " `py-execute-block-or-clause-python2'
Send block-or-clause at point to Python2 interpreter."]

	 ["Execute buffer python2" py-execute-buffer-python2
	  :help " `py-execute-buffer-python2'
Send buffer at point to Python2 interpreter."]

	 ["Execute class python2" py-execute-class-python2
	  :help " `py-execute-class-python2'
Send class at point to Python2 interpreter."]

	 ["Execute clause python2" py-execute-clause-python2
	  :help " `py-execute-clause-python2'
Send clause at point to Python2 interpreter."]

	 ["Execute def python2" py-execute-def-python2
	  :help " `py-execute-def-python2'
Send def at point to Python2 interpreter."]

	 ["Execute def or class python2" py-execute-def-or-class-python2
	  :help " `py-execute-def-or-class-python2'
Send def-or-class at point to Python2 interpreter."]

	 ["Execute expression python2" py-execute-expression-python2
	  :help " `py-execute-expression-python2'
Send expression at point to Python2 interpreter."]

	 ["Execute indent python2" py-execute-indent-python2
	  :help " `py-execute-indent-python2'
Send indent at point to Python2 interpreter."]

	 ["Execute line python2" py-execute-line-python2
	  :help " `py-execute-line-python2'
Send line at point to Python2 interpreter."]

	 ["Execute minor block python2" py-execute-minor-block-python2
	  :help " `py-execute-minor-block-python2'
Send minor-block at point to Python2 interpreter."]

	 ["Execute paragraph python2" py-execute-paragraph-python2
	  :help " `py-execute-paragraph-python2'
Send paragraph at point to Python2 interpreter."]

	 ["Execute partial expression python2" py-execute-partial-expression-python2
	  :help " `py-execute-partial-expression-python2'
Send partial-expression at point to Python2 interpreter."]

	 ["Execute region python2" py-execute-region-python2
	  :help " `py-execute-region-python2'
Send region at point to Python2 interpreter."]

	 ["Execute statement python2" py-execute-statement-python2
	  :help " `py-execute-statement-python2'
Send statement at point to Python2 interpreter."]

	 ["Execute top level python2" py-execute-top-level-python2
	  :help " `py-execute-top-level-python2'
Send top-level at point to Python2 interpreter."])
	("Python3"
	 ["Execute block python3" py-execute-block-python3
	  :help " `py-execute-block-python3'
Send block at point to Python3 interpreter."]

	 ["Execute block or clause python3" py-execute-block-or-clause-python3
	  :help " `py-execute-block-or-clause-python3'
Send block-or-clause at point to Python3 interpreter."]

	 ["Execute buffer python3" py-execute-buffer-python3
	  :help " `py-execute-buffer-python3'
Send buffer at point to Python3 interpreter."]

	 ["Execute class python3" py-execute-class-python3
	  :help " `py-execute-class-python3'
Send class at point to Python3 interpreter."]

	 ["Execute clause python3" py-execute-clause-python3
	  :help " `py-execute-clause-python3'
Send clause at point to Python3 interpreter."]

	 ["Execute def python3" py-execute-def-python3
	  :help " `py-execute-def-python3'
Send def at point to Python3 interpreter."]

	 ["Execute def or class python3" py-execute-def-or-class-python3
	  :help " `py-execute-def-or-class-python3'
Send def-or-class at point to Python3 interpreter."]

	 ["Execute expression python3" py-execute-expression-python3
	  :help " `py-execute-expression-python3'
Send expression at point to Python3 interpreter."]

	 ["Execute indent python3" py-execute-indent-python3
	  :help " `py-execute-indent-python3'
Send indent at point to Python3 interpreter."]

	 ["Execute line python3" py-execute-line-python3
	  :help " `py-execute-line-python3'
Send line at point to Python3 interpreter."]

	 ["Execute minor block python3" py-execute-minor-block-python3
	  :help " `py-execute-minor-block-python3'
Send minor-block at point to Python3 interpreter."]

	 ["Execute paragraph python3" py-execute-paragraph-python3
	  :help " `py-execute-paragraph-python3'
Send paragraph at point to Python3 interpreter."]

	 ["Execute partial expression python3" py-execute-partial-expression-python3
	  :help " `py-execute-partial-expression-python3'
Send partial-expression at point to Python3 interpreter."]

	 ["Execute region python3" py-execute-region-python3
	  :help " `py-execute-region-python3'
Send region at point to Python3 interpreter."]

	 ["Execute statement python3" py-execute-statement-python3
	  :help " `py-execute-statement-python3'
Send statement at point to Python3 interpreter."]

	 ["Execute top level python3" py-execute-top-level-python3
	  :help " `py-execute-top-level-python3'
Send top-level at point to Python3 interpreter."])
	("Ignoring defaults "
	 :help "`M-x py-execute-statement- TAB' for example list commands ignoring defaults

 of ‘py-switch-buffers-on-execute-p’ and ‘py-split-window-on-execute’")))
      ("Hide-Show"
       ("Hide"
	["Hide block" py-hide-block
	 :help " ‘py-hide-block’
Hide block at point."]

	["Hide top level" py-hide-top-level
	 :help " ‘py-hide-top-level’
Hide top-level at point."]

	["Hide def" py-hide-def
	 :help " ‘py-hide-def’
Hide def at point."]

	["Hide def or class" py-hide-def-or-class
	 :help " ‘py-hide-def-or-class’
Hide def-or-class at point."]

	["Hide statement" py-hide-statement
	 :help " ‘py-hide-statement’
Hide statement at point."]

	["Hide class" py-hide-class
	 :help " ‘py-hide-class’
Hide class at point."]

	["Hide clause" py-hide-clause
	 :help " ‘py-hide-clause’
Hide clause at point."]

	["Hide block or clause" py-hide-block-or-clause
	 :help " ‘py-hide-block-or-clause’
Hide block-or-clause at point."]

	["Hide comment" py-hide-comment
	 :help " ‘py-hide-comment’
Hide comment at point."]

	["Hide indent" py-hide-indent
	 :help " ‘py-hide-indent’
Hide indent at point."]

	["Hide expression" py-hide-expression
	 :help " ‘py-hide-expression’
Hide expression at point."]

	["Hide line" py-hide-line
	 :help " ‘py-hide-line’
Hide line at point."]

	["Hide for-block" py-hide-for-block
	 :help " ‘py-hide-for-block’
Hide for-block at point."]

	["Hide if-block" py-hide-if-block
	 :help " ‘py-hide-if-block’
Hide if-block at point."]

	["Hide elif-block" py-hide-elif-block
	 :help " ‘py-hide-elif-block’
Hide elif-block at point."]

	["Hide else-block" py-hide-else-block
	 :help " ‘py-hide-else-block’
Hide else-block at point."]

	["Hide except-block" py-hide-except-block
	 :help " ‘py-hide-except-block’
Hide except-block at point."]

	["Hide minor-block" py-hide-minor-block
	 :help " ‘py-hide-minor-block’
Hide minor-block at point."]

	["Hide paragraph" py-hide-paragraph
	 :help " ‘py-hide-paragraph’
Hide paragraph at point."]

	["Hide partial expression" py-hide-partial-expression
	 :help " ‘py-hide-partial-expression’
Hide partial-expression at point."]

	["Hide section" py-hide-section
	 :help " ‘py-hide-section’
Hide section at point."])
       ("Show"
	["Show all" py-show-all
	 :help " ‘py-show-all’
Show all in buffer."]

	["Show" py-show
	 :help " ‘py-show’
Show hidden code at point."]))
      ("Fast process"
       ["Execute block fast" py-execute-block-fast
	:help " ‘py-execute-block-fast’
Process block at point by a Python interpreter."]

       ["Execute block or clause fast" py-execute-block-or-clause-fast
	:help " ‘py-execute-block-or-clause-fast’
Process block-or-clause at point by a Python interpreter."]

       ["Execute class fast" py-execute-class-fast
	:help " ‘py-execute-class-fast’
Process class at point by a Python interpreter."]

       ["Execute clause fast" py-execute-clause-fast
	:help " ‘py-execute-clause-fast’
Process clause at point by a Python interpreter."]

       ["Execute def fast" py-execute-def-fast
	:help " ‘py-execute-def-fast’
Process def at point by a Python interpreter."]

       ["Execute def or class fast" py-execute-def-or-class-fast
	:help " ‘py-execute-def-or-class-fast’
Process def-or-class at point by a Python interpreter."]

       ["Execute expression fast" py-execute-expression-fast
	:help " ‘py-execute-expression-fast’
Process expression at point by a Python interpreter."]

       ["Execute partial expression fast" py-execute-partial-expression-fast
	:help " ‘py-execute-partial-expression-fast’
Process partial-expression at point by a Python interpreter."]

       ["Execute region fast" py-execute-region-fast
	:help " ‘py-execute-region-fast’"]

       ["Execute statement fast" py-execute-statement-fast
	:help " ‘py-execute-statement-fast’
Process statement at point by a Python interpreter."]

       ["Execute string fast" py-execute-string-fast
	:help " ‘py-execute-string-fast’"]

       ["Execute top level fast" py-execute-top-level-fast
	:help " ‘py-execute-top-level-fast’
Process top-level at point by a Python interpreter."])
      ("Virtualenv"
       ["Virtualenv activate" virtualenv-activate
	:help " ‘virtualenv-activate’
Activate the virtualenv located in DIR"]

       ["Virtualenv deactivate" virtualenv-deactivate
	:help " ‘virtualenv-deactivate’
Deactivate the current virtual enviroment"]

       ["Virtualenv p" virtualenv-p
	:help " ‘virtualenv-p’
Check if a directory is a virtualenv"]

       ["Virtualenv workon" virtualenv-workon
	:help " ‘virtualenv-workon’
Issue a virtualenvwrapper-like virtualenv-workon command"])

      ["Execute import or reload" py-execute-import-or-reload
       :help " ‘py-execute-import-or-reload’
Import the current buffer’s file in a Python interpreter."]
      ("Help"
       ["Find definition" py-find-definition
	:help " ‘py-find-definition’
Find source of definition of SYMBOL."]

       ["Help at point" py-help-at-point
	:help " ‘py-help-at-point’
Print help on symbol at point."]

       ["Info lookup symbol" py-info-lookup-symbol
	:help " ‘py-info-lookup-symbol’"]

       ["Symbol at point" py-symbol-at-point
	:help " ‘py-symbol-at-point’
Return the current Python symbol."])
      ("Debugger"
       ["Execute statement pdb" py-execute-statement-pdb
	:help " ‘py-execute-statement-pdb’
Execute statement running pdb."]

       ["Pdb" pdb
	:help " ‘pdb’
Run pdb on program FILE in buffer `*gud-FILE*'."])
      ("Checks"
       ["Pychecker run" py-pychecker-run
	:help " ‘py-pychecker-run’
*Run pychecker (default on the file currently visited)."]
       ("Pylint"
	["Pylint run" py-pylint-run
	 :help " ‘py-pylint-run’
*Run pylint (default on the file currently visited)."]

	["Pylint help" py-pylint-help
	 :help " ‘py-pylint-help’
Display Pylint command line help messages."]

	["Pylint flymake mode" pylint-flymake-mode
	 :help " ‘pylint-flymake-mode’
Toggle ‘pylint’ ‘flymake-mode’."])
       ("Pep8"
	["Pep8 run" py-pep8-run
	 :help " `py-pep8-run'
*Run pep8, check formatting - default on the file currently visited."]

	["Pep8 help" py-pep8-help
	 :help " `py-pep8-help'
Display pep8 command line help messages."]

	["Pep8 flymake mode" pep8-flymake-mode
	 :help " `pep8-flymake-mode'
Toggle `pep8’ ‘flymake-mode’."])
       ("Pyflakes"
	["Pyflakes run" py-pyflakes-run
	 :help " ‘py-pyflakes-run’
*Run pyflakes (default on the file currently visited)."]

	["Pyflakes help" py-pyflakes-help
	 :help " ‘py-pyflakes-help’
Display Pyflakes command line help messages."]

	["Pyflakes flymake mode" pyflakes-flymake-mode
	 :help " ‘pyflakes-flymake-mode’
Toggle ‘pyflakes’ ‘flymake-mode’."])
       ("Flake8"
	["Flake8 run" py-flake8-run
	 :help " `py-flake8-run'
Flake8 is a wrapper around these tools:"]

	["Flake8 help" py-flake8-help
	 :help " `py-flake8-help'
Display flake8 command line help messages."]
	("Pyflakes-pep8"
	 ["Pyflakes pep8 run" py-pyflakes-pep8-run
	  :help " `py-pyflakes-pep8-run'"]

	 ["Pyflakes pep8 help" py-pyflakes-pep8-help
	  :help " `py-pyflakes-pep8-help'"]

	 ["Pyflakes pep8 flymake mode" pyflakes-pep8-flymake-mode
	  :help " `pyflakes-pep8-flymake-mode'"])))
      ("Customize"

       ["Python-mode customize group" (customize-group 'python-mode)
	:help "Open the customization buffer for Python mode"]
       ("Switches"
	:help "Toggle useful modes"
	("Interpreter"

	 ["Shell prompt read only"
	  (setq py-shell-prompt-read-only
		(not py-shell-prompt-read-only))
	  :help "If non-nil, the python prompt is read only.  Setting this variable will only effect new shells.Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-shell-prompt-read-only]

	 ["Remove cwd from path"
	  (setq py-remove-cwd-from-path
		(not py-remove-cwd-from-path))
	  :help "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion).Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-remove-cwd-from-path]

	 ["Honor IPYTHONDIR "
	  (setq py-honor-IPYTHONDIR-p
		(not py-honor-IPYTHONDIR-p))
	  :help "When non-nil ipython-history file is constructed by \$IPYTHONDIR
followed by "/history". Default is nil.

Otherwise value of py-ipython-history is used. Use `M-x customize-variable' to set it permanently"
:style toggle :selected py-honor-IPYTHONDIR-p]

	 ["Honor PYTHONHISTORY "
	  (setq py-honor-PYTHONHISTORY-p
		(not py-honor-PYTHONHISTORY-p))
	  :help "When non-nil python-history file is set by \$PYTHONHISTORY
Default is nil.

Otherwise value of py-python-history is used. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-honor-PYTHONHISTORY-p]

	 ["Enforce py-shell-name" force-py-shell-name-p-on
	  :help "Enforce customized default ‘py-shell-name’ should upon execution. "]

	 ["Don't enforce default interpreter" force-py-shell-name-p-off
	  :help "Make execute commands guess interpreter from environment"]
	 )

	("Execute"

	 ["Fast process" py-fast-process-p
	  :help " ‘py-fast-process-p’

Use ‘py-fast-process’\.

Commands prefixed \"py-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Output-buffer is not in comint-mode"
	  :style toggle :selected py-fast-process-p]

	 ["Python mode v5 behavior"
	  (setq python-mode-v5-behavior-p
		(not python-mode-v5-behavior-p))
	  :help "Execute region through ‘shell-command-on-region’ as
v5 did it - lp:990079. This might fail with certain chars - see UnicodeEncodeError lp:550661

Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected python-mode-v5-behavior-p]

	 ["Force shell name "
	  (setq py-force-py-shell-name-p
		(not py-force-py-shell-name-p))
	  :help "When ‘t’, execution with kind of Python specified in ‘py-shell-name’ is enforced, possibly shebang doesn't take precedence. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-force-py-shell-name-p]

	 ["Execute \"if name == main\" blocks p"
	  (setq py-if-name-main-permission-p
		(not py-if-name-main-permission-p))
	  :help " ‘py-if-name-main-permission-p’

Allow execution of code inside blocks delimited by
if __name__ == '__main__'

Default is non-nil. "
	  :style toggle :selected py-if-name-main-permission-p]

	 ["Ask about save"
	  (setq py-ask-about-save
		(not py-ask-about-save))
	  :help "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking.Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-ask-about-save]

	 ["Store result"
	  (setq py-store-result-p
		(not py-store-result-p))
	  :help " ‘py-store-result-p’

When non-nil, put resulting string of `py-execute-...' into kill-ring, so it might be yanked. "
	  :style toggle :selected py-store-result-p]

	 ["Prompt on changed "
	  (setq py-prompt-on-changed-p
		(not py-prompt-on-changed-p))
	  :help "When called interactively, ask for save before a changed buffer is sent to interpreter.

Default is ‘t’Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-prompt-on-changed-p]

	 ["Dedicated process "
	  (setq py-dedicated-process-p
		(not py-dedicated-process-p))
	  :help "If commands executing code use a dedicated shell.

Default is nilUse `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-dedicated-process-p]

	 ["Execute without temporary file"
	  (setq py-execute-no-temp-p
		(not py-execute-no-temp-p))
	  :help " ‘py-execute-no-temp-p’
Seems Emacs-24.3 provided a way executing stuff without temporary files.
In experimental state yet "
	  :style toggle :selected py-execute-no-temp-p]

	 ["Warn tmp files left "
	  (setq py--warn-tmp-files-left-p
		(not py--warn-tmp-files-left-p))
	  :help "Messages a warning, when ‘py-temp-directory’ contains files susceptible being left by previous Python-mode sessions. See also lp:987534 Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py--warn-tmp-files-left-p])

	("Edit"

	 ("Completion"

	  ["Set Pymacs-based complete keymap "
	   (setq py-set-complete-keymap-p
		 (not py-set-complete-keymap-p))
	   :help "If ‘py-complete-initialize’, which sets up enviroment for Pymacs based py-complete, should load it's keys into ‘python-mode-map’

Default is nil.
See also resp. edit ‘py-complete-set-keymap’ Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-set-complete-keymap-p]

	  ["Indent no completion "
	   (setq py-indent-no-completion-p
		 (not py-indent-no-completion-p))
	   :help "If completion function should indent when no completion found. Default is ‘t’

Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-indent-no-completion-p]

	  ["Company pycomplete "
	   (setq py-company-pycomplete-p
		 (not py-company-pycomplete-p))
	   :help "Load company-pycomplete stuff. Default is nilUse `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-company-pycomplete-p])

	 ("Filling"

	  ("Docstring styles"
	   :help "Switch docstring-style"

	   ["Nil" py-set-nil-docstring-style
	    :help " ‘py-set-nil-docstring-style’

Set py-docstring-style to nil, format string normally. "]

	   ["pep-257-nn" py-set-pep-257-nn-docstring-style
	    :help " `py-set-pep-257-nn-docstring-style'

Set py-docstring-style to 'pep-257-nn "]

	   ["pep-257" py-set-pep-257-docstring-style
	    :help " `py-set-pep-257-docstring-style'

Set py-docstring-style to 'pep-257 "]

	   ["django" py-set-django-docstring-style
	    :help " ‘py-set-django-docstring-style’

Set py-docstring-style to 'django "]

	   ["onetwo" py-set-onetwo-docstring-style
	    :help " ‘py-set-onetwo-docstring-style’

Set py-docstring-style to 'onetwo "]

	   ["symmetric" py-set-symmetric-docstring-style
	    :help " ‘py-set-symmetric-docstring-style’

Set py-docstring-style to 'symmetric "])

	  ["Auto-fill mode"
	   (setq py-auto-fill-mode
		 (not py-auto-fill-mode))
	   :help "Fill according to ‘py-docstring-fill-column’ and ‘py-comment-fill-column’

Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-auto-fill-mode])

	 ["Use current dir when execute"
	  (setq py-use-current-dir-when-execute-p
		(not py-use-current-dir-when-execute-p))
	  :help " ‘py-toggle-use-current-dir-when-execute-p’

Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-use-current-dir-when-execute-p]

	 ("Indent"
	  ("TAB related"

	   ["indent-tabs-mode"
	    (setq indent-tabs-mode
		  (not indent-tabs-mode))
	    :help "Indentation can insert tabs if this is non-nil.

Use `M-x customize-variable' to set it permanently"
	    :style toggle :selected indent-tabs-mode]

	   ["Tab indent"
	    (setq py-tab-indent
		  (not py-tab-indent))
	    :help "Non-nil means TAB in Python mode calls ‘py-indent-line’.Use `M-x customize-variable' to set it permanently"
	    :style toggle :selected py-tab-indent]

	   ["Tab shifts region "
	    (setq py-tab-shifts-region-p
		  (not py-tab-shifts-region-p))
	    :help "If ‘t’, TAB will indent/cycle the region, not just the current line.

Default is nil
See also ‘py-tab-indents-region-p’

Use `M-x customize-variable' to set it permanently"
	    :style toggle :selected py-tab-shifts-region-p]

	   ["Tab indents region "
	    (setq py-tab-indents-region-p
		  (not py-tab-indents-region-p))
	    :help "When ‘t’ and first TAB doesn't shift, indent-region is called.

Default is nil
See also ‘py-tab-shifts-region-p’

Use `M-x customize-variable' to set it permanently"
	    :style toggle :selected py-tab-indents-region-p])

	  ["Close at start column"
	   (setq py-closing-list-dedents-bos
		 (not py-closing-list-dedents-bos))
	   :help "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = \[
    1, 2, 3,
    4, 5, 6,]

Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-closing-list-dedents-bos]

	  ["Closing list keeps space"
	   (setq py-closing-list-keeps-space
		 (not py-closing-list-keeps-space))
	   :help "If non-nil, closing parenthesis dedents onto column of opening plus ‘py-closing-list-space’, default is nil Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-closing-list-keeps-space]

	  ["Closing list space"
	   (setq py-closing-list-space
		 (not py-closing-list-space))
	   :help "Number of chars, closing parenthesis outdent from opening, default is 1 Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-closing-list-space]

	  ["Tab shifts region "
	   (setq py-tab-shifts-region-p
		 (not py-tab-shifts-region-p))
	   :help "If ‘t’, TAB will indent/cycle the region, not just the current line.

Default is nil
See also ‘py-tab-indents-region-p’Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-tab-shifts-region-p]

	  ["Lhs inbound indent"
	   (setq py-lhs-inbound-indent
		 (not py-lhs-inbound-indent))
	   :help "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-lhs-inbound-indent]

	  ["Continuation offset"
	   (setq py-continuation-offset
		 (not py-continuation-offset))
	   :help "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-continuation-offset]

	  ["Electric colon"
	   (setq py-electric-colon-active-p
		 (not py-electric-colon-active-p))
	   :help " ‘py-electric-colon-active-p’

‘py-electric-colon’ feature.  Default is ‘nil’. See lp:837065 for discussions. "
	   :style toggle :selected py-electric-colon-active-p]

	  ["Electric colon at beginning of block only"
	   (setq py-electric-colon-bobl-only
		 (not py-electric-colon-bobl-only))
	   :help "When inserting a colon, do not indent lines unless at beginning of block.

Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-electric-colon-bobl-only]

	  ["Electric yank active "
	   (setq py-electric-yank-active-p
		 (not py-electric-yank-active-p))
	   :help " When non-nil, ‘yank’ will be followed by an ‘indent-according-to-mode’.

Default is nilUse `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-electric-yank-active-p]



	  ["Trailing whitespace smart delete "
	   (setq py-trailing-whitespace-smart-delete-p
		 (not py-trailing-whitespace-smart-delete-p))
	   :help "Default is nil. When t, python-mode calls
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-trailing-whitespace-smart-delete-p]

	  ["Newline delete trailing whitespace "
	   (setq py-newline-delete-trailing-whitespace-p
		 (not py-newline-delete-trailing-whitespace-p))
	   :help "Delete trailing whitespace maybe left by ‘py-newline-and-indent’.

Default is ‘t’. See lp:1100892 Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-newline-delete-trailing-whitespace-p]

	  ["Dedent keep relative column"
	   (setq py-dedent-keep-relative-column
		 (not py-dedent-keep-relative-column))
	   :help "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-dedent-keep-relative-column]

	  ["Indent comment "
	   (setq py-indent-comments
		 (not py-indent-comments))
	   :help "If comments should be indented like code. Default is ‘nil’.

Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-indent-comments]

	  ["Uncomment indents "
	   (setq py-uncomment-indents-p
		 (not py-uncomment-indents-p))
	   :help "When non-nil, after uncomment indent lines. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-uncomment-indents-p]

	  ["Indent honors inline comment"
	   (setq py-indent-honors-inline-comment
		 (not py-indent-honors-inline-comment))
	   :help "If non-nil, indents to column of inlined comment start.
Default is nil. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-indent-honors-inline-comment]

	  ["Kill empty line"
	   (setq py-kill-empty-line
		 (not py-kill-empty-line))
	   :help "If t, py-indent-forward-line kills empty lines. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-kill-empty-line]

	  ("Smart indentation"
	   :help "Toggle py-smart-indentation'

Use `M-x customize-variable' to set it permanently"

	   ["Toggle py-smart-indentation" py-toggle-smart-indentation
	    :help "Toggles py-smart-indentation

Use `M-x customize-variable' to set it permanently"]

	   ["py-smart-indentation on" py-smart-indentation-on
	    :help "Switches py-smart-indentation on

Use `M-x customize-variable' to set it permanently"]

	   ["py-smart-indentation off" py-smart-indentation-off
	    :help "Switches py-smart-indentation off

Use `M-x customize-variable' to set it permanently"])

	  ["Beep if tab change"
	   (setq py-beep-if-tab-change
		 (not py-beep-if-tab-change))
	   :help "Ring the bell if ‘tab-width’ is changed.
If a comment of the form

                           	# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) ‘tab-width’ does not
equal <number>, ‘tab-width’ is set to <number>, a message saying so is
displayed in the echo area, and if ‘py-beep-if-tab-change’ is non-nil
the Emacs bell is also rung as a warning.Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-beep-if-tab-change]

	  ["Electric comment "
	   (setq py-electric-comment-p
		 (not py-electric-comment-p))
	   :help "If \"#\" should call ‘py-electric-comment’. Default is ‘nil’.

Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-electric-comment-p]

	  ["Electric comment add space "
	   (setq py-electric-comment-add-space-p
		 (not py-electric-comment-add-space-p))
	   :help "If py-electric-comment should add a space.  Default is ‘nil’. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-electric-comment-add-space-p]

	  ["Empty line closes "
	   (setq py-empty-line-closes-p
		 (not py-empty-line-closes-p))
	   :help "When non-nil, dedent after empty line following block

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil

If non-nil, a C-j from empty line dedents.
Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-empty-line-closes-p])
	 ["Defun use top level "
	  (setq py-defun-use-top-level-p
		(not py-defun-use-top-level-p))
	  :help "When non-nil, keys C-M-a, C-M-e address top-level form.

Beginning- end-of-defun forms use
commands ‘py-backward-top-level’, ‘py-forward-top-level’

mark-defun marks top-level form at point etc. "
	  :style toggle :selected py-defun-use-top-level-p]

	 ["Close provides newline"
	  (setq py-close-provides-newline
		(not py-close-provides-newline))
	  :help "If a newline is inserted, when line after block isn't empty. Default is non-nil. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-close-provides-newline]

	 ["Block comment prefix "
	  (setq py-block-comment-prefix-p
		(not py-block-comment-prefix-p))
	  :help "If py-comment inserts py-block-comment-prefix.

Default is tUse `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-block-comment-prefix-p])

	("Display"

	 ("Index"

	  ["Imenu create index "
	   (setq py--imenu-create-index-p
		 (not py--imenu-create-index-p))
	   :help "Non-nil means Python mode creates and displays an index menu of functions and global variables. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py--imenu-create-index-p]

	  ["Imenu show method args "
	   (setq py-imenu-show-method-args-p
		 (not py-imenu-show-method-args-p))
	   :help "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed.Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-imenu-show-method-args-p]
	  ["Switch index-function" py-switch-imenu-index-function
	   :help "‘py-switch-imenu-index-function’
Switch between ‘py--imenu-create-index’ from 5.1 series and ‘py--imenu-create-index-new’."])

	 ("Fontification"

	  ["Mark decorators"
	   (setq py-mark-decorators
		 (not py-mark-decorators))
	   :help "If py-mark-def-or-class functions should mark decorators too. Default is ‘nil’. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-mark-decorators]

	  ["Fontify shell buffer "
	   (setq py-fontify-shell-buffer-p
		 (not py-fontify-shell-buffer-p))
	   :help "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If ‘t’, related vars like ‘comment-start’ will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-fontify-shell-buffer-p]

	  ["Use font lock doc face "
	   (setq py-use-font-lock-doc-face-p
		 (not py-use-font-lock-doc-face-p))
	   :help "If documention string inside of def or class get ‘font-lock-doc-face’.

‘font-lock-doc-face’ inherits ‘font-lock-string-face’.

Call M-x ‘customize-face’ in order to have a visible effect. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-use-font-lock-doc-face-p])

	 ["Switch buffers on execute"
	  (setq py-switch-buffers-on-execute-p
		(not py-switch-buffers-on-execute-p))
	  :help "When non-nil switch to the Python output buffer.

Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-switch-buffers-on-execute-p]

	 ["Split windows on execute"
	  (setq py-split-window-on-execute
		(not py-split-window-on-execute))
	  :help "When non-nil split windows.

Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-split-window-on-execute]

	 ["Keep windows configuration"
	  (setq py-keep-windows-configuration
		(not py-keep-windows-configuration))
	  :help "If a windows is splitted displaying results, this is directed by variable ‘py-split-window-on-execute’\. Also setting ‘py-switch-buffers-on-execute-p’ affects window-configuration\. While commonly a screen splitted into source and Python-shell buffer is assumed, user may want to keep a different config\.

Setting ‘py-keep-windows-configuration’ to ‘t’ will restore windows-config regardless of settings mentioned above\. However, if an error occurs, it's displayed\.

To suppres window-changes due to error-signaling also: M-x customize-variable RET. Set `py-keep-4windows-configuration' onto 'force

Default is nil Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-keep-windows-configuration]

	 ["Which split windows on execute function"
	  (progn
	    (if (eq 'split-window-vertically py-split-windows-on-execute-function)
		(setq py-split-windows-on-execute-function'split-window-horizontally)
	      (setq py-split-windows-on-execute-function 'split-window-vertically))
	    (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function))

	  :help "If ‘split-window-vertically’ or `...-horizontally'. Use `M-x customize-variable' RET ‘py-split-windows-on-execute-function’ RET to set it permanently"
	  :style toggle :selected py-split-windows-on-execute-function]

	 ["Modeline display full path "
	  (setq py-modeline-display-full-path-p
		(not py-modeline-display-full-path-p))
	  :help "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil. Note: when ‘py-shell-name’ is specified with path, it's shown as an acronym in buffer-name already. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-modeline-display-full-path-p]

	 ["Modeline acronym display home "
	  (setq py-modeline-acronym-display-home-p
		(not py-modeline-acronym-display-home-p))
	  :help "If the modeline acronym should contain chars indicating the home-directory.

Default is nil Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-modeline-acronym-display-home-p]

	 ["Hide show hide docstrings"
	  (setq py-hide-show-hide-docstrings
		(not py-hide-show-hide-docstrings))
	  :help "Controls if doc strings can be hidden by hide-showUse `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-hide-show-hide-docstrings]

	 ["Hide comments when hiding all"
	  (setq py-hide-comments-when-hiding-all
		(not py-hide-comments-when-hiding-all))
	  :help "Hide the comments too when you do ‘hs-hide-all’. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-hide-comments-when-hiding-all]

	 ["Max help buffer "
	  (setq py-max-help-buffer-p
		(not py-max-help-buffer-p))
	  :help "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil. In help-buffer, \"q\" will close it.  Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-max-help-buffer-p]

	 ["Current defun show"
	  (setq py-current-defun-show
		(not py-current-defun-show))
	  :help "If ‘py-current-defun’ should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is ‘t’.Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-current-defun-show]

	 ["Match paren mode"
	  (setq py-match-paren-mode
		(not py-match-paren-mode))
	  :help "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets ‘py-match-paren-key’ in python-mode-map.
Customize ‘py-match-paren-key’ which key to use. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-match-paren-mode])

	("Debug"

	 ["py-debug-p"
	  (setq py-debug-p
		(not py-debug-p))
	  :help "When non-nil, keep resp\. store information useful for debugging\.

Temporary files are not deleted\. Other functions might implement
some logging etc\. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-debug-p]

	 ["Pdbtrack do tracking "
	  (setq py-pdbtrack-do-tracking-p
		(not py-pdbtrack-do-tracking-p))
	  :help "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the \*Python\* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb.Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-pdbtrack-do-tracking-p]

	 ["Jump on exception"
	  (setq py-jump-on-exception
		(not py-jump-on-exception))
	  :help "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame.

Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-jump-on-exception]

	 ["Highlight error in source "
	  (setq py-highlight-error-source-p
		(not py-highlight-error-source-p))
	  :help "Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-highlight-error-source-p])

	("Other"

	 ("Directory"

	  ["Guess install directory "
	   (setq py-guess-py-install-directory-p
		 (not py-guess-py-install-directory-p))
	   :help "If in cases, ‘py-install-directory’ isn't set,  ‘py-set-load-path’should guess it from ‘buffer-file-name’. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-guess-py-install-directory-p]

	  ["Use local default"
	   (setq py-use-local-default
		 (not py-use-local-default))
	   :help "If ‘t’, py-shell will use ‘py-shell-local-path’ instead
of default Python.

Making switch between several virtualenv's easier,
                               ‘python-mode’ should deliver an installer, so named-shells pointing to virtualenv's will be available. Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-use-local-default]

	  ["Use current dir when execute "
	   (setq py-use-current-dir-when-execute-p
		 (not py-use-current-dir-when-execute-p))
	   :help "When ‘t’, current directory is used by Python-shell for output of ‘py-execute-buffer’ and related commands.

See also ‘py-execute-directory’Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-use-current-dir-when-execute-p]

	  ["Keep shell dir when execute "
	   (setq py-keep-shell-dir-when-execute-p
		 (not py-keep-shell-dir-when-execute-p))
	   :help "Don't change Python shell's current working directory when sending code.

See also ‘py-execute-directory’Use `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-keep-shell-dir-when-execute-p]

	  ["Fileless buffer use default directory "
	   (setq py-fileless-buffer-use-default-directory-p
		 (not py-fileless-buffer-use-default-directory-p))
	   :help "When ‘py-use-current-dir-when-execute-p’ is non-nil and no buffer-file exists, value of ‘default-directory’ sets current working directory of Python output shellUse `M-x customize-variable' to set it permanently"
	   :style toggle :selected py-fileless-buffer-use-default-directory-p])

	 ("Underscore word syntax"
	  :help "Toggle ‘py-underscore-word-syntax-p’"

	  ["Toggle underscore word syntax" py-toggle-underscore-word-syntax-p
	   :help " ‘py-toggle-underscore-word-syntax-p’

If ‘py-underscore-word-syntax-p’ should be on or off.

  Returns value of ‘py-underscore-word-syntax-p’ switched to. .

Use `M-x customize-variable' to set it permanently"]

	  ["Underscore word syntax on" py-underscore-word-syntax-p-on
	   :help " ‘py-underscore-word-syntax-p-on’

Make sure, py-underscore-word-syntax-p' is on.

Returns value of ‘py-underscore-word-syntax-p’. .

Use `M-x customize-variable' to set it permanently"]

	  ["Underscore word syntax off" py-underscore-word-syntax-p-off
	   :help " ‘py-underscore-word-syntax-p-off’

Make sure, ‘py-underscore-word-syntax-p’ is off.

Returns value of ‘py-underscore-word-syntax-p’. .

Use `M-x customize-variable' to set it permanently"])

	 ["Load pymacs "
	  (setq py-load-pymacs-p
		(not py-load-pymacs-p))
	  :help "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.caUse `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-load-pymacs-p]

	 ["Verbose "
	  (setq py-verbose-p
		(not py-verbose-p))
	  :help "If functions should report results.

Default is nil. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-verbose-p]
	 ;; ["No session mode "
	 ;; 	  (setq py-no-session-p
	 ;; 		(not py-no-session-p))
	 ;; 	  :help "If shell should be in session-mode.

	 ;; Default is nil. Use `M-x customize-variable' to set it permanently"
	 ;; 	  :style toggle :selected py-no-session-p]

	 ["Empty comment line separates paragraph "
	  (setq py-empty-comment-line-separates-paragraph-p
		(not py-empty-comment-line-separates-paragraph-p))
	  :help "Consider paragraph start/end lines with nothing inside but comment sign.

Default is non-nilUse `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-empty-comment-line-separates-paragraph-p]

	 ["Org cycle "
	  (setq py-org-cycle-p
		(not py-org-cycle-p))
	  :help "When non-nil, command ‘org-cycle’ is available at shift-TAB, <backtab>

Default is nil. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-org-cycle-p]

	 ["Set pager cat"
	  (setq py-set-pager-cat-p
		(not py-set-pager-cat-p))
	  :help "If the shell environment variable \$PAGER should set to ‘cat’.

If ‘t’, use `C-c C-r' to jump to beginning of output. Then scroll normally.

Avoids lp:783828, \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module ‘os’ Use `M-x customize-variable' to
set it permanently"
	  :style toggle :selected py-set-pager-cat-p]

	 ["Edit only "
	  (setq py-edit-only-p
		(not py-edit-only-p))
	  :help "When ‘t’ ‘python-mode’ will not take resort nor check for installed Python executables. Default is nil.

See bug report at launchpad, lp:944093. Use `M-x customize-variable' to set it permanently"
	  :style toggle :selected py-edit-only-p])))
      ("Other"
       ["Boolswitch" py-boolswitch
	:help " ‘py-boolswitch’
Edit the assignment of a boolean variable, revert them."]

       ["Empty out list backward" py-empty-out-list-backward
	:help " ‘py-empty-out-list-backward’
Deletes all elements from list before point."]

       ["Kill buffer unconditional" py-kill-buffer-unconditional
	:help " ‘py-kill-buffer-unconditional’
Kill buffer unconditional, kill buffer-process if existing."]

       ["Remove overlays at point" py-remove-overlays-at-point
	:help " ‘py-remove-overlays-at-point’
Remove overlays as set when ‘py-highlight-error-source-p’ is non-nil."]
       ("Electric"
	["Complete electric comma" py-complete-electric-comma
	 :help " ‘py-complete-electric-comma’"]

	["Complete electric lparen" py-complete-electric-lparen
	 :help " ‘py-complete-electric-lparen’"]

	["Electric backspace" py-electric-backspace
	 :help " ‘py-electric-backspace’
Delete preceding character or level of indentation."]

	["Electric colon" py-electric-colon
	 :help " ‘py-electric-colon’
Insert a colon and indent accordingly."]

	["Electric comment" py-electric-comment
	 :help " ‘py-electric-comment’
Insert a comment. If starting a comment, indent accordingly."]

	["Electric delete" py-electric-delete
	 :help " ‘py-electric-delete’
Delete following character or levels of whitespace."]

	["Electric yank" py-electric-yank
	 :help " ‘py-electric-yank’
Perform command ‘yank’ followed by an ‘indent-according-to-mode’"]

	["Hungry delete backwards" py-hungry-delete-backwards
	 :help " ‘py-hungry-delete-backwards’
Delete the preceding character or all preceding whitespace"]

	["Hungry delete forward" py-hungry-delete-forward
	 :help " ‘py-hungry-delete-forward’
Delete the following character or all following whitespace"])
       ("Filling"
	["Py docstring style" py-docstring-style
	 :help " ‘py-docstring-style’"]

	["Py fill comment" py-fill-comment
	 :help " ‘py-fill-comment’"]

	["Py fill paragraph" py-fill-paragraph
	 :help " ‘py-fill-paragraph’"]

	["Py fill string" py-fill-string
	 :help " ‘py-fill-string’"]

	["Py fill string django" py-fill-string-django
	 :help " ‘py-fill-string-django’"]

	["Py fill string onetwo" py-fill-string-onetwo
	 :help " ‘py-fill-string-onetwo’"]

	["Py fill string pep 257" py-fill-string-pep-257
	 :help " `py-fill-string-pep-257'"]

	["Py fill string pep 257 nn" py-fill-string-pep-257-nn
	 :help " `py-fill-string-pep-257-nn'"]

	["Py fill string symmetric" py-fill-string-symmetric
	 :help " ‘py-fill-string-symmetric’"])
       ("Abbrevs"	   :help "see also ‘py-add-abbrev’"
	:filter (lambda (&rest junk)
		  (abbrev-table-menu python-mode-abbrev-table)))

       ["Add abbrev" py-add-abbrev
	:help " ‘py-add-abbrev’
Defines python-mode specific abbrev for last expressions before point."]
       ("Completion"
	["Py indent or complete" py-indent-or-complete
	 :help " ‘py-indent-or-complete’"]

	["Py shell complete" py-shell-complete
	 :help " ‘py-shell-complete’"]

	["Py complete" py-complete
	 :help " ‘py-complete’"])

       ["Find function" py-find-function
	:help " ‘py-find-function’
Find source of definition of SYMBOL."])))
  map)

;; python-components-map

(defvar py-use-menu-p t
  "If the menu should be loaded.

Default is t")

(defvar py-menu nil
  "Make a dynamically bound variable ‘py-menu’.")


(setq python-mode-map
      (let ((map (make-sparse-keymap)))
        ;; electric keys
        (define-key map [(:)] 'py-electric-colon)
        (define-key map [(\#)] 'py-electric-comment)
        (define-key map [(delete)] 'py-electric-delete)
        (define-key map [(backspace)] 'py-electric-backspace)
        (define-key map [(control backspace)] 'py-hungry-delete-backwards)
        (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
        ;; (define-key map [(control y)] 'py-electric-yank)
        ;; moving point
        (define-key map [(control c) (control p)] 'py-backward-statement)
        (define-key map [(control c) (control n)] 'py-forward-statement)
        (define-key map [(control c) (control u)] 'py-backward-block)
        (define-key map [(control c) (control q)] 'py-forward-block)
        (define-key map [(control meta a)] 'py-backward-def-or-class)
        (define-key map [(control meta e)] 'py-forward-def-or-class)
        ;; (define-key map [(meta i)] 'py-indent-forward-line)
        ;; (define-key map [(control j)] 'py-newline-and-indent)
	(define-key map (kbd "C-j") 'newline)
        ;; Most Pythoneers expect RET ‘py-newline-and-indent’
	;; which is default of var py-return-key’
        (define-key map (kbd "RET") py-return-key)
        ;; (define-key map (kbd "RET") 'newline)
        ;; (define-key map (kbd "RET") 'py-newline-and-dedent)
        (define-key map [(super backspace)] 'py-dedent)
        ;; (define-key map [(control return)] 'py-newline-and-dedent)
        ;; indentation level modifiers
        (define-key map [(control c) (control l)] 'py-shift-left)
        (define-key map [(control c) (control r)] 'py-shift-right)
        (define-key map [(control c) (<)] 'py-shift-left)
        (define-key map [(control c) (>)] 'py-shift-right)
        ;; (define-key map [(control c) (tab)] 'py-indent-region)
	(define-key map (kbd "C-c TAB") 'py-indent-region)
        (define-key map [(control c) (:)] 'py-guess-indent-offset)
        ;; subprocess commands
        (define-key map [(control c) (control c)] 'py-execute-buffer)
        (define-key map [(control c) (control m)] 'py-execute-import-or-reload)
        (define-key map [(control c) (control s)] 'py-execute-string)
        (define-key map [(control c) (|)] 'py-execute-region)
        (define-key map [(control meta x)] 'py-execute-def-or-class)
        (define-key map [(control c) (!)] 'py-shell)
        (define-key map [(control c) (control t)] 'py-toggle-shell)
        (define-key map [(control meta h)] 'py-mark-def-or-class)
        (define-key map [(control c) (control k)] 'py-mark-block-or-clause)
        (define-key map [(control c) (.)] 'py-expression)
        (define-key map [(control c) (?,)] 'py-partial-expression)
        ;; Miscellaneous
        ;; (define-key map [(super q)] 'py-copy-statement)
        (define-key map [(control c) (control d)] 'py-pdbtrack-toggle-stack-tracking)
        (define-key map [(control c) (control f)] 'py-sort-imports)
        (define-key map [(control c) (\#)] 'py-comment-region)
        (define-key map [(control c) (\?)] 'py-describe-mode)
        (define-key map [(control c) (control e)] 'py-help-at-point)
        (define-key map [(control c) (-)] 'py-up-exception)
        (define-key map [(control c) (=)] 'py-down-exception)
        (define-key map [(control x) (n) (d)] 'py-narrow-to-def-or-class)
        ;; information
        (define-key map [(control c) (control b)] 'py-submit-bug-report)
        (define-key map [(control c) (control v)] 'py-version)
        (define-key map [(control c) (control w)] 'py-pychecker-run)
        ;; (define-key map (kbd "TAB") 'py-indent-line)
        (define-key map (kbd "TAB") 'py-indent-or-complete)
	;; (if py-complete-function
        ;;     (progn
        ;;       (define-key map [(meta tab)] py-complete-function)
        ;;       (define-key map [(esc) (tab)] py-complete-function))
        ;;   (define-key map [(meta tab)] 'py-shell-complete)
        ;;   (define-key map [(esc) (tab)] 'py-shell-complete))
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                   map global-map)
        (substitute-key-definition 'backward-up-list 'py-up
                                   map global-map)
        (substitute-key-definition 'down-list 'py-down
                                   map global-map)
	(when py-use-menu-p
	  (setq map (py-define-menu map)))
        map))

(defvar py-python-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'comint-send-input)
    (define-key map [(control c)(-)] 'py-up-exception)
    (define-key map [(control c)(=)] 'py-down-exception)
    (define-key map (kbd "TAB") 'py-indent-or-complete)
    (define-key map [(meta tab)] 'py-shell-complete)
    (define-key map [(control c)(!)] 'py-shell)
    (define-key map [(control c)(control t)] 'py-toggle-shell)
    ;; electric keys
    ;; (define-key map [(:)] 'py-electric-colon)
    ;; (define-key map [(\#)] 'py-electric-comment)
    ;; (define-key map [(delete)] 'py-electric-delete)
    ;; (define-key map [(backspace)] 'py-electric-backspace)
    ;; (define-key map [(control backspace)] 'py-hungry-delete-backwards)
    ;; (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
    ;; (define-key map [(control y)] 'py-electric-yank)
    ;; moving point
    (define-key map [(control c)(control p)] 'py-backward-statement)
    (define-key map [(control c)(control n)] 'py-forward-statement)
    (define-key map [(control c)(control u)] 'py-backward-block)
    (define-key map [(control c)(control q)] 'py-forward-block)
    (define-key map [(control meta a)] 'py-backward-def-or-class)
    (define-key map [(control meta e)] 'py-forward-def-or-class)
    (define-key map [(control j)] 'py-newline-and-indent)
    (define-key map [(super backspace)] 'py-dedent)
    ;; (define-key map [(control return)] 'py-newline-and-dedent)
    ;; indentation level modifiers
    (define-key map [(control c)(control l)] 'comint-dynamic-list-input-ring)
    (define-key map [(control c)(control r)] 'comint-previous-prompt)
    (define-key map [(control c)(<)] 'py-shift-left)
    (define-key map [(control c)(>)] 'py-shift-right)
    (define-key map [(control c)(tab)] 'py-indent-region)
    (define-key map [(control c)(:)] 'py-guess-indent-offset)
    ;; subprocess commands
    (define-key map [(control meta h)] 'py-mark-def-or-class)
    (define-key map [(control c)(control k)] 'py-mark-block-or-clause)
    (define-key map [(control c)(.)] 'py-expression)
    ;; Miscellaneous
    ;; (define-key map [(super q)] 'py-copy-statement)
    (define-key map [(control c)(control d)] 'py-pdbtrack-toggle-stack-tracking)
    (define-key map [(control c)(\#)] 'py-comment-region)
    (define-key map [(control c)(\?)] 'py-describe-mode)
    (define-key map [(control c)(control e)] 'py-help-at-point)
    (define-key map [(control x) (n) (d)] 'py-narrow-to-def-or-class)
    ;; information
    (define-key map [(control c)(control b)] 'py-submit-bug-report)
    (define-key map [(control c)(control v)] 'py-version)
    (define-key map [(control c)(control w)] 'py-pychecker-run)
    (substitute-key-definition 'complete-symbol 'completion-at-point
			       map global-map)
    (substitute-key-definition 'backward-up-list 'py-up
			       map global-map)
    (substitute-key-definition 'down-list 'py-down
			       map global-map)
    map)
  "Used inside a Python-shell.")

(defvar py-ipython-shell-mode-map py-python-shell-mode-map
  "Copy ‘py-python-shell-mode-map’ here.")

(defvar py-shell-map py-python-shell-mode-map)

;; python-components-shell-menu

(and (ignore-errors (require 'easymenu) t)
     ;; (easy-menu-define py-menu map "Python Tools"
     ;;           `("PyTools"
     (easy-menu-define
       py-shell-menu py-python-shell-mode-map "Py-Shell Mode menu"
       `("Py-Shell"
         ("Edit"
          ("Shift"
           ("Shift right"
	    ["Shift block right" py-shift-block-right
	     :help " ‘py-shift-block-right’
Indent block by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift block or clause right" py-shift-block-or-clause-right
	     :help " ‘py-shift-block-or-clause-right’
Indent block-or-clause by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift class right" py-shift-class-right
	     :help " ‘py-shift-class-right’
Indent class by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift clause right" py-shift-clause-right
	     :help " ‘py-shift-clause-right’
Indent clause by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift comment right" py-shift-comment-right
	     :help " ‘py-shift-comment-right’"]

	    ["Shift def right" py-shift-def-right
	     :help " ‘py-shift-def-right’
Indent def by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift def or class right" py-shift-def-or-class-right
	     :help " ‘py-shift-def-or-class-right’
Indent def-or-class by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift minor block right" py-shift-minor-block-right
	     :help " ‘py-shift-minor-block-right’
Indent minor-block by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a ‘for’, ‘if’, ‘try’ or ‘with’."]

	    ["Shift paragraph right" py-shift-paragraph-right
	     :help " ‘py-shift-paragraph-right’
Indent paragraph by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift region right" py-shift-region-right
	     :help " ‘py-shift-region-right’
Indent region according to ‘py-indent-offset’ by COUNT times.

If no region is active, current line is indented.
Returns indentation reached."]

	    ["Shift statement right" py-shift-statement-right
	     :help " ‘py-shift-statement-right’
Indent statement by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift top level right" py-shift-top-level-right
	     :help " ‘py-shift-top-level-right’"]
            )
           ("Shift left"
	    ["Shift block left" py-shift-block-left
	     :help " ‘py-shift-block-left’
Dedent block by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift block or clause left" py-shift-block-or-clause-left
	     :help " ‘py-shift-block-or-clause-left’
Dedent block-or-clause by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift class left" py-shift-class-left
	     :help " ‘py-shift-class-left’
Dedent class by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift clause left" py-shift-clause-left
	     :help " ‘py-shift-clause-left’
Dedent clause by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift comment left" py-shift-comment-left
	     :help " ‘py-shift-comment-left’"]

	    ["Shift def left" py-shift-def-left
	     :help " ‘py-shift-def-left’
Dedent def by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift def or class left" py-shift-def-or-class-left
	     :help " ‘py-shift-def-or-class-left’
Dedent def-or-class by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift minor block left" py-shift-minor-block-left
	     :help " ‘py-shift-minor-block-left’
Dedent minor-block by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a ‘for’, ‘if’, ‘try’ or ‘with’."]

	    ["Shift paragraph left" py-shift-paragraph-left
	     :help " ‘py-shift-paragraph-left’
Dedent paragraph by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]

	    ["Shift region left" py-shift-region-left
	     :help " ‘py-shift-region-left’
Dedent region according to ‘py-indent-offset’ by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached."]

	    ["Shift statement left" py-shift-statement-left
	     :help " ‘py-shift-statement-left’
Dedent statement by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use [universal-argument] to specify a different value.

Returns outmost indentation reached."]
            ))
          ("Mark"
	   ["Mark block" py-mark-block
	    :help " ‘py-mark-block’
Mark block at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark block or clause" py-mark-block-or-clause
	    :help " ‘py-mark-block-or-clause’
Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark class" py-mark-class
	    :help " ‘py-mark-class’
Mark class at point.

With C-u or ‘py-mark-decorators’ set to ‘t’, decorators are marked too.
Returns beginning and end positions of marked area, a cons."]

	   ["Mark clause" py-mark-clause
	    :help " ‘py-mark-clause’
Mark clause at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark comment" py-mark-comment
	    :help " ‘py-mark-comment’
Mark comment at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark def" py-mark-def
	    :help " ‘py-mark-def’
Mark def at point.

With C-u or ‘py-mark-decorators’ set to ‘t’, decorators are marked too.
Returns beginning and end positions of marked area, a cons."]

	   ["Mark def or class" py-mark-def-or-class
	    :help " ‘py-mark-def-or-class’
Mark def-or-class at point.

With C-u or ‘py-mark-decorators’ set to ‘t’, decorators are marked too.
Returns beginning and end positions of marked area, a cons."]

	   ["Mark expression" py-mark-expression
	    :help " ‘py-mark-expression’
Mark expression at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark line" py-mark-line
	    :help " ‘py-mark-line’
Mark line at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark minor block" py-mark-minor-block
	    :help " ‘py-mark-minor-block’
Mark minor-block at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark paragraph" py-mark-paragraph
	    :help " ‘py-mark-paragraph’
Mark paragraph at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark partial expression" py-mark-partial-expression
	    :help " ‘py-mark-partial-expression’
Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark statement" py-mark-statement
	    :help " ‘py-mark-statement’
Mark statement at point.

Returns beginning and end positions of marked area, a cons."]

	   ["Mark top level" py-mark-top-level
	    :help " ‘py-mark-top-level’
Mark top-level at point.

Returns beginning and end positions of marked area, a cons."]
           )
          ("Copy"
	   ["Copy block" py-copy-block
	    :help " ‘py-copy-block’
Copy block at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy block or clause" py-copy-block-or-clause
	    :help " ‘py-copy-block-or-clause’
Copy block-or-clause at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy class" py-copy-class
	    :help " ‘py-copy-class’
Copy class at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy clause" py-copy-clause
	    :help " ‘py-copy-clause’
Copy clause at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy comment" py-copy-comment
	    :help " ‘py-copy-comment’"]

	   ["Copy def" py-copy-def
	    :help " ‘py-copy-def’
Copy def at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy def or class" py-copy-def-or-class
	    :help " ‘py-copy-def-or-class’
Copy def-or-class at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy expression" py-copy-expression
	    :help " ‘py-copy-expression’
Copy expression at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy line" py-copy-line
	    :help " ‘py-copy-line’"]

	   ["Copy minor block" py-copy-minor-block
	    :help " ‘py-copy-minor-block’
Copy minor-block at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy paragraph" py-copy-paragraph
	    :help " ‘py-copy-paragraph’"]

	   ["Copy partial expression" py-copy-partial-expression
	    :help " ‘py-copy-partial-expression’
Copy partial-expression at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy statement" py-copy-statement
	    :help " ‘py-copy-statement’
Copy statement at point.

Store data in kill ring, so it might yanked back."]

	   ["Copy top level" py-copy-top-level
	    :help " ‘py-copy-top-level’
Copy top-level at point.

Store data in kill ring, so it might yanked back."]
           )
          ("Kill"
	   ["Kill block" py-kill-block
	    :help " ‘py-kill-block’
Delete ‘block’ at point.

Stores data in kill ring"]

	   ["Kill block or clause" py-kill-block-or-clause
	    :help " ‘py-kill-block-or-clause’
Delete ‘block-or-clause’ at point.

Stores data in kill ring"]

	   ["Kill class" py-kill-class
	    :help " ‘py-kill-class’
Delete ‘class’ at point.

Stores data in kill ring"]

	   ["Kill clause" py-kill-clause
	    :help " ‘py-kill-clause’
Delete ‘clause’ at point.

Stores data in kill ring"]

	   ["Kill comment" py-kill-comment
	    :help " ‘py-kill-comment’"]

	   ["Kill def" py-kill-def
	    :help " ‘py-kill-def’
Delete ‘def’ at point.

Stores data in kill ring"]

	   ["Kill def or class" py-kill-def-or-class
	    :help " ‘py-kill-def-or-class’
Delete ‘def-or-class’ at point.

Stores data in kill ring"]

	   ["Kill expression" py-kill-expression
	    :help " ‘py-kill-expression’
Delete ‘expression’ at point.

Stores data in kill ring"]

	   ["Kill line" py-kill-line
	    :help " ‘py-kill-line’"]

	   ["Kill minor block" py-kill-minor-block
	    :help " ‘py-kill-minor-block’
Delete ‘minor-block’ at point.

Stores data in kill ring"]

	   ["Kill paragraph" py-kill-paragraph
	    :help " ‘py-kill-paragraph’"]

	   ["Kill partial expression" py-kill-partial-expression
	    :help " ‘py-kill-partial-expression’
Delete ‘partial-expression’ at point.

Stores data in kill ring"]

	   ["Kill statement" py-kill-statement
	    :help " ‘py-kill-statement’
Delete ‘statement’ at point.

Stores data in kill ring"]

	   ["Kill top level" py-kill-top-level
	    :help " ‘py-kill-top-level’
Delete ‘top-level’ at point.

Stores data in kill ring"]
           )
          ("Delete"
	   ["Delete block" py-delete-block
	    :help " ‘py-delete-block’
Delete BLOCK at point.

Don't store data in kill ring."]

	   ["Delete block or clause" py-delete-block-or-clause
	    :help " ‘py-delete-block-or-clause’
Delete BLOCK-OR-CLAUSE at point.

Don't store data in kill ring."]

	   ["Delete class" py-delete-class
	    :help " ‘py-delete-class’
Delete CLASS at point.

Don't store data in kill ring.
With C-u or ‘py-mark-decorators’ set to ‘t’, ‘decorators’ are included."]

	   ["Delete clause" py-delete-clause
	    :help " ‘py-delete-clause’
Delete CLAUSE at point.

Don't store data in kill ring."]

	   ["Delete comment" py-delete-comment
	    :help " ‘py-delete-comment’"]

	   ["Delete def" py-delete-def
	    :help " ‘py-delete-def’
Delete DEF at point.

Don't store data in kill ring.
With C-u or ‘py-mark-decorators’ set to ‘t’, ‘decorators’ are included."]

	   ["Delete def or class" py-delete-def-or-class
	    :help " ‘py-delete-def-or-class’
Delete DEF-OR-CLASS at point.

Don't store data in kill ring.
With C-u or ‘py-mark-decorators’ set to ‘t’, ‘decorators’ are included."]

	   ["Delete expression" py-delete-expression
	    :help " ‘py-delete-expression’
Delete EXPRESSION at point.

Don't store data in kill ring."]

	   ["Delete line" py-delete-line
	    :help " ‘py-delete-line’"]

	   ["Delete minor block" py-delete-minor-block
	    :help " ‘py-delete-minor-block’
Delete MINOR-BLOCK at point.

Don't store data in kill ring."]

	   ["Delete paragraph" py-delete-paragraph
	    :help " ‘py-delete-paragraph’"]

	   ["Delete partial expression" py-delete-partial-expression
	    :help " ‘py-delete-partial-expression’
Delete PARTIAL-EXPRESSION at point.

Don't store data in kill ring."]

	   ["Delete statement" py-delete-statement
	    :help " ‘py-delete-statement’
Delete STATEMENT at point.

Don't store data in kill ring."]

	   ["Delete top level" py-delete-top-level
	    :help " ‘py-delete-top-level’
Delete TOP-LEVEL at point.

Don't store data in kill ring."]
           )
          ("Comment"
	   ["Comment block" py-comment-block
	    :help " ‘py-comment-block’
Comments block at point.

Uses double hash (`#') comment starter when ‘py-block-comment-prefix-p’ is  ‘t’,
the default"]

	   ["Comment block or clause" py-comment-block-or-clause
	    :help " ‘py-comment-block-or-clause’
Comments block-or-clause at point.

Uses double hash (`#') comment starter when ‘py-block-comment-prefix-p’ is  ‘t’,
the default"]

	   ["Comment class" py-comment-class
	    :help " ‘py-comment-class’
Comments class at point.

Uses double hash (`#') comment starter when ‘py-block-comment-prefix-p’ is  ‘t’,
the default"]

	   ["Comment clause" py-comment-clause
	    :help " ‘py-comment-clause’
Comments clause at point.

Uses double hash (`#') comment starter when ‘py-block-comment-prefix-p’ is  ‘t’,
the default"]

	   ["Comment def" py-comment-def
	    :help " ‘py-comment-def’
Comments def at point.

Uses double hash (`#') comment starter when ‘py-block-comment-prefix-p’ is  ‘t’,
the default"]

	   ["Comment def or class" py-comment-def-or-class
	    :help " ‘py-comment-def-or-class’
Comments def-or-class at point.

Uses double hash (`#') comment starter when ‘py-block-comment-prefix-p’ is  ‘t’,
the default"]

	   ["Comment statement" py-comment-statement
	    :help " ‘py-comment-statement’
Comments statement at point.

Uses double hash (`#') comment starter when ‘py-block-comment-prefix-p’ is  ‘t’,
the default"]
           ))
         ("Move"
          ("Backward"
	   ["Beginning of block" py-beginning-of-block
	    :help " ‘py-beginning-of-block’
Go to beginning block, skip whitespace at BOL.

Returns beginning of block if successful, nil otherwise"]

	   ["Beginning of block or clause" py-beginning-of-block-or-clause
	    :help " ‘py-beginning-of-block-or-clause’
Go to beginning block-or-clause, skip whitespace at BOL.

Returns beginning of block-or-clause if successful, nil otherwise"]

	   ["Beginning of class" py-beginning-of-class
	    :help " ‘py-beginning-of-class’
Go to beginning class, skip whitespace at BOL.

Returns beginning of class if successful, nil otherwise

When ‘py-mark-decorators’ is non-nil, decorators are considered too."]

	   ["Beginning of clause" py-beginning-of-clause
	    :help " ‘py-beginning-of-clause’
Go to beginning clause, skip whitespace at BOL.

Returns beginning of clause if successful, nil otherwise"]

	   ["Beginning of def" py-beginning-of-def
	    :help " ‘py-beginning-of-def’
Go to beginning def, skip whitespace at BOL.

Returns beginning of def if successful, nil otherwise

When ‘py-mark-decorators’ is non-nil, decorators are considered too."]

	   ["Beginning of def or class" py-backward-def-or-class
	    :help " ‘py-backward-def-or-class’
Go to beginning def-or-class, skip whitespace at BOL.

Returns beginning of def-or-class if successful, nil otherwise

When ‘py-mark-decorators’ is non-nil, decorators are considered too."]

	   ["Beginning of elif block" py-beginning-of-elif-block
	    :help " ‘py-beginning-of-elif-block’
Go to beginning elif-block, skip whitespace at BOL.

Returns beginning of elif-block if successful, nil otherwise"]

	   ["Beginning of else block" py-beginning-of-else-block
	    :help " ‘py-beginning-of-else-block’
Go to beginning else-block, skip whitespace at BOL.

Returns beginning of else-block if successful, nil otherwise"]

	   ["Beginning of except block" py-beginning-of-except-block
	    :help " ‘py-beginning-of-except-block’
Go to beginning except-block, skip whitespace at BOL.

Returns beginning of except-block if successful, nil otherwise"]

	   ["Beginning of expression" py-beginning-of-expression
	    :help " ‘py-beginning-of-expression’
Go to the beginning of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

If already at the beginning or before a expression, go to next expression in buffer upwards

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

	   ["Beginning of if block" py-beginning-of-if-block
	    :help " ‘py-beginning-of-if-block’
Go to beginning if-block, skip whitespace at BOL.

Returns beginning of if-block if successful, nil otherwise"]

	   ["Beginning of partial expression" py-backward-partial-expression
	    :help " ‘py-backward-partial-expression’"]

	   ["Beginning of statement" py-backward-statement
	    :help " ‘py-backward-statement’
Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause."]

	   ["Beginning of top level" py-backward-top-level
	    :help " ‘py-backward-top-level’
Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise"]

	   ["Beginning of try block" py-beginning-of-try-block
	    :help " ‘py-beginning-of-try-block’
Go to beginning try-block, skip whitespace at BOL.

Returns beginning of try-block if successful, nil otherwise"]
           )
          ("Forward"
	   ["End of block" py-forward-block
	    :help " ‘py-forward-block’
Go to end of block.

Returns end of block if successful, nil otherwise"]

	   ["End of block or clause" py-forward-block-or-clause
	    :help " ‘py-forward-block-or-clause’
Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise"]

	   ["End of class" py-forward-class
	    :help " ‘py-forward-class’
Go to end of class.

Returns end of class if successful, nil otherwise"]

	   ["End of clause" py-forward-clause
	    :help " ‘py-forward-clause’
Go to end of clause.

Returns end of clause if successful, nil otherwise"]

	   ["End of def" py-forward-def
	    :help " ‘py-forward-def’
Go to end of def.

Returns end of def if successful, nil otherwise"]

	   ["End of def or class" py-forward-def-or-class
	    :help " ‘py-forward-def-or-class’
Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise"]

	   ["End of elif block" py-forward-elif-block
	    :help " ‘py-forward-elif-block’
Go to end of elif-block.

Returns end of elif-block if successful, nil otherwise"]

	   ["End of else block" py-forward-else-block
	    :help " ‘py-forward-else-block’
Go to end of else-block.

Returns end of else-block if successful, nil otherwise"]

	   ["End of except block" py-forward-except-block
	    :help " ‘py-forward-except-block’
Go to end of except-block.

Returns end of except-block if successful, nil otherwise"]

	   ["End of expression" py-forward-expression
	    :help " ‘py-forward-expression’
Go to the end of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference

Operators however are left aside resp. limit py-expression designed for edit-purposes."]

	   ["End of if block" py-forward-if-block
	    :help " ‘py-forward-if-block’
Go to end of if-block.

Returns end of if-block if successful, nil otherwise"]

	   ["End of partial expression" py-forward-partial-expression
	    :help " ‘py-forward-partial-expression’"]

	   ["End of statement" py-forward-statement
	    :help " ‘py-forward-statement’
Go to the last char of current statement.

Optional argument REPEAT, the number of loops done already, is checked for py-max-specpdl-size error. Avoid eternal loops due to missing string delimters etc."]

	   ["End of top level" py-forward-top-level
	    :help " ‘py-forward-top-level’
Go to end of top-level form at point.

Returns position if successful, nil otherwise"]

	   ["End of try block" py-forward-try-block
	    :help " ‘py-forward-try-block’
Go to end of try-block.

Returns end of try-block if successful, nil otherwise"]
           )
          ("BOL-forms"
           ("Backward"
	    ["Beginning of block bol" py-beginning-of-block-bol
	     :help " ‘py-beginning-of-block-bol’
Go to beginning block, go to BOL.

Returns beginning of block if successful, nil otherwise"]

	    ["Beginning of block or clause bol" py-beginning-of-block-or-clause-bol
	     :help " ‘py-beginning-of-block-or-clause-bol’
Go to beginning block-or-clause, go to BOL.

Returns beginning of block-or-clause if successful, nil otherwise"]

	    ["Beginning of class bol" py-beginning-of-class-bol
	     :help " ‘py-beginning-of-class-bol’
Go to beginning class, go to BOL.

Returns beginning of class if successful, nil otherwise

When ‘py-mark-decorators’ is non-nil, decorators are considered too."]

	    ["Beginning of clause bol" py-beginning-of-clause-bol
	     :help " ‘py-beginning-of-clause-bol’
Go to beginning clause, go to BOL.

Returns beginning of clause if successful, nil otherwise"]

	    ["Beginning of def bol" py-beginning-of-def-bol
	     :help " ‘py-beginning-of-def-bol’
Go to beginning def, go to BOL.

Returns beginning of def if successful, nil otherwise

When ‘py-mark-decorators’ is non-nil, decorators are considered too."]

	    ["Beginning of def or class bol" py-backward-def-or-class-bol
	     :help " ‘py-backward-def-or-class-bol’
Go to beginning def-or-class, go to BOL.

Returns beginning of def-or-class if successful, nil otherwise

When ‘py-mark-decorators’ is non-nil, decorators are considered too."]

	    ["Beginning of elif block bol" py-beginning-of-elif-block-bol
	     :help " ‘py-beginning-of-elif-block-bol’
Go to beginning elif-block, go to BOL.

Returns beginning of elif-block if successful, nil otherwise"]

	    ["Beginning of else block bol" py-beginning-of-else-block-bol
	     :help " ‘py-beginning-of-else-block-bol’
Go to beginning else-block, go to BOL.

Returns beginning of else-block if successful, nil otherwise"]

	    ["Beginning of except block bol" py-beginning-of-except-block-bol
	     :help " ‘py-beginning-of-except-block-bol’
Go to beginning except-block, go to BOL.

Returns beginning of except-block if successful, nil otherwise"]

	    ["Beginning of expression bol" py-beginning-of-expression-bol
	     :help " ‘py-beginning-of-expression-bol’"]

	    ["Beginning of if block bol" py-beginning-of-if-block-bol
	     :help " ‘py-beginning-of-if-block-bol’
Go to beginning if-block, go to BOL.

Returns beginning of if-block if successful, nil otherwise"]

	    ["Beginning of partial expression bol" py-backward-partial-expression-bol
	     :help " ‘py-backward-partial-expression-bol’"]

	    ["Beginning of statement bol" py-backward-statement-bol
	     :help " ‘py-backward-statement-bol’
Goto beginning of line where statement starts.
  Returns position reached, if successful, nil otherwise.

See also ‘py-up-statement’: up from current definition to next beginning of statement above."]

	    ["Beginning of try block bol" py-beginning-of-try-block-bol
	     :help " ‘py-beginning-of-try-block-bol’
Go to beginning try-block, go to BOL.

Returns beginning of try-block if successful, nil otherwise"]
            )
           ("Forward"
	    ["End of block bol" py-forward-block-bol
	     :help " ‘py-forward-block-bol’
Goto beginning of line following end of block.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-block’: down from current definition to next beginning of block below."]

	    ["End of block or clause bol" py-forward-block-or-clause-bol
	     :help " ‘py-forward-block-or-clause-bol’
Goto beginning of line following end of block-or-clause.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-block-or-clause’: down from current definition to next beginning of block-or-clause below."]

	    ["End of class bol" py-forward-class-bol
	     :help " ‘py-forward-class-bol’
Goto beginning of line following end of class.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-class’: down from current definition to next beginning of class below."]

	    ["End of clause bol" py-forward-clause-bol
	     :help " ‘py-forward-clause-bol’
Goto beginning of line following end of clause.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-clause’: down from current definition to next beginning of clause below."]

	    ["End of def bol" py-forward-def-bol
	     :help " ‘py-forward-def-bol’
Goto beginning of line following end of def.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-def’: down from current definition to next beginning of def below."]

	    ["End of def or class bol" py-forward-def-or-class-bol
	     :help " ‘py-forward-def-or-class-bol’
Goto beginning of line following end of def-or-class.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-def-or-class’: down from current definition to next beginning of def-or-class below."]

	    ["End of elif block bol" py-forward-elif-block-bol
	     :help " ‘py-forward-elif-block-bol’
Goto beginning of line following end of elif-block.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-elif-block’: down from current definition to next beginning of elif-block below."]

	    ["End of else block bol" py-forward-else-block-bol
	     :help " ‘py-forward-else-block-bol’
Goto beginning of line following end of else-block.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-else-block’: down from current definition to next beginning of else-block below."]

	    ["End of except block bol" py-forward-except-block-bol
	     :help " ‘py-forward-except-block-bol’
Goto beginning of line following end of except-block.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-except-block’: down from current definition to next beginning of except-block below."]

	    ["End of expression bol" py-forward-expression-bol
	     :help " ‘py-forward-expression-bol’"]

	    ["End of if block bol" py-forward-if-block-bol
	     :help " ‘py-forward-if-block-bol’
Goto beginning of line following end of if-block.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-if-block’: down from current definition to next beginning of if-block below."]

	    ["End of partial expression bol" py-forward-partial-expression-bol
	     :help " ‘py-forward-partial-expression-bol’"]

	    ["End of statement bol" py-forward-statement-bol
	     :help " ‘py-forward-statement-bol’
Go to the beginning-of-line following current statement."]

	    ["End of top level bol" py-forward-top-level-bol
	     :help " ‘py-forward-top-level-bol’
Go to end of top-level form at point, stop at next beginning-of-line.

Returns position successful, nil otherwise"]

	    ["End of try block bol" py-forward-try-block-bol
	     :help " ‘py-forward-try-block-bol’
Goto beginning of line following end of try-block.
  Returns position reached, if successful, nil otherwise.

See also ‘py-down-try-block’: down from current definition to next beginning of try-block below."]
            ))
          ("Up/Down"
	   ["Up" py-up
	    :help " ‘py-up’
Go up or to beginning of form if inside.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to beginning one level above of compound statement or definition at point."]

	   ["Down" py-down
	    :help " ‘py-down’
Go to beginning one level below of compound statement or definition at point.

If no statement or block below, but a delimited form --string or list-- go to its beginning. Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise"]
           ))
         ("Hide-Show"
          ("Hide"
	   ["Hide region" py-hide-region
	    :help " ‘py-hide-region’
Hide active region."]

	   ["Hide statement" py-hide-statement
	    :help " ‘py-hide-statement’
Hide statement at point."]

	   ["Hide block" py-hide-block
	    :help " ‘py-hide-block’
Hide block at point."]

	   ["Hide clause" py-hide-clause
	    :help " ‘py-hide-clause’
Hide clause at point."]

	   ["Hide block or clause" py-hide-block-or-clause
	    :help " ‘py-hide-block-or-clause’
Hide block-or-clause at point."]

	   ["Hide def" py-hide-def
	    :help " ‘py-hide-def’
Hide def at point."]

	   ["Hide class" py-hide-class
	    :help " ‘py-hide-class’
Hide class at point."]

	   ["Hide expression" py-hide-expression
	    :help " ‘py-hide-expression’
Hide expression at point."]

	   ["Hide partial expression" py-hide-partial-expression
	    :help " ‘py-hide-partial-expression’
Hide partial-expression at point."]

	   ["Hide line" py-hide-line
	    :help " ‘py-hide-line’
Hide line at point."]

	   ["Hide top level" py-hide-top-level
	    :help " ‘py-hide-top-level’
Hide top-level at point."]
           )
          ("Show"
	   ["Show" py-show
	    :help " ‘py-show’
Un-hide at point."]

	   ["Show all" py-show-all
	    :help " ‘py-show-all’
Un-hide all in buffer."]
           ))
         ("Virtualenv"
          ["Virtualenv activate" virtualenv-activate
	   :help " ‘virtualenv-activate’
Activate the virtualenv located in DIR"]

          ["Virtualenv deactivate" virtualenv-deactivate
	   :help " ‘virtualenv-deactivate’
Deactivate the current virtual enviroment"]

          ["Virtualenv p" virtualenv-p
	   :help " ‘virtualenv-p’
Check if a directory is a virtualenv"]

          ["Virtualenv workon" virtualenv-workon
	   :help " ‘virtualenv-workon’
Issue a virtualenvwrapper-like virtualenv-workon command"]
          )
         ("Help"
          ["Find definition" py-find-definition
	   :help " ‘py-find-definition’
Find source of definition of SYMBOL.

Interactively, prompt for SYMBOL."]

          ["Help at point" py-help-at-point
	   :help " ‘py-help-at-point’
Print help on symbol at point.

If symbol is defined in current buffer, jump to it's definition
Optional C-u used for debugging, will prevent deletion of temp file."]

          ["Info lookup symbol" py-info-lookup-symbol
	   :help " ‘py-info-lookup-symbol’"]

          ["Symbol at point" py-symbol-at-point
	   :help " ‘py-symbol-at-point’
Return the current Python symbol."]
          )
         ("Customize"

	  ["Python-mode customize group" (customize-group 'python-mode)
	   :help "Open the customization buffer for Python mode"]
	  ("Switches"
	   :help "Toggle useful modes"
	   ("Interpreter"

	    ["Shell prompt read only"
	     (setq py-shell-prompt-read-only
		   (not py-shell-prompt-read-only))
	     :help "If non-nil, the python prompt is read only.  Setting this variable will only effect new shells.Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-shell-prompt-read-only]

	    ["Remove cwd from path"
	     (setq py-remove-cwd-from-path
		   (not py-remove-cwd-from-path))
	     :help "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion).Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-remove-cwd-from-path]

	    ["Honor IPYTHONDIR "
	     (setq py-honor-IPYTHONDIR-p
		   (not py-honor-IPYTHONDIR-p))
	     :help "When non-nil ipython-history file is constructed by \$IPYTHONDIR
followed by "/history". Default is nil.

Otherwise value of py-ipython-history is used. Use `M-x customize-variable' to set it permanently"
:style toggle :selected py-honor-IPYTHONDIR-p]

	    ["Honor PYTHONHISTORY "
	     (setq py-honor-PYTHONHISTORY-p
		   (not py-honor-PYTHONHISTORY-p))
	     :help "When non-nil python-history file is set by \$PYTHONHISTORY
Default is nil.

Otherwise value of py-python-history is used. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-honor-PYTHONHISTORY-p]

	    ["Enforce py-shell-name" force-py-shell-name-p-on
	     :help "Enforce customized default ‘py-shell-name’ should upon execution. "]

	    ["Don't enforce default interpreter" force-py-shell-name-p-off
	     :help "Make execute commands guess interpreter from environment"]

	    )

	   ("Execute"

	    ["Fast process" py-fast-process-p
	     :help " ‘py-fast-process-p’

Use ‘py-fast-process’\.

Commands prefixed \"py-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Output-buffer is not in comint-mode"
	     :style toggle :selected py-fast-process-p]

	    ["Python mode v5 behavior"
	     (setq python-mode-v5-behavior-p
		   (not python-mode-v5-behavior-p))
	     :help "Execute region through ‘shell-command-on-region’ as
v5 did it - lp:990079. This might fail with certain chars - see UnicodeEncodeError lp:550661

Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected python-mode-v5-behavior-p]

	    ["Force shell name "
	     (setq py-force-py-shell-name-p
		   (not py-force-py-shell-name-p))
	     :help "When ‘t’, execution with kind of Python specified in ‘py-shell-name’ is enforced, possibly shebang doesn't take precedence. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-force-py-shell-name-p]

	    ["Execute \"if name == main\" blocks p"
	     (setq py-if-name-main-permission-p
		   (not py-if-name-main-permission-p))
	     :help " ‘py-if-name-main-permission-p’

Allow execution of code inside blocks delimited by
if __name__ == '__main__'

Default is non-nil. "
	     :style toggle :selected py-if-name-main-permission-p]

	    ["Ask about save"
	     (setq py-ask-about-save
		   (not py-ask-about-save))
	     :help "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking.Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-ask-about-save]

	    ["Store result"
	     (setq py-store-result-p
		   (not py-store-result-p))
	     :help " ‘py-store-result-p’

When non-nil, put resulting string of `py-execute-...' into kill-ring, so it might be yanked. "
	     :style toggle :selected py-store-result-p]

	    ["Prompt on changed "
	     (setq py-prompt-on-changed-p
		   (not py-prompt-on-changed-p))
	     :help "When called interactively, ask for save before a changed buffer is sent to interpreter.

Default is ‘t’Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-prompt-on-changed-p]

	    ["Dedicated process "
	     (setq py-dedicated-process-p
		   (not py-dedicated-process-p))
	     :help "If commands executing code use a dedicated shell.

Default is nilUse `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-dedicated-process-p]

	    ["Execute without temporary file"
	     (setq py-execute-no-temp-p
		   (not py-execute-no-temp-p))
	     :help " ‘py-execute-no-temp-p’
Seems Emacs-24.3 provided a way executing stuff without temporary files.
In experimental state yet "
	     :style toggle :selected py-execute-no-temp-p]

	    ["Warn tmp files left "
	     (setq py--warn-tmp-files-left-p
		   (not py--warn-tmp-files-left-p))
	     :help "Messages a warning, when ‘py-temp-directory’ contains files susceptible being left by previous Python-mode sessions. See also lp:987534 Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py--warn-tmp-files-left-p])

	   ("Edit"

	    ("Completion"

	     ["Set Pymacs-based complete keymap "
	      (setq py-set-complete-keymap-p
		    (not py-set-complete-keymap-p))
	      :help "If ‘py-complete-initialize’, which sets up enviroment for Pymacs based py-complete, should load it's keys into ‘python-mode-map’

Default is nil.
See also resp. edit ‘py-complete-set-keymap’ Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-set-complete-keymap-p]

	     ["Indent no completion "
	      (setq py-indent-no-completion-p
		    (not py-indent-no-completion-p))
	      :help "If completion function should indent when no completion found. Default is ‘t’

Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-indent-no-completion-p]

	     ["Company pycomplete "
	      (setq py-company-pycomplete-p
		    (not py-company-pycomplete-p))
	      :help "Load company-pycomplete stuff. Default is nilUse `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-company-pycomplete-p])

	    ("Filling"

	     ("Docstring styles"
	      :help "Switch docstring-style"

	      ["Nil" py-set-nil-docstring-style
	       :help " ‘py-set-nil-docstring-style’

Set py-docstring-style to nil, format string normally. "]

	      ["pep-257-nn" py-set-pep-257-nn-docstring-style
	       :help " `py-set-pep-257-nn-docstring-style'

Set py-docstring-style to 'pep-257-nn "]

	      ["pep-257" py-set-pep-257-docstring-style
	       :help " `py-set-pep-257-docstring-style'

Set py-docstring-style to 'pep-257 "]

	      ["django" py-set-django-docstring-style
	       :help " ‘py-set-django-docstring-style’

Set py-docstring-style to 'django "]

	      ["onetwo" py-set-onetwo-docstring-style
	       :help " ‘py-set-onetwo-docstring-style’

Set py-docstring-style to 'onetwo "]

	      ["symmetric" py-set-symmetric-docstring-style
	       :help " ‘py-set-symmetric-docstring-style’

Set py-docstring-style to 'symmetric "])

	     ["Auto-fill mode"
	      (setq py-auto-fill-mode
		    (not py-auto-fill-mode))
	      :help "Fill according to ‘py-docstring-fill-column’ and ‘py-comment-fill-column’

Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-auto-fill-mode])

	    ["Use current dir when execute"
	     (setq py-use-current-dir-when-execute-p
		   (not py-use-current-dir-when-execute-p))
	     :help " ‘py-toggle-use-current-dir-when-execute-p’

Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-use-current-dir-when-execute-p]

	    ("Indent"
	     ("TAB related"

	      ["indent-tabs-mode"
	       (setq indent-tabs-mode
		     (not indent-tabs-mode))
	       :help "Indentation can insert tabs if this is non-nil.

Use `M-x customize-variable' to set it permanently"
	       :style toggle :selected indent-tabs-mode]

	      ["Tab indent"
	       (setq py-tab-indent
		     (not py-tab-indent))
	       :help "Non-nil means TAB in Python mode calls ‘py-indent-line’.Use `M-x customize-variable' to set it permanently"
	       :style toggle :selected py-tab-indent]

	      ["Tab shifts region "
	       (setq py-tab-shifts-region-p
		     (not py-tab-shifts-region-p))
	       :help "If ‘t’, TAB will indent/cycle the region, not just the current line.

Default is nil
See also ‘py-tab-indents-region-p’

Use `M-x customize-variable' to set it permanently"
	       :style toggle :selected py-tab-shifts-region-p]

	      ["Tab indents region "
	       (setq py-tab-indents-region-p
		     (not py-tab-indents-region-p))
	       :help "When ‘t’ and first TAB doesn't shift, indent-region is called.

Default is nil
See also ‘py-tab-shifts-region-p’

Use `M-x customize-variable' to set it permanently"
	       :style toggle :selected py-tab-indents-region-p])

	     ["Close at start column"
	      (setq py-closing-list-dedents-bos
		    (not py-closing-list-dedents-bos))
	      :help "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = \[
    1, 2, 3,
    4, 5, 6,
]

Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-closing-list-dedents-bos]

	     ["Closing list keeps space"
	      (setq py-closing-list-keeps-space
		    (not py-closing-list-keeps-space))
	      :help "If non-nil, closing parenthesis dedents onto column of opening plus ‘py-closing-list-space’, default is nil Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-closing-list-keeps-space]

	     ["Closing list space"
	      (setq py-closing-list-space
		    (not py-closing-list-space))
	      :help "Number of chars, closing parenthesis outdent from opening, default is 1 Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-closing-list-space]

	     ["Tab shifts region "
	      (setq py-tab-shifts-region-p
		    (not py-tab-shifts-region-p))
	      :help "If ‘t’, TAB will indent/cycle the region, not just the current line.

Default is nil
See also ‘py-tab-indents-region-p’Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-tab-shifts-region-p]

	     ["Lhs inbound indent"
	      (setq py-lhs-inbound-indent
		    (not py-lhs-inbound-indent))
	      :help "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-lhs-inbound-indent]

	     ["Continuation offset"
	      (setq py-continuation-offset
		    (not py-continuation-offset))
	      :help "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-continuation-offset]

	     ["Electric colon"
	      (setq py-electric-colon-active-p
		    (not py-electric-colon-active-p))
	      :help " ‘py-electric-colon-active-p’

‘py-electric-colon’ feature.  Default is ‘nil’. See lp:837065 for discussions. "
	      :style toggle :selected py-electric-colon-active-p]

	     ["Electric colon at beginning of block only"
	      (setq py-electric-colon-bobl-only
		    (not py-electric-colon-bobl-only))
	      :help "When inserting a colon, do not indent lines unless at beginning of block.

Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-electric-colon-bobl-only]

	     ["Electric yank active "
	      (setq py-electric-yank-active-p
		    (not py-electric-yank-active-p))
	      :help " When non-nil, ‘yank’ will be followed by an ‘indent-according-to-mode’.

Default is nilUse `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-electric-yank-active-p]

	     ["Trailing whitespace smart delete "
	      (setq py-trailing-whitespace-smart-delete-p
		    (not py-trailing-whitespace-smart-delete-p))
	      :help "Default is nil. When t, python-mode calls
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-trailing-whitespace-smart-delete-p]

	     ["Newline delete trailing whitespace "
	      (setq py-newline-delete-trailing-whitespace-p
		    (not py-newline-delete-trailing-whitespace-p))
	      :help "Delete trailing whitespace maybe left by ‘py-newline-and-indent’.

Default is ‘t’. See lp:1100892 Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-newline-delete-trailing-whitespace-p]

	     ["Dedent keep relative column"
	      (setq py-dedent-keep-relative-column
		    (not py-dedent-keep-relative-column))
	      :help "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-dedent-keep-relative-column]

;; 	     ["Indent paren spanned multilines "
;; 	      (setq py-indent-paren-spanned-multilines-p
;; 		    (not py-indent-paren-spanned-multilines-p))
;; 	      :help "If non-nil, indents elements of list a value of ‘py-indent-offset’ to first element:

;; def foo():
;;     if (foo &&
;;             baz):
;;         bar()

;; Default lines up with first element:

;; def foo():
;;     if (foo &&
;;         baz):
;;         bar()
;; Use `M-x customize-variable' to set it permanently"
;; 	      :style toggle :selected py-indent-paren-spanned-multilines-p]

	     ;; ["Indent honors multiline listing"
	     ;;  (setq py-indent-honors-multiline-listing
	     ;; 	    (not py-indent-honors-multiline-listing))
	     ;;  :help "If ‘t’, indents to 1\+ column of opening delimiter. If ‘nil’, indent adds one level to the beginning of statement. Default is ‘nil’. Use `M-x customize-variable' to set it permanently"
	     ;;  :style toggle :selected py-indent-honors-multiline-listing]

	     ["Indent comment "
	      (setq py-indent-comments
		    (not py-indent-comments))
	      :help "If comments should be indented like code. Default is ‘nil’.

Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-indent-comments]

	     ["Uncomment indents "
	      (setq py-uncomment-indents-p
		    (not py-uncomment-indents-p))
	      :help "When non-nil, after uncomment indent lines. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-uncomment-indents-p]

	     ["Indent honors inline comment"
	      (setq py-indent-honors-inline-comment
		    (not py-indent-honors-inline-comment))
	      :help "If non-nil, indents to column of inlined comment start.
Default is nil. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-indent-honors-inline-comment]

	     ["Kill empty line"
	      (setq py-kill-empty-line
		    (not py-kill-empty-line))
	      :help "If t, py-indent-forward-line kills empty lines. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-kill-empty-line]

	     ("Smart indentation"
	      :help "Toggle py-smart-indentation'

Use `M-x customize-variable' to set it permanently"

	      ["Toggle py-smart-indentation" py-toggle-smart-indentation
	       :help "Toggles py-smart-indentation

Use `M-x customize-variable' to set it permanently"]

	      ["py-smart-indentation on" py-smart-indentation-on
	       :help "Switches py-smart-indentation on

Use `M-x customize-variable' to set it permanently"]

	      ["py-smart-indentation off" py-smart-indentation-off
	       :help "Switches py-smart-indentation off

Use `M-x customize-variable' to set it permanently"])

	     ["Beep if tab change"
	      (setq py-beep-if-tab-change
		    (not py-beep-if-tab-change))
	      :help "Ring the bell if ‘tab-width’ is changed.
If a comment of the form

                           	# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) ‘tab-width’ does not
equal <number>, ‘tab-width’ is set to <number>, a message saying so is
displayed in the echo area, and if ‘py-beep-if-tab-change’ is non-nil
the Emacs bell is also rung as a warning.Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-beep-if-tab-change]

	     ["Electric comment "
	      (setq py-electric-comment-p
		    (not py-electric-comment-p))
	      :help "If \"#\" should call ‘py-electric-comment’. Default is ‘nil’.

Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-electric-comment-p]

	     ["Electric comment add space "
	      (setq py-electric-comment-add-space-p
		    (not py-electric-comment-add-space-p))
	      :help "If py-electric-comment should add a space.  Default is ‘nil’. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-electric-comment-add-space-p]

	     ["Empty line closes "
	      (setq py-empty-line-closes-p
		    (not py-empty-line-closes-p))
	      :help "When non-nil, dedent after empty line following block

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil

If non-nil, a C-j from empty line dedents.
Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-empty-line-closes-p])
	    ["Defun use top level "
	     (setq py-defun-use-top-level-p
		   (not py-defun-use-top-level-p))
	     :help "When non-nil, keys C-M-a, C-M-e address top-level form.

Beginning- end-of-defun forms use
commands ‘py-backward-top-level’, ‘py-forward-top-level’

mark-defun marks top-level form at point etc. "
	     :style toggle :selected py-defun-use-top-level-p]

	    ["Close provides newline"
	     (setq py-close-provides-newline
		   (not py-close-provides-newline))
	     :help "If a newline is inserted, when line after block isn't empty. Default is non-nil. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-close-provides-newline]

	    ["Block comment prefix "
	     (setq py-block-comment-prefix-p
		   (not py-block-comment-prefix-p))
	     :help "If py-comment inserts py-block-comment-prefix.

Default is tUse `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-block-comment-prefix-p])

	   ("Display"

	    ("Index"

	     ["Imenu create index "
	      (setq py--imenu-create-index-p
		    (not py--imenu-create-index-p))
	      :help "Non-nil means Python mode creates and displays an index menu of functions and global variables. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py--imenu-create-index-p]

	     ["Imenu show method args "
	      (setq py-imenu-show-method-args-p
		    (not py-imenu-show-method-args-p))
	      :help "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed.Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-imenu-show-method-args-p]
	     ["Switch index-function" py-switch-imenu-index-function
	      :help "‘py-switch-imenu-index-function’
Switch between ‘py--imenu-create-index’ from 5.1 series and ‘py--imenu-create-index-new’."])

	    ("Fontification"

	     ["Mark decorators"
	      (setq py-mark-decorators
		    (not py-mark-decorators))
	      :help "If py-mark-def-or-class functions should mark decorators too. Default is ‘nil’. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-mark-decorators]

	     ["Fontify shell buffer "
	      (setq py-fontify-shell-buffer-p
		    (not py-fontify-shell-buffer-p))
	      :help "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If ‘t’, related vars like ‘comment-start’ will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-fontify-shell-buffer-p]

	     ["Use font lock doc face "
	      (setq py-use-font-lock-doc-face-p
		    (not py-use-font-lock-doc-face-p))
	      :help "If documention string inside of def or class get ‘font-lock-doc-face’.

‘font-lock-doc-face’ inherits ‘font-lock-string-face’.

Call M-x ‘customize-face’ in order to have a visible effect. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-use-font-lock-doc-face-p])

	    ["Switch buffers on execute"
	     (setq py-switch-buffers-on-execute-p
		   (not py-switch-buffers-on-execute-p))
	     :help "When non-nil switch to the Python output buffer.

Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-switch-buffers-on-execute-p]

	    ["Split windows on execute"
	     (setq py-split-window-on-execute
		   (not py-split-window-on-execute))
	     :help "When non-nil split windows.

Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-split-window-on-execute]

	    ["Keep windows configuration"
	     (setq py-keep-windows-configuration
		   (not py-keep-windows-configuration))
	     :help "If a windows is splitted displaying results, this is directed by variable ‘py-split-window-on-execute’\. Also setting ‘py-switch-buffers-on-execute-p’ affects window-configuration\. While commonly a screen splitted into source and Python-shell buffer is assumed, user may want to keep a different config\.

Setting ‘py-keep-windows-configuration’ to ‘t’ will restore windows-config regardless of settings mentioned above\. However, if an error occurs, it's displayed\.

To suppres window-changes due to error-signaling also: M-x customize-variable RET. Set `py-keep-4windows-configuration' onto 'force

Default is nil Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-keep-windows-configuration]

	    ["Which split windows on execute function"
	     (progn
	       (if (eq 'split-window-vertically py-split-windows-on-execute-function)
		   (setq py-split-windows-on-execute-function'split-window-horizontally)
		 (setq py-split-windows-on-execute-function 'split-window-vertically))
	       (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function))

	     :help "If ‘split-window-vertically’ or `...-horizontally'. Use `M-x customize-variable' RET ‘py-split-windows-on-execute-function’ RET to set it permanently"
	     :style toggle :selected py-split-windows-on-execute-function]

	    ["Modeline display full path "
	     (setq py-modeline-display-full-path-p
		   (not py-modeline-display-full-path-p))
	     :help "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil. Note: when ‘py-shell-name’ is specified with path, it's shown as an acronym in buffer-name already. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-modeline-display-full-path-p]

	    ["Modeline acronym display home "
	     (setq py-modeline-acronym-display-home-p
		   (not py-modeline-acronym-display-home-p))
	     :help "If the modeline acronym should contain chars indicating the home-directory.

Default is nil Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-modeline-acronym-display-home-p]

	    ["Hide show hide docstrings"
	     (setq py-hide-show-hide-docstrings
		   (not py-hide-show-hide-docstrings))
	     :help "Controls if doc strings can be hidden by hide-showUse `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-hide-show-hide-docstrings]

	    ["Hide comments when hiding all"
	     (setq py-hide-comments-when-hiding-all
		   (not py-hide-comments-when-hiding-all))
	     :help "Hide the comments too when you do ‘hs-hide-all’. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-hide-comments-when-hiding-all]

	    ["Max help buffer "
	     (setq py-max-help-buffer-p
		   (not py-max-help-buffer-p))
	     :help "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil. In help-buffer, \"q\" will close it.  Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-max-help-buffer-p]

	    ["Current defun show"
	     (setq py-current-defun-show
		   (not py-current-defun-show))
	     :help "If ‘py-current-defun’ should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is ‘t’.Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-current-defun-show]

	    ["Match paren mode"
	     (setq py-match-paren-mode
		   (not py-match-paren-mode))
	     :help "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets ‘py-match-paren-key’ in python-mode-map.
Customize ‘py-match-paren-key’ which key to use. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-match-paren-mode])

	   ("Debug"

	    ["py-debug-p"
	     (setq py-debug-p
		   (not py-debug-p))
	     :help "When non-nil, keep resp\. store information useful for debugging\.

Temporary files are not deleted\. Other functions might implement
some logging etc\. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-debug-p]

	    ["Pdbtrack do tracking "
	     (setq py-pdbtrack-do-tracking-p
		   (not py-pdbtrack-do-tracking-p))
	     :help "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the \*Python\* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb.Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-pdbtrack-do-tracking-p]

	    ["Jump on exception"
	     (setq py-jump-on-exception
		   (not py-jump-on-exception))
	     :help "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame.

Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-jump-on-exception]

	    ["Highlight error in source "
	     (setq py-highlight-error-source-p
		   (not py-highlight-error-source-p))
	     :help "Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-highlight-error-source-p])

	   ("Other"

	    ("Directory"

	     ["Guess install directory "
	      (setq py-guess-py-install-directory-p
		    (not py-guess-py-install-directory-p))
	      :help "If in cases, ‘py-install-directory’ isn't set,  ‘py-set-load-path’should guess it from ‘buffer-file-name’. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-guess-py-install-directory-p]

	     ["Use local default"
	      (setq py-use-local-default
		    (not py-use-local-default))
	      :help "If ‘t’, py-shell will use ‘py-shell-local-path’ instead
of default Python.

Making switch between several virtualenv's easier,
                               ‘python-mode’ should deliver an installer, so named-shells pointing to virtualenv's will be available. Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-use-local-default]

	     ["Use current dir when execute "
	      (setq py-use-current-dir-when-execute-p
		    (not py-use-current-dir-when-execute-p))
	      :help "When ‘t’, current directory is used by Python-shell for output of ‘py-execute-buffer’ and related commands.

See also ‘py-execute-directory’Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-use-current-dir-when-execute-p]

	     ["Keep shell dir when execute "
	      (setq py-keep-shell-dir-when-execute-p
		    (not py-keep-shell-dir-when-execute-p))
	      :help "Don't change Python shell's current working directory when sending code.

See also ‘py-execute-directory’Use `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-keep-shell-dir-when-execute-p]

	     ["Fileless buffer use default directory "
	      (setq py-fileless-buffer-use-default-directory-p
		    (not py-fileless-buffer-use-default-directory-p))
	      :help "When ‘py-use-current-dir-when-execute-p’ is non-nil and no buffer-file exists, value of ‘default-directory’ sets current working directory of Python output shellUse `M-x customize-variable' to set it permanently"
	      :style toggle :selected py-fileless-buffer-use-default-directory-p])

	    ("Underscore word syntax"
	     :help "Toggle ‘py-underscore-word-syntax-p’"

	     ["Toggle underscore word syntax" py-toggle-underscore-word-syntax-p
	      :help " ‘py-toggle-underscore-word-syntax-p’

If ‘py-underscore-word-syntax-p’ should be on or off.

  Returns value of ‘py-underscore-word-syntax-p’ switched to. .

Use `M-x customize-variable' to set it permanently"]

	     ["Underscore word syntax on" py-underscore-word-syntax-p-on
	      :help " ‘py-underscore-word-syntax-p-on’

Make sure, py-underscore-word-syntax-p' is on.

Returns value of ‘py-underscore-word-syntax-p’. .

Use `M-x customize-variable' to set it permanently"]

	     ["Underscore word syntax off" py-underscore-word-syntax-p-off
	      :help " ‘py-underscore-word-syntax-p-off’

Make sure, ‘py-underscore-word-syntax-p’ is off.

Returns value of ‘py-underscore-word-syntax-p’. .

Use `M-x customize-variable' to set it permanently"])

	    ["Load pymacs "
	     (setq py-load-pymacs-p
		   (not py-load-pymacs-p))
	     :help "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.caUse `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-load-pymacs-p]

	    ["Verbose "
	     (setq py-verbose-p
		   (not py-verbose-p))
	     :help "If functions should report results.

Default is nil. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-verbose-p]

	    ["Empty comment line separates paragraph "
	     (setq py-empty-comment-line-separates-paragraph-p
		   (not py-empty-comment-line-separates-paragraph-p))
	     :help "Consider paragraph start/end lines with nothing inside but comment sign.

Default is non-nilUse `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-empty-comment-line-separates-paragraph-p]

	    ["Org cycle "
	     (setq py-org-cycle-p
		   (not py-org-cycle-p))
	     :help "When non-nil, command ‘org-cycle’ is available at shift-TAB, <backtab>

Default is nil. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-org-cycle-p]

	    ["Set pager cat"
	     (setq py-set-pager-cat-p
		   (not py-set-pager-cat-p))
	     :help "If the shell environment variable \$PAGER should set to ‘cat’.

If ‘t’, use `C-c C-r' to jump to beginning of output. Then scroll normally.

Avoids lp:783828, \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module ‘os’ Use `M-x customize-variable' to
set it permanently"
	     :style toggle :selected py-set-pager-cat-p]

	    ["Edit only "
	     (setq py-edit-only-p
		   (not py-edit-only-p))
	     :help "When ‘t’ ‘python-mode’ will not take resort nor check for installed Python executables. Default is nil.

See bug report at launchpad, lp:944093. Use `M-x customize-variable' to set it permanently"
	     :style toggle :selected py-edit-only-p])))
         ("Other"
          ["Boolswitch" py-boolswitch
	   :help " ‘py-boolswitch’
Edit the assignment of a boolean variable, revert them.

I.e. switch it from \"True\" to \"False\" and vice versa"]

          ["Empty out list backward" py-empty-out-list-backward
	   :help " ‘py-empty-out-list-backward’
Deletes all elements from list before point."]

          ["Kill buffer unconditional" py-kill-buffer-unconditional
	   :help " ‘py-kill-buffer-unconditional’
Kill buffer unconditional, kill buffer-process if existing."]

          ["Remove overlays at point" py-remove-overlays-at-point
	   :help " ‘py-remove-overlays-at-point’
Remove overlays as set when ‘py-highlight-error-source-p’ is non-nil."]
          ("Electric"
	   ["Complete electric comma" py-complete-electric-comma
	    :help " ‘py-complete-electric-comma’"]

	   ["Complete electric lparen" py-complete-electric-lparen
	    :help " ‘py-complete-electric-lparen’"]

	   ["Electric backspace" py-electric-backspace
	    :help " ‘py-electric-backspace’
Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached."]

	   ["Electric colon" py-electric-colon
	    :help " ‘py-electric-colon’
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by ‘py-electric-colon-active-p’, default is nil
See also ‘py-electric-colon-greedy-p’"]

	   ["Electric comment" py-electric-comment
	    :help " ‘py-electric-comment’
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With C-u \"#\" electric behavior is inhibited inside a string or comment."]

	   ["Electric delete" py-electric-delete
	    :help " ‘py-electric-delete’
Delete following character or levels of whitespace.

With ARG do that ARG times."]

	   ["Electric yank" py-electric-yank
	    :help " ‘py-electric-yank’
Perform command ‘yank’ followed by an ‘indent-according-to-mode’"]

	   ["Hungry delete backwards" py-hungry-delete-backwards
	    :help " ‘py-hungry-delete-backwards’
Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.
See also C-c <delete>."]

	   ["Hungry delete forward" py-hungry-delete-forward
	    :help " ‘py-hungry-delete-forward’
Delete the following character or all following whitespace
up to the next non-whitespace character.
See also C-c <C-backspace>."]
            )
          ("Abbrevs"	   :help "see also ‘py-add-abbrev’"
	   :filter (lambda (&rest junk)
		     (abbrev-table-menu python-mode-abbrev-table))            )

          ["Add abbrev" py-add-abbrev
	   :help " ‘py-add-abbrev’
Defines python-mode specific abbrev for last expressions before point.
Argument is how many ‘py-partial-expression’s form the expansion; or zero means the region is the expansion.

Reads the abbreviation in the minibuffer; with numeric arg it displays a proposal for an abbrev.
Proposal is composed from the initial character(s) of the
expansion.

Don't use this function in a Lisp program; use ‘define-abbrev’ instead."]
          ("Completion"
	   ["Py indent or complete" py-indent-or-complete
	    :help " ‘py-indent-or-complete’"]

	   ["Py shell complete" py-shell-complete
	    :help " ‘py-shell-complete’"]

	   ["Py complete" py-complete
	    :help " ‘py-complete’"]
            )))))

;; python-components-complete

(defun py--shell-completion-get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (let ((erg
	 (py-send-string-no-output (format completion-code input) process)))
    (if (and erg (> (length erg) 2))
	(setq erg (split-string erg "^'\\|^\"\\|;\\|'$\\|\"$" t))
      (and py-verbose-p (message "py--shell-completion-get-completions: %s" "Don't see a completion")))
    erg))

;; post-command-hook
;; caused insert-file-contents error lp:1293172
(defun py--after-change-function (end)
  "Restore window-confiuration after completion.

Takes END"
  (when
      (and (or
            (eq this-command 'completion-at-point)
            (eq this-command 'choose-completion)
            (eq this-command 'choose-completion)
            (eq this-command 'py-shell-complete)
            (and (or
                  (eq last-command 'completion-at-point)
                  (eq last-command 'choose-completion)
                  (eq last-command 'choose-completion)
                  (eq last-command 'py-shell-complete))
                 (eq this-command 'self-insert-command))))
    (py-restore-window-configuration)
    )

  (goto-char end))

(defun py--shell-insert-completion-maybe (completion input)
  (cond ((eq completion t)
	 (and py-verbose-p (message "py--shell-do-completion-at-point %s" "‘t’ is returned, not completion. Might be a bug.")))
	((null completion)
	 (and py-verbose-p (message "py--shell-do-completion-at-point %s" "Don't see a completion")))
	((and completion
	      (or (and (listp completion)
		       (string= input (car completion)))
		  (and (stringp completion)
		       (string= input completion)))))
	((and completion (stringp completion)(or (string= input completion) (string= "''" completion))))
	((and completion (stringp completion))
	 (progn (delete-char (- (length input)))
		(insert completion)))
	(t (py--try-completion input completion)))
  )

(defun py--shell-do-completion-at-point (process imports input exception-buffer code)
  "Do completion at point for PROCESS.

Takes PROCESS IMPORTS INPUT EXCEPTION-BUFFER CODE"
  (when imports
    (py-execute-string imports process))
  (sit-for 0.1 t)
  (let* ((completion
	  (py--shell-completion-get-completions
	   input process code)))
    (set-buffer exception-buffer)
    (when completion
      (py--shell-insert-completion-maybe completion input))))

(defun py--complete-base (shell word imports buffer)
  (let* ((proc (or
		;; completing inside a shell
		(get-buffer-process buffer)
		(and (comint-check-proc shell)
		     (get-process shell))
		(prog1
		    (get-buffer-process (py-shell nil nil nil shell))
		  (sit-for py-new-shell-delay t))))
	 ;; (buffer (process-buffer proc))
	 (code (if (string-match "[Ii][Pp]ython*" shell)
		   (py-set-ipython-completion-command-string shell)
		 py-shell-module-completion-code)))
    (py--shell-do-completion-at-point proc imports word buffer code)))

(defun py--try-completion-intern (input completion buffer)
  (with-current-buffer buffer
    (let ((erg nil))
      (and (setq erg (try-completion input completion))
	   (sit-for 0.1)
	   (looking-back input (line-beginning-position))
	   (not (string= input erg))
	   (setq erg (completion-in-region (match-beginning 0) (match-end 0) completion)))))
  ;; (set-window-configuration py-last-window-configuration)
  )

(defun py--try-completion (input completion)
  "Repeat ‘try-completion’ as long as match are found.

Interal used. Takes INPUT COMPLETION"
  (let ((erg nil)
	(newlist nil))
    (unless (py--try-completion-intern input completion (current-buffer))
      (dolist (elt completion)
	(unless (string= erg elt)
	  (push elt newlist)))
      (if (< 1 (length newlist))
	  (with-output-to-temp-buffer py-python-completions
	    (display-completion-list
	     (all-completions input (or newlist completion))))))))

(defun py--fast-completion-get-completions (input process completion-code buffer)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (let ((completions
	 (py-fast-send-string
	  (format completion-code input) process buffer t)))
    (when (> (length completions) 2)
      (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t))))

(defun py--fast--do-completion-at-point (process imports input code buffer)
  "Do completion at point for PROCESS."
  ;; send setup-code
  (let (py-store-result-p)
    (when imports
      ;; (message "%s" imports)
      (py-fast-send-string imports process buffer nil t)))
  (let* ((completion
	  (py--fast-completion-get-completions input process code buffer)))
    (sit-for 0.1)
    (cond ((eq completion t)
	   (and py-verbose-p (message "py--fast--do-completion-at-point %s" "‘t’ is returned, not completion. Might be a bug.")))
	  ((null completion)
	   (and py-verbose-p (message "py--fast--do-completion-at-point %s" "Don't see a completion"))
	   (set-window-configuration py-last-window-configuration))
	  ((and completion
		(or (and (listp completion)
			 (string= input (car completion)))
		    (and (stringp completion)
			 (string= input completion))))
	   (set-window-configuration py-last-window-configuration))
	  ((and completion (stringp completion) (not (string= input completion)))
	   (progn (delete-char (- (length input)))
		  (insert completion)
		  ;; (move-marker orig (point))
		  ;; minibuffer.el expects a list
		  ))
	  (t (py--try-completion input completion)))))

(defun py--fast-complete-base (shell word imports)
  (let* (py-split-window-on-execute py-switch-buffers-on-execute-p
	 (shell (or shell py-shell-name))
	 (buffer (py-shell nil nil nil shell nil t))
 	 (proc (get-buffer-process buffer))
	 (code (if (string-match "[Ii][Pp]ython*" shell)
		   (py-set-ipython-completion-command-string shell)
		 py-shell-module-completion-code)))
    (py--python-send-completion-setup-code buffer)
    (py--fast--do-completion-at-point proc imports word code buffer)))

(defun py-shell-complete (&optional shell beg end word fast imports)
  (interactive)
  (let* ((exception-buffer (current-buffer))
	 (pps (parse-partial-sexp
	       (or
		(ignore-errors (cdr-safe comint-last-prompt))
		(ignore-errors comint-last-prompt)
		(line-beginning-position))
	       (point)))
	 (in-string (when (nth 3 pps) (nth 8 pps)))
         (beg
	  (save-excursion
	    (or beg
	 	(and in-string
	 	     ;; possible completion of filenames
	 	     (progn
	 	       (goto-char in-string)
	 	       (and
	 		(save-excursion
	 		  (skip-chars-backward "^ \t\r\n\f") (looking-at "open")))

	 	       (skip-chars-forward "\"'") (point)))
	 	(progn (and (eq (char-before) ?\()(forward-char -1))
	 	       (skip-chars-backward "a-zA-Z0-9_.'") (point)))))
         (end (or end (point)))
	 (word (or word (buffer-substring-no-properties beg end)))
	 (ausdruck (and (string-match "^/" word) (setq word (substring-no-properties word 1))(concat "\"" word "*\"")))
	 ;; when in string, assume looking for filename
	 (filenames (and in-string ausdruck
			 (list (replace-regexp-in-string "\n" "" (shell-command-to-string (concat "find / -maxdepth 1 -name " ausdruck))))))
         (imports (or imports (py-find-imports)))
         py-fontify-shell-buffer-p erg)
    (cond (fast (py--fast-complete-base shell word imports))
	  ((and in-string filenames)
	   (when (setq erg (try-completion (concat "/" word) filenames))
	     (delete-region beg end)
	     (insert erg)))
	  (t (py--complete-base shell word imports exception-buffer)))
    nil))

(defun py-fast-complete (&optional shell word imports)
  "Complete word before point, if any.

Use ‘py-fast-process’ "
  (interactive "*")
  (window-configuration-to-register py--windows-config-register)
  (setq py-last-window-configuration
  	(current-window-configuration))
  (py-shell-complete shell nil nil word 1 imports)
  (py-restore-window-configuration)
  )

(defun py-indent-or-complete ()
  "Complete or indent depending on the context.

If cursor is at end of a symbol, try to complete
Otherwise call ‘py-indent-line’

If `(use-region-p)' returns t, indent region.
Use `C-q TAB' to insert a literally TAB-character

In ‘python-mode’ ‘py-complete-function’ is called,
in (I)Python shell-modes ‘py-shell-complete’"
  (interactive "*")
  (window-configuration-to-register py--windows-config-register)
  ;; (setq py-last-window-configuration
  ;;       (current-window-configuration))
  (cond ((use-region-p)
	 (when py-debug-p (message "py-indent-or-complete: %s" "calling ‘use-region-p’-clause"))
	 (py-indent-region (region-beginning) (region-end)))
	((or (bolp)
	     (member (char-before) (list 9 10 12 13 32 ?: ?\) ?\] ?\}))
	     (not (looking-at "[ \t]*$")))
	 (py-indent-line))
	((and py-do-completion-p (comint-check-proc (current-buffer)))
	 ;; (let* ((shell (process-name (get-buffer-process (current-buffer)))))
	 (ignore-errors (completion-at-point)))
	((and py-do-completion-p t)
	 (when py-debug-p (message "py-indent-or-complete: %s" "calling ‘t’-clause"))
	 ;; (py-fast-complete)
	 (completion-at-point))))

;; python-components-pdb

(defun py-execute-statement-pdb ()
  "Execute statement running pdb."
  (interactive)
  (let ((py-python-command-args "-i -m pdb"))
    (py-execute-statement)))

(defun py-execute-region-pdb (beg end)
  "Takes region between BEG END."
  (interactive "r")
  (let ((py-python-command-args "-i -m pdb"))
    (py-execute-region beg end)))

(defun py-pdb-execute-statement ()
  "Execute statement running pdb."
  (interactive)
  (let ((stm (progn (py-statement) (car kill-ring))))
    (py-execute-string (concat "import pdb;pdb.run('" stm "')"))))

(defun py-pdb-help ()
  "Print generic pdb.help() message."
  (interactive)
  (py-execute-string "import pdb;pdb.help()"))

;; https://stackoverflow.com/questions/6980749/simpler-way-to-put-pdb-breakpoints-in-python-code
;; breakpoint at line 3
;; avoid inserting pdb.set_trace()

;; python -m pdb -c "b 3" -c c your_script.py

(defun py-pdb-break-at-current-line (&optional line)
  "Set breakpoint at current line.

Optional LINE FILE CONDITION"
  (interactive "p")
  (let ((line (number-to-string (or line (py-count-lines)))))
    (py-execute-string (concat "import pdb;pdb.break('" line "')"))))

(defun py--pdb-versioned ()
  "Guess existing pdb version from ‘py-shell-name’.

Return \"pdb[VERSION]\" if executable found, just \"pdb\" otherwise"
  (interactive)
  (let ((erg (when (string-match "[23]" py-shell-name)
	       ;; versions-part
	       (substring py-shell-name (string-match "[23]" py-shell-name)))))
    (if erg
	(cond ((executable-find (concat "pdb" erg))
	       (concat "pdb" erg))
	      ((and (string-match "\\." erg)
		    (executable-find (concat "pdb" (substring erg 0 (string-match "\\." erg)))))
	       (concat "pdb" (substring erg 0 (string-match "\\." erg)))))
      "pdb")))

(declare-function gud-query-cmdline "gud" ())

(defun py-pdb (command-line)
  "Run pdb on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

At GNU Linux required pdb version should be detected by ‘py--pdb-version’
at Windows configure ‘py-python-ms-pdb-command’

lp:963253
Argument COMMAND-LINE TBD."
  (interactive
   (progn
     (require 'gud)
     (list (gud-query-cmdline
	    (if (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
		(car (read-from-string py-python-ms-pdb-command))
	      ;; sys.version_info[0]
	      ;; (car (read-from-string (py--pdb-version)))
	      'pdb)
	    (py--buffer-filename-remote-maybe)))))
  (pdb command-line))

(defun py--pdb-current-executable ()
  "When ‘py-pdb-executable’ is set, return it.

Otherwise return resuslt from ‘executable-find’"
  (or py-pdb-executable
      (executable-find "pdb")))

(defun py-update-gud-pdb-history ()
  "Put pdb file name at the head of ‘gud-pdb-history’.

If pdb is called at a Python buffer."
  (interactive)
  (let* (;; PATH/TO/pdb
	 (first (cond ((and gud-pdb-history (ignore-errors (car gud-pdb-history)))
		       (replace-regexp-in-string "^\\([^ ]+\\) +.+$" "\\1" (car gud-pdb-history)))
		      (py-pdb-executable
		       py-pdb-executable)
		      ((or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
		       ;; lp:963253
		       "c:/python27/python\ -i\ c:/python27/Lib/pdb.py")
		      (t
		       (py--pdb-current-executable))))
	 ;; file to debug
         (second (cond ((not (ignore-errors
			       (py--buffer-filename-remote-maybe)))
			(error "%s" "Buffer must be saved first."))
		       ((py--buffer-filename-remote-maybe))
		       (t (and gud-pdb-history (stringp (car gud-pdb-history)) (replace-regexp-in-string "^\\([^ ]+\\) +\\(.+\\)$" "\\2" (car gud-pdb-history))))))
         (erg (and first second (concat first " " second))))
    (when erg
      (push erg gud-pdb-history))))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline py-pdb-path
                            ;; (file-name-nondirectory buffer-file-name)
			    (file-name-nondirectory (py--buffer-filename-remote-maybe)) ))))

;; tbreak [ ([filename:]lineno | function) [, condition] ]
;;         Same arguments as break, but sets a temporary breakpoint: it
;;         is automatically deleted when first hit.

;; python -m pdb -c "b 3" -c c your_script.py

(defun py-pdb-tbreak ()
  "Insert a temporary break."
  (interactive)
  (let (
	(py-python-command-args '("-i -c \"b 30\" -c c \"eyp.py\""))
	(py-python3-command-args '("-i -c \"b 30\" -c c \"eyp.py\""))
	)
    (py-execute-buffer)))



(defun py--pdbtrack-overlay-arrow (activation)
  "Activate or de arrow at beginning-of-line in current buffer."
  ;; This was derived/simplified from edebug-overlay-arrow
  (cond (activation
         (setq overlay-arrow-position (make-marker))
         (setq overlay-arrow-string "=>")
         (set-marker overlay-arrow-position (line-beginning-position) (current-buffer))
         (setq py-pdbtrack-is-tracking-p t))
        (overlay-arrow-position
         (setq overlay-arrow-position nil)
         (setq py-pdbtrack-is-tracking-p nil))))

(defun py--pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
‘py-pdbtrack-do-tracking-p’ is nil.

We depend on the pdb input prompt matching ‘py-pdbtrack-input-prompt’
at the beginning of the line.

If the traceback target file path is invalid, we look for the most
recently visited python-mode buffer which either has the name of the
current function \(or class) or which defines the function \(or
class).  This is to provide for remote scripts, eg, Zope's ‘Script
\(Python)’ - put a _copy_ of the script in a buffer named for the
script, and set to python-mode, and pdbtrack will find it.)"
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; ‘where’ (aka ‘w’) command to reveal the overlay arrow.
  (let* ((origbuf (current-buffer))
         (currproc (get-buffer-process origbuf)))

    (if (not (and currproc py-pdbtrack-do-tracking-p))
        (py--pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              py-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat py-pdbtrack-input-prompt "$") block))
            (py--pdbtrack-overlay-arrow nil)

          (setq target (py--pdbtrack-get-source-buffer block))

          (if (stringp target)
              (message "pdbtrack: %s" target)

            (setq target_lineno (car target))
            (setq target_buffer (cadr target))
            (setq target_fname
		  (py--buffer-filename-remote-maybe target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-char (point-min))
            (forward-line (1- target_lineno))
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (py--pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)))))))

(defun py--pdbtrack-map-filename (filename)

  (let
      ((replacement-val (assoc-default
                         filename py-pdbtrack-filename-mapping
                         (lambda (mapkey path)
                           (string-match
                            (concat "^" (regexp-quote mapkey))
                            path)))
                        ))
    (if (not (eq replacement-val nil))
        (replace-match replacement-val 't 't filename)
      filename)))

(defun py--pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (and (not (string-match py-pdbtrack-stack-entry-regexp block))
           ;; pydb integration still to be done
           ;; (not (string-match py-pydbtrack-stack-entry-regexp block))
	   )
      (prog1
	  "Traceback cue not found"
	(message "Block: %s" block))
    (let* ((remote-prefix (or (file-remote-p default-directory) ""))
           (filename (concat remote-prefix
                             (match-string
                              py-pdbtrack-marker-regexp-file-group block)))
           (lineno (string-to-number (match-string
                                      py-pdbtrack-marker-regexp-line-group
                                      block)))
           (funcname (match-string py-pdbtrack-marker-regexp-funcname-group
                                   block))
           funcbuffer)

      (cond ((string= filename "")
             (format "(Skipping empty filename)"))

            ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((file-exists-p (py--pdbtrack-map-filename filename))
             (list lineno (find-file-noselect (py--pdbtrack-map-filename filename))))

            ((setq funcbuffer (py--pdbtrack-grub-for-buffer funcname))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (save-excursion
                            (with-current-buffer funcbuffer
			      (count-lines
			       (point-min)
			       (max (point-min)
				    (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
						  (buffer-substring (point-min)
								    (point-max))))))))))
             (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename))))))

(defun py--pdbtrack-grub-for-buffer (funcname)
  "Find most recent buffer itself named or having function funcname.

We walk the buffer-list history for python-mode buffers that are
named for funcname or define a function funcname."
  (let ((buffers (buffer-list))
        buf
        got)
    (while (and buffers (not got))
      (setq buf (car buffers)
            buffers (cdr buffers))
      (if (and (save-excursion
		 (with-current-buffer buf
		   (string= major-mode "python-mode")))
               (or (string-match funcname (buffer-name buf))
                   (string-match (concat "^\\s-*\\(def\\|class\\)\\s-+"
                                         funcname "\\s-*(")
                                 (save-excursion
                                   (with-current-buffer buf
                                     (buffer-substring (point-min)
                                                       (point-max)))))))
          (setq got buf)))
    got))

;; pdbtrack functions
(defun py-pdbtrack-set-tracked-buffer (file-name)
  "Set the buffer for FILE-NAME as the tracked buffer.
Internally it uses the ‘py-pdbtrack-tracked-buffer’ variable.
Returns the tracked buffer."
  (let* ((file-name-prospect (concat (file-remote-p default-directory)
                                     file-name))
         (file-buffer (get-file-buffer file-name-prospect)))
    (if file-buffer
        (setq py-pdbtrack-tracked-buffer file-buffer)
      (cond
       ((file-exists-p file-name-prospect)
        (setq file-buffer (find-file-noselect file-name-prospect)))
       ((and (not (equal file-name file-name-prospect))
             (file-exists-p file-name))
        ;; Fallback to a locally available copy of the file.
        (setq file-buffer (find-file-noselect file-name-prospect))))
      (when (not (member file-buffer py-pdbtrack-buffers-to-kill))
        (add-to-list 'py-pdbtrack-buffers-to-kill file-buffer)))
    file-buffer))

(defun py-pdbtrack-toggle-stack-tracking (arg)
  "Set variable ‘py-pdbtrack-do-tracking-p’. "
  (interactive "P")
  ;; (if (not (get-buffer-process (current-buffer)))
  ;; (error "No process associated with buffer '%s'" (current-buffer)))

  ;; missing or 0 is toggle, >0 turn on, <0 turn off
  (cond ((not arg)
         (setq py-pdbtrack-do-tracking-p (not py-pdbtrack-do-tracking-p)))
        ((zerop (prefix-numeric-value arg))
         (setq py-pdbtrack-do-tracking-p nil))
        ((> (prefix-numeric-value arg) 0)
         (setq py-pdbtrack-do-tracking-p t)))
  ;; (if py-pdbtrack-do-tracking-p
  ;;     (progn
  ;;       (add-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file)
  ;;       (remove-hook 'comint-output-filter-functions 'python-pdbtrack-track-stack-file))
  ;;   (remove-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file)
  ;;   )
  (message "%sabled Python's pdbtrack"
           (if py-pdbtrack-do-tracking-p "En" "Dis")))

(defun turn-on-pdbtrack ()
  (interactive)
  (py-pdbtrack-toggle-stack-tracking 1))

(defun turn-off-pdbtrack ()
  (interactive)
  (py-pdbtrack-toggle-stack-tracking 0))



(if pdb-track-stack-from-shell-p
    (add-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file)
  (remove-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file))


(defun py-pdbtrack-comint-output-filter-function (output)
  "Move overlay arrow to current pdb line in tracked buffer.
Argument OUTPUT is a string with the output from the comint process."
  (when (and pdb-track-stack-from-shell-p (not (string= output "")))
    (let* ((full-output (ansi-color-filter-apply
                         (buffer-substring comint-last-input-end (point-max))))
           (line-number)
           (file-name
            (with-temp-buffer
              (insert full-output)
              ;; When the debugger encounters a pdb.set_trace()
              ;; command, it prints a single stack frame.  Sometimes
              ;; it prints a bit of extra information about the
              ;; arguments of the present function.  When ipdb
              ;; encounters an exception, it prints the _entire_ stack
              ;; trace.  To handle all of these cases, we want to find
              ;; the _last_ stack frame printed in the most recent
              ;; batch of output, then jump to the corresponding
              ;; file/line number.
              (goto-char (point-max))
              (when (re-search-backward py-pdbtrack-stacktrace-info-regexp nil t)
                (setq line-number (string-to-number
                                   (match-string-no-properties 2)))
                (match-string-no-properties 1)))))
      (if (and file-name line-number)
          (let* ((tracked-buffer
                  (py-pdbtrack-set-tracked-buffer file-name))
                 (shell-buffer (current-buffer))
                 (tracked-buffer-window (get-buffer-window tracked-buffer))
                 (tracked-buffer-line-pos))
            (with-current-buffer tracked-buffer
              (set (make-local-variable 'overlay-arrow-string) "=>")
              (set (make-local-variable 'overlay-arrow-position) (make-marker))
              (setq tracked-buffer-line-pos (progn
                                              (goto-char (point-min))
                                              (forward-line (1- line-number))
                                              (point-marker)))
              (when tracked-buffer-window
                (set-window-point
                 tracked-buffer-window tracked-buffer-line-pos))
              (set-marker overlay-arrow-position tracked-buffer-line-pos))
            (pop-to-buffer tracked-buffer)
            (switch-to-buffer-other-window shell-buffer))
        (when py-pdbtrack-tracked-buffer
          (with-current-buffer py-pdbtrack-tracked-buffer
            (set-marker overlay-arrow-position nil))
          (mapc #'(lambda (buffer)
                    (ignore-errors (kill-buffer buffer)))
                py-pdbtrack-buffers-to-kill)
          (setq py-pdbtrack-tracked-buffer nil
                py-pdbtrack-buffers-to-kill nil)))))
  output)

;; python-components-pdbtrack


;; python-components-help

;;  Info-look functionality.
(require 'info-look)
(eval-when-compile (require 'info))

(defun py-info-lookup-symbol ()
  "Call ‘info-lookup-symbol’.

Sends help if stuff is missing."
  (interactive)
  (if (functionp 'pydoc-info-add-help)
      (call-interactively 'info-lookup-symbol)
    (message "pydoc-info-add-help not found. Please check INSTALL-INFO-FILES")))

(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
'(("(python)Index" nil "")))

(defun python-after-info-look ()
  "Set up info-look for Python.

Tries to take account of versioned Python Info files, e.g. Debian's
python2.5-ref.info.gz.
Used with ‘eval-after-load’."
  (let* ((version (let ((s (shell-command-to-string (concat py-python-command
							    " -V"))))
		    (string-match "^Python \\([0-9]+\\.[0-9]+\\>\\)" s)
		    (match-string 1 s)))
	 ;; Whether info files have a Python version suffix, e.g. in Debian.
	 (versioned
	  (with-temp-buffer
	    (Info-mode)
	    ;; First look for Info files corresponding to the version
	    ;; of the interpreter we're running.
	    (condition-case ()
		;; Don't use ‘info’ because it would pop-up a *info* buffer.
		(progn
		  (Info-goto-node (format "(python%s-lib)Miscellaneous Index"
					  version))
		  t)
	      (error
	       ;; Otherwise see if we actually have an un-versioned one.
	       (condition-case ()
		   (progn
		     (Info-goto-node
		      (format "(python%s-lib)Miscellaneous Index" version))
		     nil)
		 (error
		  ;; Otherwise look for any versioned Info file.
		  (condition-case ()
		      (let (found)
			(dolist (dir (or Info-directory-list
					 Info-default-directory-list))
			  (unless found
			    (let ((file (car (file-expand-wildcards
					      (expand-file-name "python*-lib*"
								dir)))))
			      (if (and file
				       (string-match
					"\\<python\\([0-9]+\\.[0-9]+\\>\\)-"
					file))
				  (setq version (match-string 1 file)
					found t)))))
			found)
		    (error)))))))))
    (info-lookup-maybe-add-help
     :mode 'python-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
	 ;; The empty prefix just gets us highlighted terms.
	 `((,(concat "(python" version "-ref)Miscellaneous Index"))
	   (,(concat "(python" version "-ref)Module Index"))
	   (,(concat "(python" version "-ref)Function-Method-Variable Index"))
	   (,(concat "(python" version "-ref)Class-Exception-Object Index"))
	   (,(concat "(python" version "-lib)Module Index"))
	   (,(concat "(python" version "-lib)Class-Exception-Object Index"))
	   (,(concat "(python" version "-lib)Function-Method-Variable Index"))
	   (,(concat "(python" version "-lib)Miscellaneous Index")))
       '(("(python-ref)Miscellaneous Index")
	 ("(python-ref)Module Index")
	 ("(python-ref)Function-Method-Variable Index")
	 ("(python-ref)Class-Exception-Object Index")
	 ("(python-lib)Module Index")
	 ("(python-lib)Class-Exception-Object Index")
	 ("(python-lib)Function-Method-Variable Index")
	 ("(python-lib)Miscellaneous Index"))))))

;;  (if (featurep 'info-look)
;;      (python-after-info-look))

;;  (eval-after-load "info-look" '(python-after-info-look))

;; ;

(defun py-fetch-docu ()
  "Lookup in current buffer for the doku for the symbol at point.

Useful for newly defined symbol, not known to python yet."
  (interactive)
  (let* ((symb (prin1-to-string (symbol-at-point)))
         erg)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat py-def-or-class-re " *" symb) nil (quote move) 1)
        (forward-line 1)
        (when (looking-at "[ \t]*\"\"\"\\|[ \t]*'''\\|[ \t]*'[^]+\\|[ \t]*\"[^\"]+")
          (goto-char (match-end 0))
          (setq erg (buffer-substring-no-properties (match-beginning 0) (re-search-forward "\"\"\"\\|'''" nil 'move)))
          (when erg
            (set-buffer (get-buffer-create "*Python-Help*"))
            (erase-buffer)
            ;; (when (called-interactively-p 'interactive)
            ;;   (switch-to-buffer (current-buffer)))
            (insert erg)))))))

(defun py-info-current-defun (&optional include-type)
  "Return name of surrounding function.

Use Python compatible dotted expression syntax
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function is compatible to be used as
‘add-log-current-defun-function’ since it returns nil if point is
not inside a defun."
  (interactive)
  (let ((names '())
        (min-indent)
        (first-run t))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (line-end-position))
        (forward-comment -9999)
        (setq min-indent (current-indentation))
        (while (py-backward-def-or-class)
          (when (or (< (current-indentation) min-indent)
                    first-run)
            (setq first-run nil)
            (setq min-indent (current-indentation))
            (looking-at py-def-or-class-re)
            (setq names (cons
                         (if (not include-type)
                             (match-string-no-properties 1)
                           (mapconcat 'identity
                                      (split-string
                                       (match-string-no-properties 0)) " "))
                         names))))))
    (when names
      (mapconcat (lambda (strg) strg) names "."))))

(defalias 'py-describe-symbol 'py-help-at-point)
(defun py--help-at-point-intern (sym orig)
  (let* ((origfile (py--buffer-filename-remote-maybe))
	 (cmd (py-find-imports))
	 (oldbuf (current-buffer))
	 )
    (when (not py-remove-cwd-from-path)
      (setq cmd (concat cmd "import sys\n"
			"sys.path.insert(0, '"
			(file-name-directory origfile) "')\n")))
    ;; (setq cmd (concat cmd "pydoc.help('" sym "')\n"))
    (py-execute-string (concat cmd "help('" sym "')\n") nil t nil orig nil nil nil nil nil nil oldbuf t)
    (display-buffer oldbuf)))
    ;; (with-help-window "Hilfe" (insert py-result))))

(defun py-help-at-point ()
  "Print help on symbol at point.

If symbol is defined in current buffer, jump to it's definition"
  (interactive)
  (let* ((orig (point))
	 (beg (and (use-region-p) (region-beginning)))
	 (end (and (use-region-p) (region-end)))
	 (symbol
	  (or (and beg end
		   (buffer-substring-no-properties beg end))
	      ;; (thing-at-point 'symbol t)
	      (py-symbol-at-point))))
    (and symbol (unless (string= "" symbol)
		  (py--help-at-point-intern symbol orig))
	 ;; (py--shell-manage-windows buffer exception-buffer split (or interactivep switch))
	 )))

(defun py--dump-help-string (str)
  (with-output-to-temp-buffer "*Help*"
    (let ((locals (buffer-local-variables))
          funckind funcname func funcdoc
          (start 0) mstart end
          keys)
      (while (string-match "^%\\([vc]\\):\\(.+\\)\n" str start)
        (setq mstart (match-beginning 0) end (match-end 0)
              funckind (substring str (match-beginning 1) (match-end 1))
              funcname (substring str (match-beginning 2) (match-end 2))
              func (intern funcname))
        (princ (substitute-command-keys (substring str start mstart)))
        (cond
         ((equal funckind "c")          ; command
          (setq funcdoc (documentation func)
                keys (concat
                      "Key(s): "
                      (mapconcat 'key-description
                                 (where-is-internal func python-mode-map)
                                 ", "))))
         ((equal funckind "v")          ; variable
          (setq funcdoc (documentation-property func 'variable-documentation)
                keys (if (assq func locals)
                         (concat
                          "Local/Global values: "
                          (prin1-to-string (symbol-value func))
                          " / "
                          (prin1-to-string (default-value func)))
                       (concat
                        "Value: "
                        (prin1-to-string (symbol-value func))))))
         (t                             ; unexpected
          (error "Error in py--dump-help-string, tag %s" funckind)))
        (princ (format "\n-> %s:\t%s\t%s\n\n"
                       (if (equal funckind "c") "Command" "Variable")
                       funcname keys))
        (princ funcdoc)
        (terpri)
        (setq start end))
      (princ (substitute-command-keys (substring str start)))
      ;; (and comint-vars-p (py-report-comint-variable-setting))
      )
    (if (featurep 'xemacs) (print-help-return-message)
      (help-print-return-message))))

(defun py-describe-mode ()
  "Dump long form of ‘python-mode’ docs."
  (interactive)
  (py--dump-help-string "Major mode for editing Python files.
Knows about Python indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only.

Major sections below begin with the string `@'; specific function and
variable docs begin with ->.

@EXECUTING PYTHON CODE

\\[py-execute-import-or-reload]\timports or reloads the file in the Python interpreter
\\[py-execute-buffer]\tsends the entire buffer to the Python interpreter
\\[py-execute-region]\tsends the current region
\\[py-execute-def-or-class]\tsends the current function or class definition
\\[py-execute-string]\tsends an arbitrary string
\\[py-shell]\tstarts a Python interpreter window; this will be used by
\tsubsequent Python execution commands
%c:py-execute-import-or-reload
%c:py-execute-buffer
%c:py-execute-region
%c:py-execute-def-or-class
%c:py-execute-string
%c:py-shell

@VARIABLES

py-install-directory\twherefrom ‘python-mode’ looks for extensions
py-indent-offset\tindentation increment
py-block-comment-prefix\tcomment string used by comment-region

py-shell-name\tshell command to invoke Python interpreter
py-temp-directory\tdirectory used for temp files (if needed)

py-beep-if-tab-change\tring the bell if tab-width is changed
%v:py-install-directory
%v:py-indent-offset
%v:py-block-comment-prefix
%v:py-shell-name
%v:py-temp-directory
%v:py-beep-if-tab-change

@KINDS OF LINES

Each physical line in the file is either a `continuation line' (the
preceding line ends with a backslash that's not part of a comment, or
the paren/bracket/brace nesting level at the start of the line is
non-zero, or both) or an `initial line' (everything else).

An initial line is in turn a `blank line' (contains nothing except
possibly blanks or tabs), a `comment line' (leftmost non-blank
character is `#’), or a ‘code line' (everything else).

Comment Lines

Although all comment lines are treated alike by Python, Python mode
recognizes two kinds that act differently with respect to indentation.

An `indenting comment line' is a comment line with a blank, tab or
nothing after the initial `#'.  The indentation commands (see below)
treat these exactly as if they were code lines: a line following an
indenting comment line will be indented like the comment line.  All
other comment lines (those with a non-whitespace character immediately
following the initial `#’) are ‘non-indenting comment lines', and
their indentation is ignored by the indentation commands.

Indenting comment lines are by far the usual case, and should be used
whenever possible.  Non-indenting comment lines are useful in cases
like these:

\ta = b # a very wordy single-line comment that ends up being
\t #... continued onto another line

\tif a == b:
##\t\tprint 'panic!' # old code we've `commented out'
\t\treturn a

Since the `#...’ and ‘##' comment lines have a non-whitespace
character following the initial `#', Python mode ignores them when
computing the proper indentation for the next line.

Continuation Lines and Statements

The ‘python-mode’ commands generally work on statements instead of on
individual lines, where a ‘statement’ is a comment or blank line, or a
code line and all of its following continuation lines (if any)
considered as a single logical unit.  The commands in this mode
generally (when it makes sense) automatically move to the start of the
statement containing point, even if point happens to be in the middle
of some continuation line.

@INDENTATION

Primarily for entering new code:
\t\\[indent-for-tab-command]\t indent line appropriately
\t\\[py-newline-and-indent]\t insert newline, then indent
\t\\[py-electric-backspace]\t reduce indentation, or delete single character

Primarily for reindenting existing code:
\t\\[py-guess-indent-offset]\t guess py-indent-offset from file content; change locally
\t\\[universal-argument] \\[py-guess-indent-offset]\t ditto, but change globally

\t\\[py-indent-region]\t reindent region to match its context
\t\\[py-shift-left]\t shift line or region left by py-indent-offset
\t\\[py-shift-right]\t shift line or region right by py-indent-offset

Unlike most programming languages, Python uses indentation, and only
indentation, to specify block structure.  Hence the indentation supplied
automatically by ‘python-mode’ is just an educated guess:  only you know
the block structure you intend, so only you can supply correct
indentation.

The \\[indent-for-tab-command] and \\[py-newline-and-indent] keys try to suggest plausible indentation, based on
the indentation of preceding statements.  E.g., assuming
py-indent-offset is 4, after you enter
\tif a > 0: \\[py-newline-and-indent]
the cursor will be moved to the position of the ‘_’ (_ is not a
character in the file, it's just used here to indicate the location of
the cursor):
\tif a > 0:
\t _
If you then enter `c = d' \\[py-newline-and-indent], the cursor will move
to
\tif a > 0:
\t c = d
\t _
‘python-mode’ cannot know whether that's what you intended, or whether
\tif a > 0:
\t c = d
\t_
was your intent.  In general, ‘python-mode’ either reproduces the
indentation of the (closest code or indenting-comment) preceding
statement, or adds an extra py-indent-offset blanks if the preceding
statement has `:' as its last significant (non-whitespace and non-
comment) character.  If the suggested indentation is too much, use
\\[py-electric-backspace] to reduce it.

Continuation lines are given extra indentation.  If you don't like the
suggested indentation, change it to something you do like, and Python-
mode will strive to indent later lines of the statement in the same way.

If a line is a continuation line by virtue of being in an unclosed
paren/bracket/brace structure (‘list’, for short), the suggested
indentation depends on whether the current line contains the first item
in the list.  If it does, it's indented py-indent-offset columns beyond
the indentation of the line containing the open bracket.  If you don't
like that, change it by hand.  The remaining items in the list will mimic
whatever indentation you give to the first item.

If a line is a continuation line because the line preceding it ends with
a backslash, the third and following lines of the statement inherit their
indentation from the line preceding them.  The indentation of the second
line in the statement depends on the form of the first (base) line:  if
the base line is an assignment statement with anything more interesting
than the backslash following the leftmost assigning `=', the second line
is indented two columns beyond that `='.  Else it's indented to two
columns beyond the leftmost solid chunk of non-whitespace characters on
the base line.

Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
repeatedly, and as explained above, \\[indent-for-tab-command] can't guess the block
structure you intend.
%c:indent-for-tab-command
%c:py-newline-and-indent
%c:py-electric-backspace

The next function may be handy when editing code you didn't write:
%c:py-guess-indent-offset

The remaining ‘indent’ functions apply to a region of Python code.  They
assume the block structure (equals indentation, in Python) of the region
is correct, and alter the indentation in various ways while preserving
the block structure:
%c:py-indent-region
%c:py-shift-left
%c:py-shift-right

@MARKING & MANIPULATING REGIONS OF CODE

\\[py-mark-block]\t mark block of lines
\\[py-mark-def-or-class]\t mark smallest enclosing def
\\[universal-argument] \\[py-mark-def-or-class]\t mark smallest enclosing class
\\[comment-region]\t comment out region of code
\\[universal-argument] \\[comment-region]\t uncomment region of code
%c:py-mark-block
%c:py-mark-def-or-class
%c:comment-region

@MOVING POINT

\\[py-previous-statement]\t move to statement preceding point
\\[py-next-statement]\t move to statement following point
\\[py-goto-block-up]\t move up to start of current block
\\[py-backward-def-or-class]\t move to start of def
\\[universal-argument] \\[py-backward-def-or-class]\t move to start of class
\\[py-forward-def-or-class]\t move to end of def
\\[universal-argument] \\[py-forward-def-or-class]\t move to end of class

The first two move to one statement beyond the statement that contains
point.  A numeric prefix argument tells them to move that many
statements instead.  Blank lines, comment lines, and continuation lines
do not count as ‘statements’ for these commands.  So, e.g., you can go
to the first code statement in a file by entering
\t\\[beginning-of-buffer]\t to move to the top of the file
\t\\[py-next-statement]\t to skip over initial comments and blank lines
Or do \\[py-previous-statement] with a huge prefix argument.
%c:py-previous-statement
%c:py-next-statement
%c:py-goto-block-up
%c:py-backward-def-or-class
%c:py-forward-def-or-class

@LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN PYTHON MODE

\\[indent-new-comment-line] is handy for entering a multi-line comment.

\\[set-selective-display] with a ‘small’ prefix arg is ideally suited for viewing the
overall class and def structure of a module.

`\\[back-to-indentation]' moves point to a line's first non-blank character.

`\\[indent-relative]' is handy for creating odd indentation.

@OTHER EMACS HINTS

If you don't like the default value of a variable, change its value to
whatever you do like by putting a ‘setq’ line in your .emacs file.
E.g., to set the indentation increment to 4, put this line in your
.emacs:
\t(setq py-indent-offset 4)
To see the value of a variable, do `\\[describe-variable]' and enter the variable
name at the prompt.

When entering a key sequence like `C-c C-n', it is not necessary to
release the CONTROL key after doing the `C-c' part -- it suffices to
press the CONTROL key, press and release ‘c’ (while still holding down
CONTROL), press and release ‘n’ (while still holding down CONTROL), &
then release CONTROL.

Entering Python mode calls with no arguments the value of the variable
‘python-mode-hook’, if that value exists and is not nil; for backward
compatibility it also tries ‘py-mode-hook’; see the ‘Hooks' section of
the Elisp manual for details.

Obscure:  When python-mode is first loaded, it looks for all bindings
to newline-and-indent in the global keymap, and shadows them with
local bindings to py-newline-and-indent."))

;;  (require 'info-look)
;;  The info-look package does not always provide this function (it
;;  appears this is the case with XEmacs 21.1)
(when (fboundp 'info-lookup-maybe-add-help)
  (info-lookup-maybe-add-help
   :mode 'python-mode
   :regexp "[a-zA-Z0-9_]+"
   :doc-spec '(("(python-lib)Module Index")
               ("(python-lib)Class-Exception-Object Index")
               ("(python-lib)Function-Method-Variable Index")
               ("(python-lib)Miscellaneous Index"))))

(defun py--find-definition-in-source (sourcefile symbol)
  (called-interactively-p 'any) (message "sourcefile: %s" sourcefile)
  (when (find-file sourcefile)
    (goto-char (point-min))
    (when
	(or (re-search-forward (concat py-def-or-class-re symbol) nil t 1)
	    (progn
	      ;; maybe a variable definition?
	      (goto-char (point-min))
	      (re-search-forward (concat "^.+ " symbol) nil t 1)))
      (push-mark)
      (goto-char (match-beginning 0))
      (exchange-point-and-mark))))

;;  Find function stuff, lifted from python.el
(defalias 'py-find-function 'py-find-definition)
(defun py--find-definition-question-type (symbol imports)
  (let (erg)
    (cond ((setq erg (py-execute-string (concat "import inspect;inspect.isbuiltin(\"" symbol "\")"))))
	  (t (setq erg (py-execute-string (concat imports "import inspect;inspect.getmodule(\"" symbol "\")")))))
    erg))

(defun py-find-definition (&optional symbol)
  "Find source of definition of SYMBOL.

Interactively, prompt for SYMBOL."
  (interactive)
  ;; (set-register 98888888 (list (current-window-configuration) (point-marker)))
  (let* (;; end
	 ;; (last-window-configuration
         ;;  (current-window-configuration))
	 (orig (point))
         ;; (exception-buffer (current-buffer))
         (imports (py-find-imports))
         (symbol-raw (or symbol (with-syntax-table py-dotted-expression-syntax-table
				  (current-word))))
         ;; (enable-recursive-minibuffers t)
         (symbol (if (called-interactively-p 'interactive)
		     (read-string (format "Find location of (default %s): " symbol-raw)
		                  symbol-raw nil symbol-raw)
		   symbol-raw))
         (local (progn (goto-char (point-min)) (re-search-forward (concat "^[ \t]*" "\\(def\\|class\\)" "[ \t]" symbol) orig t))))
    ;; ismethod(), isclass(), isfunction() or isbuiltin()
    ;; ismethod isclass isfunction isbuiltin)
    (if local
        (progn
	  (goto-char orig)
	  (split-window-vertically)
	  (other-buffer)
	  (goto-char local)
	  (beginning-of-line)
          (push-mark)
	  (message "%s" (current-buffer))
	  (exchange-point-and-mark))
      (with-help-window (help-buffer)
	(princ (py--find-definition-question-type symbol imports))))))

(defun py-update-imports ()
  "Return imports.

Imports done are displayed in message buffer."
  (interactive)
  (save-excursion
    (let ((orig (point))
          (erg (py-find-imports)))
      (goto-char orig)
      erg)))

;;  Code-Checker
;;  pep8
(defalias 'pep8 'py-pep8-run)
(defun py-pep8-run (command)
  "*Run pep8 using COMMAND, check formatting.
Default on the file currently visited."
  (interactive
   (let ((default
           (if (py--buffer-filename-remote-maybe)
               (format "%s %s %s" py-pep8-command
                       (mapconcat 'identity py-pep8-command-args " ")
                       (py--buffer-filename-remote-maybe))
             (format "%s %s" py-pep8-command
                     (mapconcat 'identity py-pep8-command-args " "))))
         (last (when py-pep8-history
                 (let* ((lastcmd (car py-pep8-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pep8 like this: "
                              (if last
                                  last
                                default)
                              'py-pep8-history)
        (read-string "Run pep8 like this: "
                     (if last
                         last
                       default)
                     'py-pep8-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-pep8-help ()
  "Display pep8 command line help messages."
  (interactive)
  (set-buffer (get-buffer-create "*pep8-Help*"))
  (erase-buffer)
  (shell-command "pep8 --help" "*pep8-Help*"))

;;  Pylint
(defalias 'pylint 'py-pylint-run)
(defun py-pylint-run (command)
  "Run pylint from COMMAND.

Default on the file currently visited.

For help see \\[pylint-help] resp. \\[pylint-long-help].
Home-page: http://www.logilab.org/project/pylint"
  (interactive
   (let ((default (format "%s %s %s" py-pylint-command
			  (mapconcat 'identity py-pylint-command-args " ")
			  (py--buffer-filename-remote-maybe)))
         (last (and py-pylint-history (car py-pylint-history))))
     (list (funcall (if (fboundp 'read-shell-command)
			'read-shell-command 'read-string)
		    "Run pylint like this: "
		    (or default last)
		    'py-pylint-history))))
    (save-some-buffers (not py-ask-about-save))
  (set-buffer (get-buffer-create "*Pylint*"))
  (erase-buffer)
  (unless (file-readable-p (car (cddr (split-string command))))
    (message "Warning: %s" "pylint needs a file"))
  (shell-command command "*Pylint*"))

(defalias 'pylint-help 'py-pylint-help)
(defun py-pylint-help ()
  "Display Pylint command line help messages.

Let's have this until more Emacs-like help is prepared"
  (interactive)
  (set-buffer (get-buffer-create "*Pylint-Help*"))
  (erase-buffer)
  (shell-command "pylint --long-help" "*Pylint-Help*"))

(defalias 'pylint-doku 'py-pylint-doku)
(defun py-pylint-doku ()
  "Display Pylint Documentation.

Calls `pylint --full-documentation'"
  (interactive)
  (set-buffer (get-buffer-create "*Pylint-Documentation*"))
  (erase-buffer)
  (shell-command "pylint --full-documentation" "*Pylint-Documentation*"))

;;  Pyflakes
(defalias 'pyflakes 'py-pyflakes-run)
(defun py-pyflakes-run (command)
  "*Run pyflakes on COMMAND.

Default on the file currently visited.

For help see \\[pyflakes-help] resp. \\[pyflakes-long-help].
Home-page: http://www.logilab.org/project/pyflakes"
  (interactive
   (let ((default
           (if (py--buffer-filename-remote-maybe)
               (format "%s %s %s" py-pyflakes-command
                       (mapconcat 'identity py-pyflakes-command-args " ")
                       (py--buffer-filename-remote-maybe))
             (format "%s %s" py-pyflakes-command
                     (mapconcat 'identity py-pyflakes-command-args " "))))
         (last (when py-pyflakes-history
                 (let* ((lastcmd (car py-pyflakes-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pyflakes like this: "
                              (if last
                                  last
                                default)
                              'py-pyflakes-history)
        (read-string "Run pyflakes like this: "
                     (if last
                         last
                       default)
                     'py-pyflakes-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defalias 'pyflakes-help 'py-pyflakes-help)
(defun py-pyflakes-help ()
  "Display Pyflakes command line help messages."
  (interactive)
  ;; (set-buffer (get-buffer-create "*Pyflakes-Help*"))
  ;; (erase-buffer)
  (with-help-window "*Pyflakes-Help*"
    (with-current-buffer standard-output
      (insert "       pyflakes [file-or-directory ...]

       Pyflakes is a simple program which checks Python
       source files for errors. It is similar to
       PyChecker in scope, but differs in that it does
       not execute the modules to check them. This is
       both safer and faster, although it does not
       perform as many checks. Unlike PyLint, Pyflakes
       checks only for logical errors in programs; it
       does not perform any checks on style.

       All commandline arguments are checked, which
       have to be either regular files or directories.
       If a directory is given, every .py file within
       will be checked.

       When no commandline arguments are given, data
       will be read from standard input.

       The exit status is 0 when no warnings or errors
       are found. When errors are found the exit status
       is 2. When warnings (but no errors) are found
       the exit status is 1.

Extracted from http://manpages.ubuntu.com/manpages/natty/man1/pyflakes.1.html"))))

;;  Pyflakes-pep8
(defalias 'pyflakespep8 'py-pyflakespep8-run)
(defun py-pyflakespep8-run (command)
  "*Run COMMAND pyflakespep8, check formatting.

Default on the file currently visited."
  (interactive
   (let ((default
           (if (py--buffer-filename-remote-maybe)
               (format "%s %s %s" py-pyflakespep8-command
                       (mapconcat 'identity py-pyflakespep8-command-args " ")
                       (py--buffer-filename-remote-maybe))
             (format "%s %s" py-pyflakespep8-command
                     (mapconcat 'identity py-pyflakespep8-command-args " "))))
         (last (when py-pyflakespep8-history
                 (let* ((lastcmd (car py-pyflakespep8-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pyflakespep8 like this: "
                              (if last
                                  last
                                default)
                              'py-pyflakespep8-history)
        (read-string "Run pyflakespep8 like this: "
                     (if last
                         last
                       default)
                     'py-pyflakespep8-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-pyflakespep8-help ()
  "Display pyflakespep8 command line help messages."
  (interactive)
  (set-buffer (get-buffer-create "*pyflakespep8-Help*"))
  (erase-buffer)
  (shell-command "pyflakespep8 --help" "*pyflakespep8-Help*"))

;;  Pychecker
;;  hack for GNU Emacs
;;  (unless (fboundp 'read-shell-command)
;;  (defalias 'read-shell-command 'read-string))

(defun py-pychecker-run (command)
  "Run COMMAND pychecker (default on the file currently visited)."
  (interactive
   (let ((default
           (if (py--buffer-filename-remote-maybe)
               (format "%s %s %s" py-pychecker-command
		       py-pychecker-command-args
		       (py--buffer-filename-remote-maybe))
             (format "%s %s" py-pychecker-command py-pychecker-command-args)))
         (last (when py-pychecker-history
                 (let* ((lastcmd (car py-pychecker-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pychecker like this: "
                              (if last
                                  last
                                default)
                              'py-pychecker-history)
        (read-string "Run pychecker like this: "
                     (if last
                         last
                       default)
                     'py-pychecker-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

;;  After ‘sgml-validate-command’.
(defun py-check-command (command)
  "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by ‘compile’.
See ‘py-check-command’ for the default."
  (interactive
   (list (read-string "Checker command: "
                      (concat py-check-command " "
                              (let ((name (py--buffer-filename-remote-maybe)))
                                (if name
                                    (file-name-nondirectory name)))))))
  (require 'compile)                    ;To define compilation-* variables.
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((compilation-error-regexp-alist py-compilation-regexp-alist)
	;; (cons '("(\\([^,]+\\), line \\([0-9]+\\))" 1)
	;; compilation-error-regexp-alist)
	)
    (compilation-start command)))

;;  flake8
(defalias 'flake8 'py-flake8-run)
(defun py-flake8-run (command)
  "COMMAND Flake8 is a wrapper around these tools:
- PyFlakes
        - pep8
        - Ned Batchelder's McCabe script

        It also adds features:
        - files that contain this line are skipped::
            # flake8: noqa
        - no-warn lines that contain a `# noqa`` comment at the end.
        - a Git and a Mercurial hook.
        - a McCabe complexity checker.
        - extendable through ``flake8.extension`` entry points."
  (interactive
   (let* ((py-flake8-command
           (if (string= "" py-flake8-command)
               (or (executable-find "flake8")
                   (error "Don't see \"flake8\" on your system.
Consider \"pip install flake8\" resp. visit \"pypi.python.org\""))
             py-flake8-command))
          (default
            (if (py--buffer-filename-remote-maybe)
                (format "%s %s %s" py-flake8-command
                        py-flake8-command-args
                        (py--buffer-filename-remote-maybe))
              (format "%s %s" py-flake8-command
                      py-flake8-command-args)))
          (last
           (when py-flake8-history
             (let* ((lastcmd (car py-flake8-history))
                    (cmd (cdr (reverse (split-string lastcmd))))
                    (newcmd (reverse (cons (py--buffer-filename-remote-maybe) cmd))))
               (mapconcat 'identity newcmd " ")))))
     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run flake8 like this: "
                              ;; (if last
                              ;; last
                              default
                              'py-flake8-history1)
        (read-string "Run flake8 like this: "
                     (if last
                         last
                       default)
                     'py-flake8-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-flake8-help ()
  "Display flake8 command line help messages."
  (interactive)
  (set-buffer (get-buffer-create "*flake8-Help*"))
  (erase-buffer)
  (shell-command "flake8 --help" "*flake8-Help*"))

;;  from string-strip.el --- Strip CHARS from STRING

(defun py-nesting-level (&optional pps)
  "Accepts the output of ‘parse-partial-sexp’ - PPS."
  (interactive)
  (let* ((pps (or (ignore-errors (nth 0 pps))
                  (if (featurep 'xemacs)
                      (parse-partial-sexp (point-min) (point))
                    (parse-partial-sexp (point-min) (point)))))
         (erg (nth 0 pps)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

;;  Flymake
(defun py-toggle-flymake-intern (name command)
  "Clear flymake allowed file-name masks.

Takes NAME COMMAND"
  (unless (string-match "pyflakespep8" name)
    (unless (executable-find name)
      (when py-verbose-p (message "Don't see %s. Use ‘easy_install’ %s? " name name))))
  (if (py--buffer-filename-remote-maybe)
      (let* ((temp-file (if (functionp 'flymake-proc-init-create-temp-buffer-copy)
			    (flymake-proc-init-create-temp-buffer-copy 'flymake-create-temp-inplace)
			  (flymake-proc-init-create-temp-buffer-copy 'flymake-create-temp-inplace)
			  ))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory (py--buffer-filename-remote-maybe)))))
	(if (boundp 'flymake-proc-allowed-file-name-masks)
            (push (car (read-from-string (concat "(\"\\.py\\'\" flymake-" name ")"))) flymake-proc-allowed-file-name-masks)
	  (push (car (read-from-string (concat "(\"\\.py\\'\" flymake-" name ")"))) flymake-proc-allowed-file-name-masks))
        (list command (list local-file)))
    (message "%s" "flymake needs a ‘file-name’. Please save before calling.")))

(defun pylint-flymake-mode ()
  "Toggle ‘pylint’ ‘flymake-mode’."
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode 0)
    (py-toggle-flymake-intern "pylint" "pylint")
    (flymake-mode 1)))

(defun pyflakes-flymake-mode ()
  "Toggle ‘pyflakes’ ‘flymake-mode’."
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pyflakes" "pyflakes")
    (flymake-mode)))

(defun pychecker-flymake-mode ()
  "Toggle ‘pychecker’ ‘flymake-mode’."
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pychecker" "pychecker")
    (flymake-mode)))

(defun pep8-flymake-mode ()
  "Toggle `pep8’ ‘flymake-mode’."
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pep8" "pep8")
    (flymake-mode)))

(defun pyflakespep8-flymake-mode ()
  "Toggle `pyflakespep8’ ‘flymake-mode’.

Joint call to pyflakes and pep8 as proposed by
Keegan Carruthers-Smith"
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pyflakespep8" "pyflakespep8")
    (flymake-mode)))

(defun py-display-state-of-variables ()
  "Read the state of ‘python-mode’ variables.

Assumes vars are defined in current source buffer"
  (interactive)
  (save-restriction
    (let (variableslist)
      (goto-char (point-min))
      ;; (eval-buffer)
      (while (and (not (eobp))(re-search-forward "^(defvar [[:alpha:]]\\|^(defcustom [[:alpha:]]\\|^(defconst [[:alpha:]]" nil t 1))
        (let* ((name (symbol-at-point))
               (state
                (unless
                    (or (eq name 'py-menu)
                        (eq name 'python-mode-map)
                        (string-match "syntax-table" (prin1-to-string name)))

                  (prin1-to-string (symbol-value name)))))
          (if state
              (push (cons (prin1-to-string name) state) variableslist)
            (message "don't see a state for %s" (prin1-to-string name))))
        (forward-line 1))
      (setq variableslist (nreverse variableslist))
      (set-buffer (get-buffer-create "State-of-Python-mode-variables.org"))
      (erase-buffer)
      ;; org
      (insert "State of python-mode variables\n\n")
      (switch-to-buffer (current-buffer))
      (dolist (ele variableslist)
        (if (string-match "^;;; " (car ele))
            (unless (or (string-match "^;;; Constants\\|^;;; Commentary\\|^;;; Code\\|^;;; Macro definitions\\|^;;; Customization" (car ele)))

              (insert (concat (replace-regexp-in-string "^;;; " "* " (car ele)) "\n")))
          (insert (concat "\n** "(car ele) "\n"))
          (insert (concat "   " (cdr ele) "\n\n")))
        ;; (richten)
        (sit-for 0.01 t))
      (sit-for 0.01 t))))

;; common typo
(defalias 'iypthon 'ipython)
(defalias 'pyhton 'python)

;; python-components-extensions

(defun py-indent-forward-line (&optional arg)
  "Indent and move line forward to next indentation.
Returns column of line reached.

If ‘py-kill-empty-line’ is non-nil, delete an empty line.

With \\[universal argument] just indent.
"
  (interactive "*P")
  (let ((orig (point))
        erg)
    (unless (eobp)
      (if (and (py--in-comment-p)(not py-indent-comments))
          (forward-line 1)
        (py-indent-line-outmost)
        (unless (eq 4 (prefix-numeric-value arg))
          (if (eobp) (newline)
            (progn (forward-line 1))
            (when (and py-kill-empty-line (py-empty-line-p) (not (looking-at "[ \t]*\n[[:alpha:]]")) (not (eobp)))
              (delete-region (line-beginning-position) (line-end-position)))))))
    (back-to-indentation)
    (when (or (eq 4 (prefix-numeric-value arg)) (< orig (point))) (setq erg (current-column)))
    erg))

(defun py-dedent-forward-line (&optional arg)
  "Dedent line and move one line forward. "
  (interactive "*p")
  (py-dedent arg)
  (if (eobp)
      (newline 1)
    (forward-line 1))
  (end-of-line))

(defun py-dedent (&optional arg)
  "Dedent line according to ‘py-indent-offset’.

With arg, do it that many times.
If point is between indent levels, dedent to next level.
Return indentation reached, if dedent done, nil otherwise.

Affected by ‘py-dedent-keep-relative-column’. "
  (interactive "*p")
  (or arg (setq arg 1))
  (let ((orig (copy-marker (point)))
        erg)
    (dotimes (_ arg)
      (let* ((cui (current-indentation))
             (remain (% cui py-indent-offset))
             (indent (* py-indent-offset (/ cui py-indent-offset))))
        (beginning-of-line)
        (fixup-whitespace)
        (if (< 0 remain)
            (indent-to-column indent)
          (indent-to-column (- cui py-indent-offset)))))
    (when (< (point) orig)
      (setq erg (current-column)))
    (when py-dedent-keep-relative-column (goto-char orig))
    erg))

(defun py-class-at-point ()
  "Return class definition as string. "
  (interactive)
  (save-excursion
    (let* ((beg (py-backward-class))
	   (end (py-forward-class))
	   (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
      res)))

(defun py-backward-function ()
  "Jump to the beginning of defun.

Returns position. "
  (interactive "p")
  (py-backward-def-or-class))

(defun py-forward-function ()
  "Jump to the end of function.

Returns position."
  (interactive "p")
  (py-forward-def-or-class))

(defun py-function-at-point ()
  "Return functions definition as string. "
  (interactive)
  (save-excursion
    (let* ((beg (py-backward-function))
	   (end (py-forward-function)))
      (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end)))))

;; Functions for marking regions

(defun py-line-at-point ()
  "Return line as string. "
  (interactive)
  (let* ((beg (line-beginning-position))
	 (end (line-end-position)))
    (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))

(defun py-match-paren-mode (&optional arg)
  "py-match-paren-mode nil oder t"
  (interactive "P")
  (if (or arg (not py-match-paren-mode))
      (progn
	(setq py-match-paren-mode t)
        (setq py-match-paren-mode nil))))

(defun py--match-end-finish (cui)
  (let (skipped)
    (unless (eq (current-column) cui)
      (when (< (current-column) cui)
	(setq skipped (skip-chars-forward " \t" (line-end-position)))
	(setq cui (- cui skipped))
	;; may current-column greater as needed indent?
	(if (< 0 cui)
	    (progn
	      (unless (py-empty-line-p) (split-line))
	      (indent-to cui))
	  (forward-char cui))
	(unless (eq (char-before) 32)(insert 32)(forward-char -1))))))

(defun py--match-paren-forward ()
  (setq py--match-paren-forward-p t)
  (let ((cui (current-indentation)))
    (cond
     ((py--beginning-of-top-level-p)
      (py-forward-top-level-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-class-p)
      (py-forward-class-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-def-p)
      (py-forward-def-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-if-block-p)
      (py-forward-if-block-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-try-block-p)
      (py-forward-try-block-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-for-block-p)
      (py-forward-for-block-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-block-p)
      (py-forward-block-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-clause-p)
      (py-forward-clause-bol)
      (py--match-end-finish cui))
     ((py--beginning-of-statement-p)
      (py-forward-statement-bol)
      (py--match-end-finish cui))
     (t (py-forward-statement)
	(py--match-end-finish cui)))))

(defun py--match-paren-backward ()
  (setq py--match-paren-forward-p nil)
  (let* ((cui (current-indentation))
	 (cuc (current-column))
	 (cui (min cuc cui)))
    (if (eq 0 cui)
	(py-backward-top-level)
      (when (py-empty-line-p) (delete-region (line-beginning-position) (point)))
      (py-backward-statement)
      (unless (< (current-column) cuc)
      (while (and (not (bobp))
		  (< cui (current-column))
		  (py-backward-statement)))))))

(defun py--match-paren-blocks ()
  (cond
   ((and (looking-back "^[ \t]*" (line-beginning-position))(if (eq last-command 'py-match-paren)(not py--match-paren-forward-p)t)
	 ;; (looking-at py-extended-block-or-clause-re)
	 (looking-at "[[:alpha:]_]"))
    ;; from beginning of top-level, block, clause, statement
    (py--match-paren-forward))
   (t
    (py--match-paren-backward))))

(defun py-match-paren (&optional arg)
  "If at a beginning, jump to end and vice versa.

When called from within, go to the start.
Matches lists, but also block, statement, string and comment. "
  (interactive "*P")
  (if (eq 4 (prefix-numeric-value arg))
      (insert "%")
    (let ((pps (parse-partial-sexp (point-min) (point))))
      (cond
       ;; if inside string, go to beginning
       ((nth 3 pps)
	(goto-char (nth 8 pps)))
       ;; if inside comment, go to beginning
       ((nth 4 pps)
	(py-backward-comment))
       ;; at comment start, go to end of commented section
       ((and
	 ;; unless comment starts where jumped to some end
	 (not py--match-paren-forward-p)
	 (eq 11 (car-safe (syntax-after (point)))))
	(py-forward-comment))
       ;; at string start, go to end
       ((or (eq 15 (car-safe (syntax-after (point))))
	    (eq 7 (car (syntax-after (point)))))
	(goto-char (scan-sexps (point) 1))
	(forward-char -1))
       ;; open paren
       ((eq 4 (car (syntax-after (point))))
	(goto-char (scan-sexps (point) 1))
	(forward-char -1))
       ((eq 5 (car (syntax-after (point))))
	(goto-char (scan-sexps (1+ (point)) -1)))
       ((nth 1 pps)
	(goto-char (nth 1 pps)))
       (t
	;; Python specific blocks
	(py--match-paren-blocks))))))

(unless (functionp 'in-string-p)
  (defun in-string-p (&optional pos)
    (interactive)
    (let ((orig (or pos (point))))
      (save-excursion
        (save-restriction
          (widen)
          (beginning-of-defun)
          (numberp
           (progn
             (if (featurep 'xemacs)
                 (nth 3 (parse-partial-sexp (point) orig)
                      (nth 3 (parse-partial-sexp (point-min) (point))))))))))))

(defun py-documentation (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (py-symbol-at-point))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) "" (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word) (error "No pydoc args given")
                 word) ;sinon word
             input)))) ;sinon input
  (shell-command (concat py-shell-name " -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

(defun pst-here ()
  "Kill previous \"pdb.set_trace()\" and insert it at point. "
  (interactive "*")
  (let ((orig (copy-marker (point))))
    (search-backward "pdb.set_trace()")
    (replace-match "")
    (when (py-empty-line-p)
      (delete-region (line-beginning-position) (line-end-position)))
    (goto-char orig)
    (insert "pdb.set_trace()")))

(defun py-printform-insert (&optional arg strg)
  "Inserts a print statement from `(car kill-ring)'.

With optional \\[universal-argument] print as string"
  (interactive "*P")
  (let* ((name (py--string-strip (or strg (car kill-ring))))
         ;; guess if doublequotes or parentheses are needed
         (numbered (not (eq 4 (prefix-numeric-value arg))))
         (form (if numbered
		   (concat "print(\"" name ": %s \" % (" name "))")
		 (concat "print(\"" name ": %s \" % \"" name "\")"))))
    (insert form)))

(defun py-print-formatform-insert (&optional strg)
  "Inserts a print statement out of current `(car kill-ring)' by default.

print(\"\\nfoo: {}\"\.format(foo))"
  (interactive "*")
  (let ((name (py--string-strip (or strg (car kill-ring)))))
    (insert (concat "print(\"" name ": {}\".format(" name "))"))))

(defun py-line-to-printform-python2 ()
  "Transforms the item on current in a print statement. "
  (interactive "*")
  (let* ((name (py-symbol-at-point))
         (form (concat "print(\"" name ": %s \" % " name ")")))
    (delete-region (line-beginning-position) (line-end-position))
    (insert form))
  (forward-line 1)
  (back-to-indentation))

(defun py-boolswitch ()
  "Edit the assignment of a boolean variable, revert them.

I.e. switch it from \"True\" to \"False\" and vice versa"
  (interactive "*")
  (save-excursion
    (unless (py--end-of-statement-p)
      (py-forward-statement))
    (backward-word)
    (cond ((looking-at "True")
           (replace-match "False"))
          ((looking-at "False")
           (replace-match "True"))
          (t (message "%s" "Can't see \"True or False\" here")))))

;; python-components-imenu
;; Imenu definitions

(defvar py-imenu-class-regexp
  (concat                               ; <<classes>>
   "\\("                                ;
   "^[ \t]*"                            ; newline and maybe whitespace
   "\\(class[ \t]+[a-zA-Z0-9_]+\\)"     ; class name
                                        ; possibly multiple superclasses
   "\\([ \t]*\\((\\([a-zA-Z0-9_,. \t\n]\\)*)\\)?\\)"
   "[ \t]*:"                            ; and the final :
   "\\)"                                ; >>classes<<
   )
  "Regexp for Python classes for use with the Imenu package."
  )

;; (defvar py-imenu-method-regexp
;;   (concat                               ; <<methods and functions>>
;;    "\\("                                ;
;;    "^[ \t]*"                            ; new line and maybe whitespace
;;    "\\(def[ \t]+"                       ; function definitions start with def
;;    "\\([a-zA-Z0-9_]+\\)"                ;   name is here
;;                                         ;   function arguments...
;;    ;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
;;    "[ \t]*(\\([^:#]*\\))"
;;    "\\)"                                ; end of def
;;    "[ \t]*:"                            ; and then the :
;;    "\\)"                                ; >>methods and functions<<
;;    )
;;   "Regexp for Python methods/functions for use with the Imenu package."
;;   )

(defvar py-imenu-method-regexp
  (concat                               ; <<methods and functions>>
   "\\("                                ;
   "^[ \t]*"                            ; new line and maybe whitespace
   "\\(def[ \t]+"                       ; function definitions start with def
   "\\([a-zA-Z0-9_]+\\)"                ;   name is here
                                        ;   function arguments...
   ;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
   "[ \t]*(\\(.*\\))"
   "\\)"                                ; end of def
   "[ \t]*:"                            ; and then the :
   "\\)"                                ; >>methods and functions<<
   )
  "Regexp for Python methods/functions for use with the Imenu package.")





(defvar py-imenu-method-no-arg-parens '(2 8)
  "Indices into groups of the Python regexp for use with Imenu.

Using these values will result in smaller Imenu lists, as arguments to
functions are not listed.

See the variable ‘py-imenu-show-method-args-p’ for more
information.")

(defvar py-imenu-method-arg-parens '(2 7)
  "Indices into groups of the Python regexp for use with imenu.
Using these values will result in large Imenu lists, as arguments to
functions are listed.

See the variable ‘py-imenu-show-method-args-p’ for more
information.")

;; Note that in this format, this variable can still be used with the
;; imenu--generic-function. Otherwise, there is no real reason to have
;; it.
(defvar py-imenu-generic-expression
  (cons
   (concat
    py-imenu-class-regexp
    "\\|"                               ; or...
    py-imenu-method-regexp
    )
   py-imenu-method-no-arg-parens)
  "Generic Python expression which may be used directly with Imenu.
Used by setting the variable ‘imenu-generic-expression’ to this value.
Also, see the function \\[py--imenu-create-index] for a better
alternative for finding the index.")


(defvar py-imenu-generic-regexp nil)
(defvar py-imenu-generic-parens nil)


(defun py--imenu-create-index ()
  "Python interface function for the Imenu package.
Finds all Python classes and functions/methods. Calls function
\\[py--imenu-create-index-engine].  See that function for the details
of how this works."
  (let (index-alist)
    (save-excursion
      (setq py-imenu-generic-regexp (car py-imenu-generic-expression)
	    py-imenu-generic-parens (if py-imenu-show-method-args-p
				        py-imenu-method-arg-parens
				      py-imenu-method-no-arg-parens))
      (goto-char (point-min))
      ;; Warning: When the buffer has no classes or functions, this will
      ;; return nil, which seems proper according to the Imenu API, but
      ;; causes an error in the XEmacs port of Imenu.  Sigh.
      (setq index-alist (cdr (py--imenu-create-index-engine nil))))))

(defun py--imenu-create-index-engine (&optional start-indent)
  "Function for finding Imenu definitions in Python.

Finds all definitions (classes, methods, or functions) in a Python
file for the Imenu package.

Returns a possibly nested alist of the form

        (INDEX-NAME . INDEX-POSITION)

The second element of the alist may be an alist, producing a nested
list as in

        (INDEX-NAME . INDEX-ALIST)

This function should not be called directly, as it calls itself
recursively and requires some setup.  Rather this is the engine for
the function \\[py--imenu-create-index-function].

It works recursively by looking for all definitions at the current
indention level.  When it finds one, it adds it to the alist.  If it
finds a definition at a greater indentation level, it removes the
previous definition from the alist. In its place it adds all
definitions found at the next indentation level.  When it finds a
definition that is less indented then the current level, it returns
the alist it has created thus far.

The optional argument START-INDENT indicates the starting indentation
at which to continue looking for Python classes, methods, or
functions.  If this is not supplied, the function uses the indentation
of the first definition found."
  (let (index-alist
        sub-method-alist
        looking-p
        def-name prev-name
        cur-indent def-pos
        (class-paren (first py-imenu-generic-parens))
        (def-paren (second py-imenu-generic-parens)))
    ;; (switch-to-buffer (current-buffer))
    (setq looking-p
          (re-search-forward py-imenu-generic-regexp (point-max) t))
    (while looking-p
      (save-excursion
        ;; used to set def-name to this value but generic-extract-name
        ;; is new to imenu-1.14. this way it still works with
        ;; imenu-1.11
        ;;(imenu--generic-extract-name py-imenu-generic-parens))
        (let ((cur-paren (if (match-beginning class-paren)
                             class-paren def-paren)))
          (setq def-name
                (buffer-substring-no-properties (match-beginning cur-paren)
                                                (match-end cur-paren))))
        (save-match-data
          (py-backward-def-or-class))
        (beginning-of-line)
        (setq cur-indent (current-indentation)))
      ;; HACK: want to go to the next correct definition location.  We
      ;; explicitly list them here but it would be better to have them
      ;; in a list.
      (setq def-pos
            (or (match-beginning class-paren)
                (match-beginning def-paren)))
      ;; if we don't have a starting indent level, take this one
      (or start-indent
          (setq start-indent cur-indent))
      ;; if we don't have class name yet, take this one
      (or prev-name
          (setq prev-name def-name))
      ;; what level is the next definition on?  must be same, deeper
      ;; or shallower indentation
      (cond
       ;; Skip code in comments and strings
       ((py--in-literal))
       ;; at the same indent level, add it to the list...
       ((= start-indent cur-indent)
        (push (cons def-name def-pos) index-alist))
       ;; deeper indented expression, recurse
       ((< start-indent cur-indent)
        ;; the point is currently on the expression we're supposed to
        ;; start on, so go back to the last expression. The recursive
        ;; call will find this place again and add it to the correct
        ;; list
        (re-search-backward py-imenu-generic-regexp (point-min) 'move)
        (setq sub-method-alist (py--imenu-create-index-engine cur-indent))
        (if sub-method-alist
            ;; we put the last element on the index-alist on the start
            ;; of the submethod alist so the user can still get to it.
            (let* ((save-elmt (pop index-alist))
                   (classname (and (string-match "^class " (car save-elmt))(replace-regexp-in-string "^class " "" (car save-elmt)))))
              (if (and classname (not (string-match "^class " (caar sub-method-alist))))
                  (setcar (car sub-method-alist) (concat classname "." (caar sub-method-alist))))
              (push (cons prev-name
                          (cons save-elmt sub-method-alist))
                    index-alist))))
       (t
        (setq looking-p nil)
        (re-search-backward py-imenu-generic-regexp (point-min) t)))
      ;; end-cond
      (setq prev-name def-name)
      (and looking-p
           (setq looking-p
                 (re-search-forward py-imenu-generic-regexp
                                    (point-max) 'move))))
    (nreverse index-alist)))

(defun py--imenu-create-index-new (&optional beg end)
  "‘imenu-create-index-function’ for Python. "
  (interactive)
  (set (make-local-variable 'imenu-max-items) py-imenu-max-items)
  (let ((orig (point))
        (beg (or beg (point-min)))
        (end (or end (point-max)))
        index-alist vars thisend sublist classname pos name)
    (goto-char beg)
    (while (and (re-search-forward "^[ \t]*\\(def\\|class\\)[ \t]+\\(\\sw+\\)" end t 1)(not (nth 8 (parse-partial-sexp (point-min) (point)))))
      (if (save-match-data (string= "class" (match-string-no-properties 1)))
          (progn
            (setq pos (match-beginning 0)
                  name (match-string-no-properties 2)
                  classname (concat "class " name)
                  thisend (save-match-data (py--end-of-def-or-class-position))
                  sublist '())
            (while (and (re-search-forward "^[ \t]*\\(def\\|class\\)[ \t]+\\(\\sw+\\)" (or thisend end) t 1)(not (nth 8 (parse-partial-sexp (point-min) (point)))))
              (let* ((pos (match-beginning 0))
                     (name (match-string-no-properties 2)))
		(push (cons (concat " " name) pos) sublist)))
            (if classname
                (progn
                  (setq sublist (nreverse sublist))
                  (push (cons classname pos) sublist)
                  (push (cons classname sublist) index-alist))
              (push sublist index-alist)))

        (let ((pos (match-beginning 0))
              (name (match-string-no-properties 2)))
          (push (cons name pos) index-alist))))
    ;; Look for module variables.
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\sw+\\)[ \t]*=" end t)
      (unless (nth 8 (parse-partial-sexp (point-min) (point)))
        (let ((pos (match-beginning 1))
              (name (match-string-no-properties 1)))
          (push (cons name pos) vars))))
    (setq index-alist (nreverse index-alist))
    (when vars
      (push (cons "Module variables"
                  (nreverse vars))
            index-alist))
    (goto-char orig)
    index-alist))

;; A modified slice from python.el
(defvar py-imenu-format-item-label-function
  'py-imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar py-imenu-format-parent-item-label-function
  'py-imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar py-imenu-format-parent-item-jump-label-function
  'py-imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun py-imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s (%s)" name type))

(defun py-imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s..." (py-imenu-format-item-label type name)))

;; overengineering?
(defun py-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (if (string= type "class")
      "*class definition*"
    "*function definition*"))

(defun py-imenu--put-parent (type name pos tree)
  "Add the parent with TYPE, NAME and POS to TREE."
  (let* ((label
         (funcall py-imenu-format-item-label-function type name))
        ;; (jump-label
	;; (funcall py-imenu-format-parent-item-jump-label-function type name))
	(jump-label label
         ;; (funcall py-imenu-format-parent-item-jump-label-function type name)
	 )
	)
    (if (not tree)
        (cons label pos)
      (cons label (cons (cons jump-label pos) tree)))))

(defun py-imenu--build-tree (&optional min-indent prev-indent tree)
  "Recursively build the tree of nested definitions of a node.
Arguments MIN-INDENT, PREV-INDENT and TREE are internal and should
not be passed explicitly unless you know what you are doing."
  (setq min-indent (or min-indent 0)
        prev-indent (or prev-indent py-indent-offset))
  (save-restriction
    (narrow-to-region (point-min) (point))
    (let* ((pos
	    (progn
	      ;; finds a top-level class
	      (py-backward-def-or-class)
	      ;; stops behind the indented form at EOL
	      (py-forward-def-or-class)
	      ;; may find an inner def-or-class
	      (py-backward-def-or-class)))
	   type
	   (name (when (and pos (looking-at py-def-or-class-re))
		   (let ((split (split-string (match-string-no-properties 0))))
		     (setq type (car split))
		     (cadr split))))
	   (label (when name
		    (funcall py-imenu-format-item-label-function type name)))
	   (indent (current-indentation))
	   (children-indent-limit (+ py-indent-offset min-indent)))
      (cond ((not pos)
	     ;; Nothing found, probably near to bobp.
	     nil)
	    ((<= indent min-indent)
	     ;; The current indentation points that this is a parent
	     ;; node, add it to the tree and stop recursing.
	     (py-imenu--put-parent type name pos tree))
	    (t
	     (py-imenu--build-tree
	      min-indent
	      indent
	      (if (<= indent children-indent-limit)
		  (cons (cons label pos) tree)
		(cons
		 (py-imenu--build-tree
		  prev-indent indent (list (cons label pos)))
		 tree))))))))

(defun py--imenu-index ()
  "Return tree Imenu alist for the current Python buffer. "
  (save-excursion
    (goto-char (point-max))
    (let ((index)
	  (tree))
      (while (setq tree (py-imenu--build-tree))
	(setq index (cons tree index)))
      index)))

;; python-components-electric
(defun py-electric-colon (arg)
  "Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix \\[universal-argument].

Switched by ‘py-electric-colon-active-p’, default is nil
See also ‘py-electric-colon-greedy-p’"
  (interactive "*P")
  (cond
   ((not py-electric-colon-active-p)
    (self-insert-command (prefix-numeric-value arg)))
   ;;
   ((and py-electric-colon-bobl-only
         (save-excursion
           (py-backward-statement)
           (not (py--beginning-of-block-p))))
    (self-insert-command (prefix-numeric-value arg)))
   ;;
   ((eq 4 (prefix-numeric-value arg))
    (self-insert-command 1))
   ;;
   (t
    (insert ":")
    (unless (py-in-string-or-comment-p)
      (let ((orig (copy-marker (point)))
            (indent (py-compute-indentation)))
        (unless (or (eq (current-indentation) indent)
                    (and py-electric-colon-greedy-p
                         (eq indent
                             (save-excursion
                               (py-backward-statement)
                               (current-indentation))))
                    (and (looking-at py-def-or-class-re)
                         (< (current-indentation) indent)))
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to indent))
        (goto-char orig))
      (when py-electric-colon-newline-and-indent-p
        (py-newline-and-indent))))))

;; TODO: PRouleau: I would like to better understand this.
;;                 I don't understand the docstring.
;;                 What was the completion bug this is reacting to?
(defun py-electric-close (arg)
  "Close completion buffer when no longer needed.

It is it's sure, it's no longer needed, i.e. when inserting a space.

Works around a bug in ‘choose-completion’."

  (interactive "*P")
  (cond
   ((not py-electric-close-active-p)
    (self-insert-command (prefix-numeric-value arg)))
   ;;
   ((eq 4 (prefix-numeric-value arg))
    (self-insert-command 1))
   ;;
   (t (if (called-interactively-p 'any)
          (self-insert-command (prefix-numeric-value arg))
        ;; used from dont-indent-code-unnecessarily-lp-1048778-test
        (insert " ")))))

;; TODO: PRouleau: describe the electric behavior of '#'.
;;       This description should be in docstring of the
;;       ‘py-electric-comment-p’ user option and be referred to here.
;;       I currently don't understand what it should be and prefer not
;;       having to infer it from code.
;;       - From what I saw, the intent is to align the comment being
;;         typed to the one on line above or at the indentation level.
;;         - Is there more to it it than that?
;;         - I would like to see the following added (possibly via options):
;;           - When inserting the '#' follow it with a space, such that
;;             comment text is separated from the leading '#' by one space, as
;;             recommended in PEP-8
;;             URL https://www.python.org/dev/peps/pep-0008/#inline-comments
(defun py-electric-comment (arg)
  "Insert a comment.  If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With \\[universal-argument] \"#\" electric behavior is inhibited inside a
string or comment."
  (interactive "*P")
  (if (and py-indent-comments py-electric-comment-p)
      (if (ignore-errors (eq 4 (car-safe arg)))
          (insert "#")
        (when (and (eq last-command 'py-electric-comment)
                   (looking-back " " (line-beginning-position)))
          (forward-char -1))
        (if (called-interactively-p 'any)
            (self-insert-command (prefix-numeric-value arg))
          (insert "#"))
        (let ((orig (copy-marker (point)))
              (indent (py-compute-indentation)))
          (unless (eq (current-indentation) indent)
            (goto-char orig)
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to indent)
            (goto-char orig))
          (when py-electric-comment-add-space-p
            (unless (looking-at "[ \t]")
              (insert " "))))
        (setq last-command this-command))
    (self-insert-command (prefix-numeric-value arg))))

;; Electric deletion
(defun py-empty-out-list-backward ()
  "Deletes all elements from list before point."
  (interactive "*")
  (and (member (char-before) (list ?\) ?\] ?\}))
       (let ((orig (point))
             (thischar (char-before))
             pps cn)
         (forward-char -1)
         (setq pps (parse-partial-sexp (point-min) (point)))
         (if (and (not (nth 8 pps)) (nth 1 pps))
             (progn
               (goto-char (nth 1 pps))
               (forward-char 1))
           (cond ((or (eq thischar 41)(eq thischar ?\)))
                  (setq cn "("))
                 ((or (eq thischar 125) (eq thischar ?\}))
                  (setq cn "{"))
                 ((or (eq thischar 93)(eq thischar ?\]))
                  (setq cn "[")))
           (skip-chars-backward (concat "^" cn)))
         (delete-region (point) orig)
         (insert-char thischar 1)
         (forward-char -1))))

;; TODO: PRouleau Question: [...]

;;       - Also, the mapping for [backspace] in python-mode-map only works in
;;         graphics mode, it does not work when Emacs runs in terminal mode.
;;         It would be nice to have a binding that works in terminal mode too.
;; keep-one handed over form ‘py-electric-delete’ maybe
(defun py-electric-backspace (&optional arg)
  "Delete one or more of whitespace chars left from point.
Honor indentation.

If called at whitespace below max indentation,

Delete region when both variable ‘delete-active-region’ and ‘use-region-p’
are non-nil.

With \\[universal-argument], deactivate electric-behavior this time,
delete just one character before point.

At no-whitespace character, delete one before point.

"
  (interactive "*P")
  (unless (bobp)
    (let ((backward-delete-char-untabify-method 'untabify)
	  indent
	  done)
      (cond
       ;; electric-pair-mode
       ((and electric-pair-mode
             (or
              (and
               (ignore-errors (eq 5 (car (syntax-after (point)))))
               (ignore-errors (eq 4 (car (syntax-after (1- (point)))))))
              (and
               (ignore-errors (eq 7 (car (syntax-after (point)))))
               (ignore-errors (eq 7 (car (syntax-after (1- (point)))))))))
      (delete-char 1)
      (backward-delete-char-untabify 1))
       ((eq 4 (prefix-numeric-value arg))
	(backward-delete-char-untabify 1))
       ((use-region-p)
        ;; Emacs23 doesn't know that var
        (if (boundp 'delete-active-region)
	    (delete-active-region)
	  (delete-region (region-beginning) (region-end))))
       ((looking-back "[[:graph:]]" (line-beginning-position))
	(backward-delete-char-untabify 1))
       ;; before code
       ((looking-back "^[ \t]+" (line-beginning-position))
        (setq indent (py-compute-indentation))
	(cond ((< indent (current-indentation))
	       (back-to-indentation)
	       (delete-region (line-beginning-position) (point))
	       (indent-to indent))
	      ((<=  (current-column) py-indent-offset)
	       (delete-region (line-beginning-position) (point)))
	      ((eq 0 (% (current-column) py-indent-offset))
	       (delete-region (point) (progn (backward-char py-indent-offset) (point))))
	      (t (delete-region
		  (point)
		  (progn
		    ;; go backward the remainder
		    (backward-char (% (current-column) py-indent-offset))
		    (point))))))
       ((looking-back "[[:graph:]][ \t]+" (line-beginning-position))
	;; in the middle fixup-whitespace
	(setq done (line-end-position))
	(fixup-whitespace)
	;; if just one whitespace at point, delete that one
	(or (< (line-end-position) done) (delete-char 1)))

       ;; (if (< 1 (abs (skip-chars-backward " \t")))
       ;; 		 (delete-region (point) (progn (skip-chars-forward " \t") (point)))
       ;; 	       (delete-char 1))

       ((bolp)
	(backward-delete-char 1))
       (t
	(py-indent-line nil t))))))

(defun py-electric-delete (&optional arg)
  "Delete one or more of whitespace chars right from point.
Honor indentation.

Delete region when both variable ‘delete-active-region’ and ‘use-region-p’
are non-nil.

With \\[universal-argument], deactivate electric-behavior this time,
delete just one character at point.

At spaces in line of code, call fixup-whitespace.
At no-whitespace char, delete one char at point.
"
  (interactive "P*")
  (unless (eobp)
    (let* (;; py-ert-deletes-too-much-lp:1300270-dMegYd
	   ;; x = {'abc':'def',
           ;;     'ghi':'jkl'}
	   (backward-delete-char-untabify-method 'untabify)
	   (indent (py-compute-indentation))
	   ;; (delpos (+ (line-beginning-position) indent))
	   ;; (line-end-pos (line-end-position))
	   ;; (orig (point))
	   done)
      (cond
       ((eq 4 (prefix-numeric-value arg))
	(delete-char 1))
       ;; delete active region if one is active
       ((use-region-p)
	;; Emacs23 doesn't know that var
	(if (boundp 'delete-active-region)
            (delete-active-region)
	  (delete-region (region-beginning) (region-end))))
       ((looking-at "[[:graph:]]")
	(delete-char 1))
       ((or (eolp) (looking-at "[ \t]+$"))
	(cond
	 ((eolp) (delete-char 1))
	 ((< (+ indent (line-beginning-position)) (line-end-position))
	  (end-of-line)
	  (while (and (member (char-before) (list 9 32 ?\r))
		      (< indent (current-column)))
	    (backward-delete-char-untabify 1)))))
       (;; before code
	(looking-at "[ \t]+[[:graph:]]")
	;; before indent
	(if (looking-back "^[ \t]*" (line-beginning-position))
	    (cond ((< indent (current-indentation))
		   (back-to-indentation)
		   (delete-region (line-beginning-position) (point))
		   (indent-to indent))
		  ((< 0 (% (current-indentation) py-indent-offset))
		   (back-to-indentation)
		   (delete-region (point) (progn (backward-char (% (current-indentation) py-indent-offset)) (point))))
		  ((eq 0 (% (current-indentation) py-indent-offset))
		   (back-to-indentation)
		   (delete-region (point) (progn (backward-char py-indent-offset) (point))))
		  (t
		   (skip-chars-forward " \t")
		   (delete-region (line-beginning-position) (point))))
	  ;; in the middle fixup-whitespace
	  (setq done (line-end-position))
	  (fixup-whitespace)
	  ;; if just one whitespace at point, delete that one
	  (or (< (line-end-position) done) (delete-char 1))))
       (t (delete-char 1))))))

;; TODO: PRouleau: the electric yank mechanism is currently commented out.
;;       Is this a feature to keep?  Was it used?  I can see a benefit for it.
;;       Why is it currently disabled?
(defun py-electric-yank (&optional arg)
  "Perform command ‘yank’ followed by an ‘indent-according-to-mode’.
Pass ARG to the command ‘yank’."
  (interactive "P")
  (cond
   (py-electric-yank-active-p
    (yank arg)
    ;; (py-indent-line)
    )
   (t
    (yank arg))))

(defun py-toggle-py-electric-colon-active ()
  "Toggle use of electric colon for Python code."
  (interactive)
  (setq py-electric-colon-active-p (not py-electric-colon-active-p))
  (when (and py-verbose-p (called-interactively-p 'interactive)) (message "py-electric-colon-active-p: %s" py-electric-colon-active-p)))

;; TODO: PRouleau: It might be beneficial to have toggle commands for all
;;       the electric behaviours, not just the electric colon.

;; required for pending-del and delsel modes
(put 'py-electric-colon 'delete-selection t) ;delsel
(put 'py-electric-colon 'pending-delete t) ;pending-del
(put 'py-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'py-electric-backspace 'pending-delete 'supersede) ;pending-del
(put 'py-electric-delete 'delete-selection 'supersede) ;delsel
(put 'py-electric-delete 'pending-delete 'supersede) ;pending-del

;; python-components-virtualenv

(defvar virtualenv-workon-home nil)

(defvar virtualenv-name nil)

(defvar virtualenv-old-path nil)

(defvar virtualenv-old-exec-path nil)

(if (getenv "WORKON_HOME")
    (setq virtualenv-workon-home (getenv "WORKON_HOME"))
  (setq virtualenv-workon-home "~/.virtualenvs"))

;;TODO: Move to a generic UTILITY or TOOL package
(defun virtualenv-filter (predicate sequence)
  "Return a list of each SEQUENCE element for which the PREDICATE is non-nil.
The order of elements in SEQUENCE is retained."
  (let ((retlist '()))
    (dolist (element sequence (nreverse retlist))
      (when (funcall predicate element)
        (push element retlist)))))

(defun virtualenv-append-path (dir var)
  "Append DIR to a path-like variable VAR.

For example:
>>> (virtualenv-append-path \"/usr/bin:/bin\" \"/home/test/bin\")
\"/home/test/bin:/usr/bin:/bin\""
  (concat (expand-file-name dir)
          path-separator
          var))

(defun virtualenv-add-to-path (dir)
  "Add the specified DIR path element to the Emacs PATH."
  (setenv "PATH"
          (virtualenv-append-path dir
                                  (getenv "PATH"))))

(defun virtualenv-current ()
  "Display the current activated virtualenv."
  (interactive)
  (message virtualenv-name))

(defun virtualenv-activate (dir)
  "Activate the virtualenv located in specified DIR."
  (interactive "DVirtualenv Directory: ")
  ;; Eventually deactivate previous virtualenv
  (when virtualenv-name
    (virtualenv-deactivate))
  (let ((cmd (concat "source " dir "/bin/activate\n")))
    (comint-send-string (get-process (get-buffer-process "*shell*")) cmd)
    ;; Storing old variables
    (setq virtualenv-old-path (getenv "PATH"))
    (setq virtualenv-old-exec-path exec-path)

    (setenv "VIRTUAL_ENV" dir)
    (virtualenv-add-to-path (concat (py--normalize-directory dir) "bin"))
    (push (concat (py--normalize-directory dir) "bin")  exec-path)

    (setq virtualenv-name dir)))

(defun virtualenv-deactivate ()
  "Deactivate the current virtual environment."
  (interactive)
  ;; Restoring old variables
  (setenv "PATH" virtualenv-old-path)
  (setq exec-path virtualenv-old-exec-path)
  (message (concat "Virtualenv '" virtualenv-name "' deactivated."))
  (setq virtualenv-name nil))

(defun virtualenv-p (dir)
  "Check if a directory DIR is a virtualenv."
  (file-exists-p (concat dir "/bin/activate")))

(defun virtualenv-workon-complete ()
  "Return available completions for ‘virtualenv-workon’."
  (let
      ;;Varlist
      ((filelist (directory-files virtualenv-workon-home t)))
    ;; Get only the basename from the list of the virtual environments
    ;; paths
    (mapcar
     'file-name-nondirectory
     ;; Filter the directories and then the virtual environments
     (virtualenv-filter 'virtualenv-p
                        (virtualenv-filter 'file-directory-p filelist)))))

(defun virtualenv-workon (name)
  "Issue a virtualenvwrapper-like virtualenv-workon NAME command."
  (interactive (list (completing-read "Virtualenv: "
                                      (virtualenv-workon-complete))))
  (if (getenv "WORKON_HOME")
      (virtualenv-activate (concat (py--normalize-directory
                                    (getenv "WORKON_HOME")) name))
    (virtualenv-activate (concat
                          (py--normalize-directory virtualenv-workon-home)
                          name))))

;; python-abbrev-propose

(defun py-edit-abbrevs ()
  "Jumps to ‘python-mode-abbrev-table’."
  (interactive)
  (save-excursion
    (let ((mat (abbrev-table-name local-abbrev-table)))
      (prepare-abbrev-list-buffer)
      (set-buffer "*Abbrevs*")
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (search-forward (concat "(" (format "%s" mat))))))

(defun py--add-abbrev-propose (table type arg &optional dont-ask)
  (save-excursion
    (let ((orig (point))
          proposal exp name)
      (while (< 0 arg)
        (py-backward-partial-expression)
        (when (looking-at "[[:alpha:]]")
          (setq proposal (concat (downcase (match-string-no-properties 0)) proposal)))
        (setq arg (1- arg)))
      (setq exp (buffer-substring-no-properties (point) orig))
      (setq name
            ;; ask only when interactive
            (if dont-ask
                proposal
              (read-string (format (if exp "%s abbrev for \"%s\": "
                                     "Undefine %s abbrev: ")
                                   type exp) proposal)))
      (set-text-properties 0 (length name) nil name)
      (when (or (null exp)
                (not (abbrev-expansion name table))
                (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                                  name (abbrev-expansion name table))))
        (define-abbrev table (downcase name) exp)))))

(defun py-add-abbrev (arg)
  "Defines python-mode specific abbrev."
  (interactive "p")
  (save-excursion
    (py--add-abbrev-propose
     (if only-global-abbrevs
         global-abbrev-table
       (or local-abbrev-table
           (error "No per-mode abbrev table")))
     "Mode" arg)))

;; python-components-paragraph

(defun py-fill-string-django (&optional justify)
  "Fill docstring according to Django's coding standards style.

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
"
  (interactive "*P")
  (py-fill-string justify 'django t))

(defun py-fill-string-onetwo (&optional justify)
  "One newline and start and Two at end style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
"
  (interactive "*P")
  (py-fill-string justify 'onetwo t))

(defun py-fill-string-pep-257 (&optional justify)
  "PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
"
  (interactive "*P")
  (py-fill-string justify 'pep-257 t))

(defun py-fill-string-pep-257-nn (&optional justify)
  "PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
"
  (interactive "*P")
  (py-fill-string justify 'pep-257-nn t))

(defun py-fill-string-symmetric (&optional justify)
  "Symmetric style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
"
  (interactive "*P")
  (py-fill-string justify 'symmetric t))

(defun py-set-nil-docstring-style ()
  "Set py-docstring-style to \\='nil"
  (interactive)
  (setq py-docstring-style 'nil)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-nn-docstring-style ()
  "Set py-docstring-style to \\='pep-257-nn"
  (interactive)
  (setq py-docstring-style 'pep-257-nn)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-docstring-style ()
  "Set py-docstring-style to \\='pep-257"
  (interactive)
  (setq py-docstring-style 'pep-257)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-django-docstring-style ()
  "Set py-docstring-style to \\='django"
  (interactive)
  (setq py-docstring-style 'django)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-symmetric-docstring-style ()
  "Set py-docstring-style to \\='symmetric"
  (interactive)
  (setq py-docstring-style 'symmetric)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-onetwo-docstring-style ()
  "Set py-docstring-style to \\='onetwo"
  (interactive)
  (setq py-docstring-style 'onetwo)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-fill-comment (&optional justify)
  "Fill the comment paragraph at point"
  (interactive "*P")
  (let (;; Non-nil if the current line contains a comment.
        has-comment

        ;; If has-comment, the appropriate fill-prefix (format "%s" r the comment.
        comment-fill-prefix)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
        (setq has-comment t
              comment-fill-prefix (buffer-substring (match-beginning 0)
                                                    (match-end 0))))

       ;; A line with some code, followed by a comment? Remember that the hash
       ;; which starts the comment shouldn't be part of a string or character.
       ((progn
          (while (not (looking-at "#\\|$"))
            (skip-chars-forward "^#\n\"'\\")
            (cond
             ((eq (char-after (point)) ?\\) (forward-char 2))
             ((memq (char-after (point)) '(?\" ?')) (forward-sexp 1))))
          (looking-at "#+[\t ]*"))
        (setq has-comment t)
        (setq comment-fill-prefix
              (concat (make-string (current-column) ? )
                      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
        (fill-paragraph justify)

      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
        (narrow-to-region

         ;; Find the first line we should include in the region to fill.
         (save-excursion
           (while (and (zerop (forward-line -1))
                       (looking-at "^[ \t]*#")))

           ;; We may have gone to far.  Go forward again.
           (or (looking-at "^[ \t]*#")
               (forward-line 1))
           (point))

         ;; Find the beginning of the first line past the region to fill.
         (save-excursion
           (while (progn (forward-line 1)
                         (looking-at "^[ \t]*#")))
           (point)))

        ;; Lines with only hashes on them can be paragraph boundaries.
        (let ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
              (paragraph-separate (concat paragraph-separate "\\|[ \t#]*$"))
              (fill-prefix comment-fill-prefix))
          (fill-paragraph justify))))
    t))

(defun py--in-or-behind-or-before-a-docstring (pps)
  (interactive "*")
  (save-excursion
    (let* ((strg-start-pos (when (nth 3 pps) (nth 8 pps)))
	   (n8pps (or strg-start-pos
		      (when
			  (equal (string-to-syntax "|")
				 (syntax-after (point)))
			(and
			 (< 0 (skip-chars-forward "\"'"))
			 (nth 3 (parse-partial-sexp (point-min) (point))))))))
      (and n8pps (py--docstring-p n8pps)))))

(defun py--string-fence-delete-spaces (&optional start)
  "Delete spaces following or preceding delimiters of string at point. "
  (interactive "*")
  (let ((beg (or start (nth 8 (parse-partial-sexp (point-min) (point))))))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward "\"'rRuU")
      (delete-region (point) (progn (skip-chars-forward " \t\r\n\f")(point)))
      (goto-char beg)
      (forward-char 1)
      (skip-syntax-forward "^|")
      (skip-chars-backward "\"'rRuU")
      ;; (delete-region (point) (progn (skip-chars-backward " \t\r\n\f")(point)))
)))

(defun py--skip-raw-string-front-fence ()
  "Skip forward chars u, U, r, R followed by string-delimiters. "
  (when (member (char-after) (list ?u ?U ?r ?R))
    (forward-char 1))
  (skip-chars-forward "\'\""))

(defun py--fill-fix-end (thisend orig delimiters-style)
  ;; Add the number of newlines indicated by the selected style
  ;; at the end.
  ;; (widen)
  (goto-char thisend)
  (skip-chars-backward "\"'\n ")
  (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (point)))
  (unless (eq (char-after) 10)
    (and
     (cdr delimiters-style)
     (or (newline (cdr delimiters-style)) t)))
  (py-indent-line nil t)
  (goto-char orig))

(defun py--fill-docstring-first-line (beg end multi-line-p)
  "Refill first line after newline maybe. "
  (let (;;(beg (copy-marker
        (lae (length (buffer-substring-no-properties beg (line-end-position)))))
    (cond ((and
            ;; newline if multiline
            (member py-docstring-style (list 'django 'onetwo 'symmetric))
            (or multi-line-p (< lae (+ 3 (- fill-column (current-indentation))))))
           (goto-char beg)
           (newline 1)
           (indent-according-to-mode)))
    (fill-region-as-paragraph beg (line-end-position) nil t t)
    (goto-char beg)
    (forward-line 1)
    (back-to-indentation)
    (unless (or (< end (point)) (py-empty-line-p))
	  (split-line))))

(defun py-travel-single-words-and-symbols (beg end)
  (skip-chars-forward " \t\r\n\f" end)
  (while (and (looking-at " *\\w+ *$\\| *\\s.+ *$")(< end (line-end-position)))
    (forward-line 1)
    (back-to-indentation))
  (max beg (point)))

(defun py-fill-labelled-string (beg end)
  "Fill string or paragraph containing lines starting with label

See lp:1066489 "
  (interactive "r*")
  (let ((end (copy-marker end))
        (old-fill-prefix fill-prefix))
    (goto-char beg)
    (when (save-excursion (end-of-line) (re-search-forward py-labelled-re end t 1))
      (setq end (match-beginning 0)))
    (skip-chars-forward " \t\r\n\f")
    (py-travel-single-words-and-symbols beg end)
    (if (looking-at py-star-labelled-re)
        (setq fill-prefix (make-string (+ (current-indentation) 2) 32))
      (setq fill-prefix (make-string (+ (current-indentation) py-indent-offset) 32)))
    ;; (while (or (looking-at py-colon-labelled-re)
    ;;            (looking-at py-star-labelled-re))
    ;;   (forward-line 1))
    (fill-region-as-paragraph (line-beginning-position) end)
    (setq fill-prefix old-fill-prefix)))

(defun py--fill-docstring (docstring &optional beg end)
  "Fills paragraph in docstring below or at cursor position."
  (let* ((orig (point))
         (beg (or beg (progn (goto-char docstring) (line-beginning-position))))
         (end (copy-marker (or end (progn (goto-char beg)
                                          (skip-chars-forward " \t\r\n\f")
                                          (py--skip-raw-string-front-fence)
                                          (skip-syntax-forward "^|")
		                          (1+ (point)))))))
    (save-restriction
      ;; don't go backward beyond beginning of string
      (narrow-to-region beg end)
      (let* (;; If paragraph starts with beginning of string, skip the fence-chars
	     (innerbeg (copy-marker
                        (goto-char docstring)
                        (max
                         (py--skip-raw-string-front-fence)
		         (progn (unless (looking-at paragraph-start)
		                  (backward-paragraph))
                                (skip-chars-forward " \t\r\n\f")
		                ;; (when (looking-at paragraph-start)
		                (point)))))
	     (innerend (copy-marker (progn (goto-char end) (skip-chars-backward "\\'\"") (skip-chars-backward " \t\r\n\f") (point))))
	     (multi-line-p (string-match "\n" (buffer-substring-no-properties innerbeg innerend)))
             ;; (paragraph-separate (concat py-symbol-re "\\|" py-star-labelled-re "\\|" py-colon-labelled-re "\\|" paragraph-separate))
             ;; (paragraph-start (concat py-symbol-re "\\|" py-star-labelled-re "\\|" py-colon-labelled-re "\\|"  paragraph-start))
             parabeg paraend on-first-line)
        (setq paraend
              (save-excursion
                (goto-char orig)
                (py-travel-single-words-and-symbols innerbeg innerend)
                (end-of-line)
                (if (re-search-forward py-labelled-re end t)
                    (progn
                      (min (progn (beginning-of-line) (skip-chars-backward " \t\r\n\f") (point))
                           (save-excursion (goto-char orig) (forward-paragraph) (point)) innerend))
                  (progn (forward-paragraph) (skip-chars-backward " \t\r\n\f" orig) (min (point) innerend)))))
        (setq parabeg (max (progn (goto-char paraend) (backward-paragraph) (skip-chars-forward " \t\r\n\f") (point)) innerbeg))
        (setq on-first-line (< (line-beginning-position) docstring))
        (if (or (string-match (concat "^" py-colon-labelled-re) (buffer-substring-no-properties parabeg paraend))
                (string-match (concat "^" py-star-labelled-re) (buffer-substring-no-properties parabeg paraend)))
            (py-fill-labelled-string parabeg paraend)
          (when on-first-line (py--fill-docstring-first-line parabeg (line-end-position) multi-line-p))
          (setq parabeg (py-travel-single-words-and-symbols parabeg paraend))
          (goto-char parabeg)
          (setq fill-prefix (make-string (current-column) 32))
          (fill-region-as-paragraph parabeg paraend t))
        (when (member py-docstring-style (list 'pep-257 'onetwo))
          (goto-char paraend)
          (forward-line -1)
          (unless (py-empty-line-p)
            (forward-line 1)
            (split-line)))))))

(defun py-fill-string (&optional justify docstring pps)
  "String fill function for ‘py-fill-paragraph’.
JUSTIFY should be used (if applicable) as in ‘fill-paragraph’.

Fill according to ‘py-docstring-style’ "
  (interactive "*")
  (let* ((justify (or justify (if current-prefix-arg 'full t)))
	 ;; (style (or style py-docstring-style))
	 (pps (or pps (parse-partial-sexp (point-min) (point))))
	 (orig (copy-marker (point)))
	 ;; (docstring (or docstring (py--in-or-behind-or-before-a-docstring pps)))
	 (docstring (cond (docstring
			   (if (not (number-or-marker-p docstring))
			       (py--in-or-behind-or-before-a-docstring pps))
			   docstring)
			  (t (py--in-or-behind-or-before-a-docstring pps))))
	 (beg (and (nth 3 pps) (nth 8 pps)))
	 (tqs (progn (and beg (goto-char beg) (looking-at "\"\"\"\\|'''"))))
	 (end (copy-marker (if tqs
			       (or
				(progn (ignore-errors (forward-sexp))(and (< orig (point)) (point)))
				(goto-char orig)
				(line-end-position))
			     (or (progn (goto-char beg) (ignore-errors (forward-sexp))(and (< orig (point)) (point)))
				 (goto-char orig)
				 (line-end-position))))))
    (save-restriction
      ;; don't go backward beyond beginning of string
      (narrow-to-region beg (point-max))
      (goto-char orig)
      (when beg
        (if docstring
	    (py--fill-docstring docstring beg end)
	  (if (not tqs)
	      (if (py-preceding-line-backslashed-p)
		  (progn
		    (setq end (copy-marker (line-end-position)))
		    (narrow-to-region (line-beginning-position) end)
		    (fill-region (line-beginning-position) end justify t)
		    (when (< 1 (py-count-lines))
		      (py--continue-lines-region (point-min) end)))
		(narrow-to-region beg end)
		(fill-region beg end justify t)
		(when
		    ;; counting in narrowed buffer
		    (< 1 (py-count-lines))
		  (py--continue-lines-region beg end)))
	    (fill-region beg end justify)))))))

(defun py--continue-lines-region (beg end)
  (save-excursion
    (goto-char beg)
    (while (< (line-end-position) end)
      (end-of-line)
      (unless (py-escaped-p) (insert-and-inherit 32) (insert-and-inherit 92))
      (ignore-errors (forward-line 1)))))

(defun py-fill-paragraph (&optional pps beg end tqs)
  (interactive "*")
  (window-configuration-to-register py--windows-config-register)
  (let* ((pps (or pps (parse-partial-sexp (point-min) (point))))
	 (docstring (unless (not py-docstring-style) (py--in-or-behind-or-before-a-docstring pps)))
	 (fill-column py-comment-fill-column)
	 (in-string (nth 3 pps)))
    (cond ((or (nth 4 pps)
	       (and (bolp) (looking-at "[ \t]*#[# \t]*")))
	   (py-fill-comment))
	  (docstring
	   (setq fill-column py-docstring-fill-column)
	   (py--fill-docstring docstring
			       ;; current indentation
			       ;; (save-excursion (and (nth 3 pps) (goto-char (nth 8 pps)) (current-indentation)))
                               ))
	  (t
	   (let* ((beg (or beg (save-excursion
				 (if (looking-at paragraph-start)
				     (point)
				   (backward-paragraph)
				   (when (looking-at paragraph-start)
				     (point))))
			   (and (nth 3 pps) (nth 8 pps))))
		  (end (or end
			   (when beg
			     (save-excursion
			       (or
				(and in-string
				     (progn
				       (goto-char (nth 8 pps))
				       (setq tqs (looking-at "\"\"\"\\|'''"))
				       (forward-sexp) (point)))
				(progn
				  (forward-paragraph)
				  (when (looking-at paragraph-separate)
				    (point)))))))))
	     (and beg end (fill-region beg end))
	     (when (and in-string (not tqs))
	       (py--continue-lines-region beg end))))))
  (jump-to-register py--windows-config-register))

(defun py-fill-string-or-comment ()
  "Serve auto-fill-mode"
  (unless (< (current-column) fill-column)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (if (nth 3 pps)
	(py-fill-string nil nil pps)
      ;; (py-fill-comment pps)
      (do-auto-fill)
      ))))

;; python-components-section-forms

(defun py-execute-section ()
  "Execute section at point."
  (interactive)
  (py-execute-section-prepare))

(defun py-execute-section-python ()
  "Execute section at point using python interpreter."
  (interactive)
  (py-execute-section-prepare "python"))

(defun py-execute-section-python2 ()
  "Execute section at point using python2 interpreter."
  (interactive)
  (py-execute-section-prepare "python2"))

(defun py-execute-section-python3 ()
  "Execute section at point using python3 interpreter."
  (interactive)
  (py-execute-section-prepare "python3"))

(defun py-execute-section-ipython ()
  "Execute section at point using ipython interpreter."
  (interactive)
  (py-execute-section-prepare "ipython"))

(defun py-execute-section-ipython2.7 ()
  "Execute section at point using ipython2.7 interpreter."
  (interactive)
  (py-execute-section-prepare "ipython2.7"))

(defun py-execute-section-ipython3 ()
  "Execute section at point using ipython3 interpreter."
  (interactive)
  (py-execute-section-prepare "ipython3"))

(defun py-execute-section-jython ()
  "Execute section at point using jython interpreter."
  (interactive)
  (py-execute-section-prepare "jython"))

;; python-components-comment


(defun py-comment-region (beg end &optional arg)
  "Like `comment-region’ but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start)))
    (comment-region beg end arg)))

(defun py-comment-block (&optional beg end arg)
  "Comments block at point.

Uses double hash (`#') comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-block-position)))
          (end (or end (py--end-of-block-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-block-or-clause (&optional beg end arg)
  "Comments block-or-clause at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-block-or-clause-position)))
          (end (or end (py--end-of-block-or-clause-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-class (&optional beg end arg)
  "Comments class at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-class-position)))
          (end (or end (py--end-of-class-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-clause (&optional beg end arg)
  "Comments clause at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-clause-position)))
          (end (or end (py--end-of-clause-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-def (&optional beg end arg)
  "Comments def at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-def-position)))
          (end (or end (py--end-of-def-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-def-or-class (&optional beg end arg)
  "Comments def-or-class at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-def-or-class-position)))
          (end (or end (py--end-of-def-or-class-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-indent (&optional beg end arg)
  "Comments indent at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-indent-position)))
          (end (or end (py--end-of-indent-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-minor-block (&optional beg end arg)
  "Comments minor-block at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-minor-block-position)))
          (end (or end (py--end-of-minor-block-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-section (&optional beg end arg)
  "Comments section at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-section-position)))
          (end (or end (py--end-of-section-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-statement (&optional beg end arg)
  "Comments statement at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-statement-position)))
          (end (or end (py--end-of-statement-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-top-level (&optional beg end arg)
  "Comments top-level at point.

Uses double hash (`#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-top-level-position)))
          (end (or end (py--end-of-top-level-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))


;; python-components-comment ends here
;; python-components-fast-forms

;; Process forms fast

(defun py-execute-buffer-fast (&optional shell dedicated split switch proc)
  "Send accessible part of buffer to a Python interpreter.

Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SPLIT: split buffers after executing
Optional SWITCH: switch to output buffer after executing
Optional PROC: select an already running process for executing"
  (interactive)
  (py-execute-buffer shell dedicated t split switch proc))

(defun py-execute-region-fast (beg end &optional shell dedicated split switch proc)
  "Send region to a Python interpreter.

Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SPLIT: split buffers after executing
Optional SWITCH: switch to output buffer after executing
Optional PROC: select an already running process for executing"
  (interactive "r")
  (let ((py-fast-process-p t))
    (py-execute-region beg end shell dedicated t split switch proc)))

(defun py-execute-block-fast (&optional shell dedicated switch beg end file)
  "Process block at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'block shell dedicated switch beg end file t))

(defun py-execute-block-or-clause-fast (&optional shell dedicated switch beg end file)
  "Process block-or-clause at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'block-or-clause shell dedicated switch beg end file t))

(defun py-execute-class-fast (&optional shell dedicated switch beg end file)
  "Process class at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'class shell dedicated switch beg end file t))

(defun py-execute-clause-fast (&optional shell dedicated switch beg end file)
  "Process clause at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'clause shell dedicated switch beg end file t))

(defun py-execute-def-fast (&optional shell dedicated switch beg end file)
  "Process def at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'def shell dedicated switch beg end file t))

(defun py-execute-def-or-class-fast (&optional shell dedicated switch beg end file)
  "Process def-or-class at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'def-or-class shell dedicated switch beg end file t))

(defun py-execute-expression-fast (&optional shell dedicated switch beg end file)
  "Process expression at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'expression shell dedicated switch beg end file t))

(defun py-execute-partial-expression-fast (&optional shell dedicated switch beg end file)
  "Process partial-expression at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'partial-expression shell dedicated switch beg end file t))

(defun py-execute-section-fast (&optional shell dedicated switch beg end file)
  "Process section at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'section shell dedicated switch beg end file t))

(defun py-execute-statement-fast (&optional shell dedicated switch beg end file)
  "Process statement at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'statement shell dedicated switch beg end file t))

(defun py-execute-top-level-fast (&optional shell dedicated switch beg end file)
  "Process top-level at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default
Optional SHELL: Selecte a Python-shell(VERSION) as py-shell-name
Optional DEDICATED: run in a dedicated process
Optional SWITCH: switch to output buffer after executing
Optional File: execute through running a temp-file"
  (interactive)
  (py--execute-prepare 'top-level shell dedicated switch beg end file t))

;; python-components-narrow

(defun py-narrow-to-block ()
  "Narrow to block at point."
  (interactive)
  (py--narrow-prepare "block"))

(defun py-narrow-to-block-or-clause ()
  "Narrow to block-or-clause at point."
  (interactive)
  (py--narrow-prepare "block-or-clause"))

(defun py-narrow-to-class ()
  "Narrow to class at point."
  (interactive)
  (py--narrow-prepare "class"))

(defun py-narrow-to-clause ()
  "Narrow to clause at point."
  (interactive)
  (py--narrow-prepare "clause"))

(defun py-narrow-to-def ()
  "Narrow to def at point."
  (interactive)
  (py--narrow-prepare "def"))

(defun py-narrow-to-def-or-class ()
  "Narrow to def-or-class at point."
  (interactive)
  (py--narrow-prepare "def-or-class"))

(defun py-narrow-to-statement ()
  "Narrow to statement at point."
  (interactive)
  (py--narrow-prepare "statement"))

;; python-components-hide-show

;; (setq hs-block-start-regexp 'py-extended-block-or-clause-re)
;; (setq hs-forward-sexp-func 'py-forward-block)

(defun py-hide-base (form &optional beg end)
  "Hide visibility of existing form at point."
  (hs-minor-mode 1)
  (save-excursion
    (let* ((form (prin1-to-string form))
           (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
                            (funcall (intern-soft (concat "py-backward-" form))))))
           (end (or end (funcall (intern-soft (concat "py-forward-" form)))))
           (modified (buffer-modified-p))
           (inhibit-read-only t))
      (if (and beg end)
          (progn
            (hs-make-overlay beg end 'code)
            (set-buffer-modified-p modified))
        (error (concat "No " (format "%s" form) " at point"))))))

(defun py-hide-show (&optional form beg end)
  "Toggle visibility of existing forms at point."
  (interactive)
  (save-excursion
    (let* ((form (prin1-to-string form))
           (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
                            (funcall (intern-soft (concat "py-backward-" form))))))
           (end (or end (funcall (intern-soft (concat "py-forward-" form)))))
           (modified (buffer-modified-p))
           (inhibit-read-only t))
      (if (and beg end)
          (if (overlays-in beg end)
              (hs-discard-overlays beg end)
            (hs-make-overlay beg end 'code))
        (error (concat "No " (format "%s" form) " at point")))
      (set-buffer-modified-p modified))))

(defun py-show ()
  "Remove invisibility of existing form at point."
  (interactive)
  (with-silent-modifications
    (save-excursion
      (back-to-indentation)
      (let ((end (next-overlay-change (point))))
	(hs-discard-overlays (point) end)))))

(defun py-show-all ()
  "Remove invisibility of hidden forms in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (end)
      (while (and (not (eobp))  (setq end (next-overlay-change (point))))
	(hs-discard-overlays (point) end)
	(goto-char end)))))

(defun py-hide-region (beg end)
  "Hide active region."
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-hide-base 'region beg end))

(defun py-show-region (beg end)
  "Un-hide active region."
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (hs-discard-overlays beg end))

(defun py-hide-block ()
  "Hide block at point."
  (interactive)
  (py-hide-base 'block))

(defun py-hide-block-or-clause ()
  "Hide block-or-clause at point."
  (interactive)
  (py-hide-base 'block-or-clause))

(defun py-hide-class ()
  "Hide class at point."
  (interactive)
  (py-hide-base 'class))

(defun py-hide-clause ()
  "Hide clause at point."
  (interactive)
  (py-hide-base 'clause))

(defun py-hide-comment ()
  "Hide comment at point."
  (interactive)
  (py-hide-base 'comment))

(defun py-hide-def ()
  "Hide def at point."
  (interactive)
  (py-hide-base 'def))

(defun py-hide-def-or-class ()
  "Hide def-or-class at point."
  (interactive)
  (py-hide-base 'def-or-class))

(defun py-hide-elif-block ()
  "Hide elif-block at point."
  (interactive)
  (py-hide-base 'elif-block))

(defun py-hide-else-block ()
  "Hide else-block at point."
  (interactive)
  (py-hide-base 'else-block))

(defun py-hide-except-block ()
  "Hide except-block at point."
  (interactive)
  (py-hide-base 'except-block))

(defun py-hide-expression ()
  "Hide expression at point."
  (interactive)
  (py-hide-base 'expression))

(defun py-hide-for-block ()
  "Hide for-block at point."
  (interactive)
  (py-hide-base 'for-block))

(defun py-hide-if-block ()
  "Hide if-block at point."
  (interactive)
  (py-hide-base 'if-block))

(defun py-hide-indent ()
  "Hide indent at point."
  (interactive)
  (py-hide-base 'indent))

(defun py-hide-line ()
  "Hide line at point."
  (interactive)
  (py-hide-base 'line))

(defun py-hide-minor-block ()
  "Hide minor-block at point."
  (interactive)
  (py-hide-base 'minor-block))

(defun py-hide-paragraph ()
  "Hide paragraph at point."
  (interactive)
  (py-hide-base 'paragraph))

(defun py-hide-partial-expression ()
  "Hide partial-expression at point."
  (interactive)
  (py-hide-base 'partial-expression))

(defun py-hide-section ()
  "Hide section at point."
  (interactive)
  (py-hide-base 'section))

(defun py-hide-statement ()
  "Hide statement at point."
  (interactive)
  (py-hide-base 'statement))

(defun py-hide-top-level ()
  "Hide top-level at point."
  (interactive)
  (py-hide-base 'top-level))

(defun py-dynamically-hide-indent ()
  (interactive)
  (py-show)
  (py-hide-indent))

(defun py-dynamically-hide-further-indent (&optional arg)
  (interactive "P")
  (if (eq 4  (prefix-numeric-value arg))
      (py-show)
  (py-show)
  (py-forward-indent)
  (py-hide-indent)))

;; python-components-hide-show.el ends here
;; python-components-foot

(defun py-shell-fontify ()
  "Fontifies input in shell buffer. "
  ;; causes delay in fontification until next trigger
  ;; (unless (or (member (char-before) (list 32 ?: ?\)))
  ;; (unless (and (eq last-command 'self-insert-command) (eq (char-before) 32))
  ;; (< (abs (save-excursion (skip-chars-backward "^ \t\r\n\f"))) 2))
  (let* ((pps (parse-partial-sexp (line-beginning-position) (point)))
	 (start (if (and (nth 8 pps) (nth 1 pps))
		    (max (nth 1 pps) (nth 8 pps))
		  (or (nth 1 pps) (nth 8 pps)))))
    (when (or start
	      (setq start (ignore-errors (cdr comint-last-prompt))))
      (let* ((input (buffer-substring-no-properties
		     start (point-max)))
	     (buffer-undo-list t)
	     (replacement
	      (save-current-buffer
		(set-buffer py-shell--font-lock-buffer)
		(erase-buffer)
		(insert input)
		;; Ensure buffer is fontified, keeping it
		;; compatible with Emacs < 24.4.
		(if (fboundp 'font-lock-ensure)
		    (funcall 'font-lock-ensure)
		  (font-lock-default-fontify-buffer))
		(buffer-substring (point-min) (point-max))))
	     (replacement-length (length replacement))
	     (i 0))
	;; Inject text properties to get input fontified.
	(while (not (= i replacement-length))
	  (let* ((plist (text-properties-at i replacement))
		 (next-change (or (next-property-change i replacement)
				  replacement-length))
		 (plist (let ((face (plist-get plist 'face)))
			  (if (not face)
			      plist
			    ;; Replace FACE text properties with
			    ;; FONT-LOCK-FACE so input is fontified.
			    (plist-put plist 'face nil)
			    (plist-put plist 'font-lock-face face)))))
	    (set-text-properties
	     (+ start i) (+ start next-change) plist)
	    (setq i next-change)))))))

(defun py-message-which-python-mode ()
  (if (buffer-file-name)
      (if (string= "python-mode-el" (buffer-file-name))
	  (message "%s" "python-mode loaded from python-mode-el")
	(message "%s" "python-mode loaded from python-components-mode"))
    (message "python-mode loaded from: %s" python-mode-message-string)))

(defalias 'py-next-statement 'py-forward-statement)
;; #134, cython-mode compatibility
(defalias 'py-end-of-statement 'py-forward-statement)
(defalias 'py-beginning-of-statement 'py-backward-statement)
(defalias 'py-beginning-of-block 'py-backward-block)
(defalias 'py-end-of-block 'py-forward-block)
(defalias 'py-previous-statement 'py-backward-statement)
(defalias 'py-markup-region-as-section 'py-sectionize-region)

(define-derived-mode py-auto-completion-mode python-mode "Pac"
  "Run auto-completion"
  ;; disable company
  ;; (when company-mode (company-mode))
  (if py-auto-completion-mode-p
      (progn
	(setq py-auto-completion-mode-p nil
	      py-auto-completion-buffer nil)
	(when (timerp py--auto-complete-timer)(cancel-timer py--auto-complete-timer)))
    (setq py-auto-completion-mode-p t
	  py-auto-completion-buffer (current-buffer))
    (setq py--auto-complete-timer
	  (run-with-idle-timer
	   py--auto-complete-timer-delay
	   ;; 1
	   t
	   #'py-complete-auto)))
  (force-mode-line-update))

(autoload 'python-mode "python-mode" "Python Mode." t)

(defun all-mode-setting ()
  (set (make-local-variable 'indent-tabs-mode) py-indent-tabs-mode)
  )

(define-derived-mode python-mode prog-mode python-mode-modeline-display
  "Major mode for editing Python files.

To submit a report, enter `\\[py-submit-bug-report]'
from a‘python-mode’ buffer.
Do `\\[py-describe-mode]' for detailed documentation.
To see what version of ‘python-mode’ you are running,
enter `\\[py-version]'.

This mode knows about Python indentation,
tokens, comments (and continuation lines.
Paragraphs are separated by blank lines only.

COMMANDS

‘py-shell’\tStart an interactive Python interpreter in another window
‘py-execute-statement’\tSend statement at point to Python default interpreter
‘py-backward-statement’\tGo to the initial line of a simple statement

etc.

See available commands listed in files commands-python-mode at directory doc

VARIABLES

‘py-indent-offset’	indentation increment
‘py-shell-name’		shell command to invoke Python interpreter
‘py-split-window-on-execute’		When non-nil split windows
‘py-switch-buffers-on-execute-p’	When non-nil switch to the Python output buffer

\\{python-mode-map}"
  :group 'python-mode
  ;; load known shell listed in
  ;; Local vars
  (all-mode-setting)
  (set (make-local-variable 'electric-indent-inhibit) nil)
  (set (make-local-variable 'outline-regexp)
       (concat (mapconcat 'identity
                          (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                  py-outline-mode-keywords)
                          "\\|")))
  (when py-font-lock-defaults-p
    (if py-use-font-lock-doc-face-p
	(set (make-local-variable 'font-lock-defaults)
             '(python-font-lock-keywords nil nil nil nil
					 (font-lock-syntactic-keywords
					  . py-font-lock-syntactic-keywords)
					 (font-lock-syntactic-face-function
					  . py--font-lock-syntactic-face-function)))
      (set (make-local-variable 'font-lock-defaults)
           '(python-font-lock-keywords nil nil nil nil
				       (font-lock-syntactic-keywords
					. py-font-lock-syntactic-keywords)))))
  ;; avoid to run py-choose-shell again from ‘py--fix-start’
  (cond ((string-match "ython3" py-python-edit-version)
	 (font-lock-add-keywords 'python-mode
				 '(("\\<print\\>" . 'py-builtins-face)
				   ("\\<file\\>" . nil))))
	(t (font-lock-add-keywords 'python-mode
				   '(("\\<print\\>" . 'font-lock-keyword-face)
				     ("\\<file\\>" . 'py-builtins-face)))))
  (set (make-local-variable 'which-func-functions) 'py-which-def-or-class)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")

  (if py-empty-comment-line-separates-paragraph-p
      (progn
        (set (make-local-variable 'paragraph-separate) (concat "\f\\|^[\t]*$\\|^[ \t]*" comment-start "[ \t]*$\\|^[\t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
        (set (make-local-variable 'paragraph-start)
	     (concat "\f\\|^[ \t]*$\\|^[ \t]*" comment-start "[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
	(set (make-local-variable 'paragraph-separate)
	     (concat "\f\\|^[ \t]*$\\|^[ \t]*" comment-start "[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")))
    (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[\t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
    (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[\t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) #'py--comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  ;; introduced to silence compiler warning, no real setting
  ;; (set (make-local-variable 'hs-hide-comments-when-hiding-all) 'py-hide-comments-when-hiding-all)
  (set (make-local-variable 'outline-heading-end-regexp) ":[^\n]*\n")
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'add-log-current-defun-function) 'py-current-defun)
  (set (make-local-variable 'fill-paragraph-function) 'py-fill-paragraph)
  (set (make-local-variable 'normal-auto-fill-function) 'py-fill-string-or-comment)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'tab-width) py-indent-offset)
  (set (make-local-variable 'electric-indent-mode) nil)
  (and py-load-skeletons-p (py-load-skeletons))
  (and py-guess-py-install-directory-p (py-set-load-path))
  (and py-autopair-mode
       (declare-function autopair-python-triple-quote-action "autopair" ())
       (declare-function autopair-default-handle-action "autopair" ())
       (load-library "autopair")
       (add-hook 'python-mode-hook
                 #'(lambda ()
                     (setq autopair-handle-action-fns
                           (list #'autopair-default-handle-action
                                 #'autopair-python-triple-quote-action))))
       (py-autopair-mode-on))
  (when (and py--imenu-create-index-p
             (fboundp 'imenu-add-to-menubar)
             (ignore-errors (require 'imenu)))
    (setq imenu-create-index-function 'py--imenu-create-index-function)
    (setq imenu--index-alist (funcall py--imenu-create-index-function))
    ;; fallback
    (unless imenu--index-alist
      (setq imenu--index-alist (py--imenu-create-index-new)))
    ;; (message "imenu--index-alist: %s" imenu--index-alist)
    (imenu-add-to-menubar "PyIndex"))
  (when py-trailing-whitespace-smart-delete-p
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (py-shell-prompt-set-calculated-regexps)
  (setq comint-prompt-regexp py-shell--prompt-calculated-input-regexp)
  (cond
   (py-complete-function
    (add-hook 'completion-at-point-functions
              py-complete-function))
   (py-load-pymacs-p
    (add-hook 'completion-at-point-functions
              'py-complete-completion-at-point nil 'local))
   (t
    (add-hook 'completion-at-point-functions
              'py-shell-complete nil 'local)))
  ;; #'python-shell-completion-at-point nil 'local)))
  ;; (if py-auto-complete-p
  ;; (add-hook 'python-mode-hook 'py--run-completion-timer)
  ;; (remove-hook 'python-mode-hook 'py--run-completion-timer))
  ;; (when py-auto-complete-p
  ;; (add-hook 'python-mode-hook
  ;; (lambda ()
  ;; (run-with-idle-timer 1 t 'py-shell-complete))))
  (if py-auto-fill-mode
      (add-hook 'python-mode-hook 'py--run-auto-fill-timer)
    (remove-hook 'python-mode-hook 'py--run-auto-fill-timer))
  (add-hook 'python-mode-hook
            (lambda ()
              (setq imenu-create-index-function py--imenu-create-index-function)))
  ;; caused insert-file-contents error lp:1293172
  ;;  (add-hook 'after-change-functions 'py--after-change-function nil t)
  (if py-defun-use-top-level-p
      (progn
        (set (make-local-variable 'beginning-of-defun-function) 'py-backward-top-level)
        (set (make-local-variable 'end-of-defun-function) 'py-forward-top-level)
        (define-key python-mode-map [(control meta a)] 'py-backward-top-level)
        (define-key python-mode-map [(control meta e)] 'py-forward-top-level))
    (set (make-local-variable 'beginning-of-defun-function) 'py-backward-def-or-class)
    (set (make-local-variable 'end-of-defun-function) 'py-forward-def-or-class)
    (define-key python-mode-map [(control meta a)] 'py-backward-def-or-class)
    (define-key python-mode-map [(control meta e)] 'py-forward-def-or-class))
  (when py-sexp-use-expression-p
    (define-key python-mode-map [(control meta f)] 'py-forward-expression)
    (define-key python-mode-map [(control meta b)] 'py-backward-expression))

  (when py-hide-show-minor-mode-p (hs-minor-mode 1))
  (when py-outline-minor-mode-p (outline-minor-mode 1))
  (when (and py-debug-p (called-interactively-p 'any))
    (py-message-which-python-mode))
  (force-mode-line-update))

(define-derived-mode py-shell-mode comint-mode py-modeline-display
  "Major mode for Python shell process.

Variables
‘py-shell-prompt-regexp’,
‘py-shell-prompt-output-regexp’,
`py-shell-input-prompt-2-regexp',
‘py-shell-fontify-p’,
‘py-completion-setup-code’,
‘py-shell-completion-string-code’,
can customize this mode for different Python interpreters.

This mode resets ‘comint-output-filter-functions’ locally, so you
may want to re-add custom functions to it using the
‘py-shell-mode-hook’.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (setq mode-line-process '(":%s"))
  (all-mode-setting)
  ;; (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'py-shell--prompt-calculated-input-regexp) nil)
  (set (make-local-variable 'py-shell--block-prompt) nil)
  (set (make-local-variable 'py-shell--prompt-calculated-output-regexp) nil)
  (py-shell-prompt-set-calculated-regexps)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (set (make-local-variable 'comint-output-filter-functions)
       '(ansi-color-process-output
         py-comint-watch-for-first-prompt-output-filter
         py-pdbtrack-comint-output-filter-function
         py-comint-postoutput-scroll-to-bottom
         comint-watch-for-password-prompt))
  (set (make-local-variable 'compilation-error-regexp-alist)
       py-shell-compilation-regexp-alist)
  (compilation-shell-minor-mode 1)
  (add-hook 'completion-at-point-functions
	    #'py-shell-completion-at-point nil 'local)
  (cond
   ((string-match "^[Jj]" (process-name (get-buffer-process (current-buffer))))
    'indent-for-tab-command)
   (t
    (define-key py-shell-mode-map "\t"
		'py-indent-or-complete)))
  (make-local-variable 'py-pdbtrack-buffers-to-kill)
  (make-local-variable 'py-shell-fast-last-output)
  (set (make-local-variable 'py-shell--block-prompt) nil)
  (set (make-local-variable 'py-shell--prompt-calculated-output-regexp) nil)
  (py-shell-prompt-set-calculated-regexps)
  (if py-shell-fontify-p
      (progn
  	(py-shell-font-lock-turn-on))
    (py-shell-font-lock-turn-off)))

(make-obsolete 'jpython-mode 'jython-mode nil)

;; (push "*Python*"  same-window-buffer-names)
;; (push "*IPython*"  same-window-buffer-names)

;; Python Macro File
(unless (member '("\\.py\\'" . python-mode) auto-mode-alist)
  (push (cons "\\.py\\'"  'python-mode)  auto-mode-alist))

(unless (member '("\\.pym\\'" . python-mode) auto-mode-alist)
  (push (cons "\\.pym\\'"  'python-mode)  auto-mode-alist))

(unless (member '("\\.pyc\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyc\\'"  'python-mode)  auto-mode-alist))

;; Pyrex Source
(unless (member '("\\.pyx\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyx\\'"  'python-mode) auto-mode-alist))

;; Python Optimized Code
(unless (member '("\\.pyo\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyo\\'"  'python-mode) auto-mode-alist))

;; Pyrex Definition File
(unless (member '("\\.pxd\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pxd\\'"  'python-mode) auto-mode-alist))

;; Python Repository
(unless (member '("\\.pyr\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyr\\'"  'python-mode)  auto-mode-alist))

;; Python Stub file
;; https://www.python.org/dev/peps/pep-0484/#stub-files
(unless (member '("\\.pyi\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pyi\\'"  'python-mode)  auto-mode-alist))

;; Python Path Configuration
(unless (member '("\\.pth\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.pth\\'"  'python-mode)  auto-mode-alist))

;; Python Wheels
(unless (member '("\\.whl\\'" . python-mode)  auto-mode-alist)
  (push (cons "\\.whl\\'"  'python-mode)  auto-mode-alist))

(unless (member '("!#[          ]*/.*[jp]ython[0-9.]*" . python-mode) magic-mode-alist)
  (push '("!#[ \\t]*/.*[jp]ython[0-9.]*" . python-mode) magic-mode-alist))

;;  lp:1355458, what about using ‘magic-mode-alist’?

(defalias 'py-hungry-delete-forward 'c-hungry-delete-forward)
(defalias 'py-hungry-delete-backwards 'c-hungry-delete-backwards)

;; https://gitlab.com/python-mode-devs/python-mode/-/issues/105#note_1095808557
(puthash "python-"
         (append (gethash "python" definition-prefixes) '("python-mode"))
         definition-prefixes)
;;;
(provide 'python-mode)
;;; python-mode.el ends here
