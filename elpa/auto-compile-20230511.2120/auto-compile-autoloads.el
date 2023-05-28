;;; auto-compile-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-compile" "auto-compile.el" (0 0 0 0))
;;; Generated autoloads from auto-compile.el

(autoload 'auto-compile-mode "auto-compile" "\
Compile Emacs Lisp source files after the visiting buffers are saved.

This is a minor mode.  If called interactively, toggle the
`Auto-Compile mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `auto-compile-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

After a buffer containing Emacs Lisp code is saved to its source
file update the respective byte code file.  If the latter does
not exist do nothing.  Therefore to disable automatic compilation
remove the byte code file.  See command `toggle-auto-compile' for
a convenient way to do so.

This mode should be enabled globally, using it's globalized
variant `auto-compile-on-save-mode'.  Also see the related
`auto-compile-on-load-mode'.

\(fn &optional ARG)" t nil)

(put 'auto-compile-on-save-mode 'globalized-minor-mode t)

(defvar auto-compile-on-save-mode nil "\
Non-nil if Auto-Compile-On-Save mode is enabled.
See the `auto-compile-on-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `auto-compile-on-save-mode'.")

(custom-autoload 'auto-compile-on-save-mode "auto-compile" nil)

(autoload 'auto-compile-on-save-mode "auto-compile" "\
Toggle Auto-Compile mode in all buffers.
With prefix ARG, enable Auto-Compile-On-Save mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Auto-Compile mode is enabled in all buffers where
`turn-on-auto-compile-mode' would do it.

See `auto-compile-mode' for more information on Auto-Compile mode.

\(fn &optional ARG)" t nil)

(autoload 'toggle-auto-compile "auto-compile" "\
Toggle automatic compilation of an Emacs Lisp source file or files.

Read a file or directory name from the minibuffer defaulting to
the visited Emacs Lisp source file or `default-directory' if no
such file is being visited in the current buffer.

If the user selects a file then automatic compilation of only
that file is toggled.  Since both `auto-compile-on-save' and
`auto-compile-on-save' only ever _recompile_ byte code files,
toggling automatic compilation is done simply by creating or
removing the respective byte code file.

If the user selects a directory then automatic compilation for
multiple files is toggled as follows:

* With a positive prefix argument always compile source files;
  with a negative prefix argument always remove byte code files.

* Otherwise the existence or absence of the byte code file of
  the source file that was current when this command was invoked
  determines whether byte code files should be created or removed.

* If no Emacs Lisp source file is being visited in the buffer
  that was current when the command was invoked ask the user what
  to do.

* When _removing_ byte code files then all byte code files are
  removed.  If `auto-compile-deletes-stray-dest' is non-nil this
  even includes byte code files for which no source file exists.

* When _creating_ byte code files then only compile files for
  which `auto-compile-predicate-function' returns non-nil.  By
  default that includes all files that look like source files,
  based solely on their filenames.

* Note that non-libraries can still be automatically compiled,
  you just cannot _recursively_ turn on automatic compilation
  using this command.

* When `auto-compile-toggle-recompiles' is non-nil recompile all
  affected source files even when the respective source files are
  up-to-date.  Do so even for non-library source files.

* Compile libraries in subdirectories, except for files in hidden
  directories and directories containing a file named \".nosearch\".

\(fn FILE ACTION)" t nil)

(defvar auto-compile-on-load-mode nil "\
Non-nil if Auto-Compile-On-Load mode is enabled.
See the `auto-compile-on-load-mode' command
for a description of this minor mode.")

(custom-autoload 'auto-compile-on-load-mode "auto-compile" nil)

(autoload 'auto-compile-on-load-mode "auto-compile" "\
Before loading a library recompile it if it needs recompilation.

This is a minor mode.  If called interactively, toggle the
`Auto-Compile-On-Load mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='auto-compile-on-load-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

A library needs to be recompiled if the source file is newer than
it's byte-compile destination.  Without this advice the outdated
byte code file would be loaded instead.

Also see the related `auto-compile-on-save-mode'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "auto-compile" '("auto-compile-" "byte-compile-log-warning" "load" "mode-line-" "require" "save-buffers-kill-" "turn-on-auto-compile-mode"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-compile-autoloads.el ends here
