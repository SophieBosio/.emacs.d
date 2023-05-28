;;; vterm-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vterm" "vterm.el" (0 0 0 0))
;;; Generated autoloads from vterm.el

(autoload 'vterm-module-compile "vterm" "\
Compile vterm-module." t nil)

(autoload 'vterm--bookmark-handler "vterm" "\
Handler to restore a vterm bookmark BMK.

If a vterm buffer of the same name does not exist, the function will create a
new vterm buffer of the name. It also checks the current directory and sets
it to the bookmarked directory if needed.

\(fn BMK)" nil nil)

(autoload 'vterm-next-error-function "vterm" "\
Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in Compilation
buffers.  Prefix arg N says how many error messages to move
forwards (or backwards, if negative).

Optional argument RESET clears all the errors.

\(fn N &optional RESET)" t nil)

(autoload 'vterm "vterm" "\
Create an interactive Vterm buffer.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'.

\(fn &optional ARG)" t nil)

(autoload 'vterm-other-window "vterm" "\
Create an interactive Vterm buffer in another window.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "vterm" '("vterm-"))

;;;***

;;;### (autoloads nil nil ("vterm-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vterm-autoloads.el ends here
