;;; racket-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "racket-back-end" "racket-back-end.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from racket-back-end.el

(register-definition-prefixes "racket-back-end" '("racket-"))

;;;***

;;;### (autoloads nil "racket-browse-url" "racket-browse-url.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-browse-url.el

(register-definition-prefixes "racket-browse-url" '("racket-browse-"))

;;;***

;;;### (autoloads nil "racket-bug-report" "racket-bug-report.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-bug-report.el

(autoload 'racket-bug-report "racket-bug-report" "\
Fill a buffer with data to make a Racket Mode bug report." t nil)

;;;***

;;;### (autoloads nil "racket-cmd" "racket-cmd.el" (0 0 0 0))
;;; Generated autoloads from racket-cmd.el

(defvar racket-start-back-end-hook nil "\
Hook run after `racket-start-back-end' finishes successfully.")

(defvar racket-stop-back-end-hook nil "\
Hook run before `racket-stop-back-end'.")

(autoload 'racket-start-back-end "racket-cmd" "\
Start a back end process used by Racket Mode.

If a process is already started, this command will stop and restart it.

When successful runs the hook `racket-start-back-end-hook'." t nil)

(autoload 'racket-stop-back-end "racket-cmd" "\
Stop a back end process used by Racket Mode.

Before doing anything runs the hook `racket-stop-back-end-hook'." t nil)

(register-definition-prefixes "racket-cmd" '("racket--"))

;;;***

;;;### (autoloads nil "racket-collection" "racket-collection.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-collection.el

(register-definition-prefixes "racket-collection" '("racket-"))

;;;***

;;;### (autoloads nil "racket-common" "racket-common.el" (0 0 0 0))
;;; Generated autoloads from racket-common.el

(register-definition-prefixes "racket-common" '("racket-"))

;;;***

;;;### (autoloads nil "racket-company-doc" "racket-company-doc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-company-doc.el

(register-definition-prefixes "racket-company-doc" '("racket--"))

;;;***

;;;### (autoloads nil "racket-complete" "racket-complete.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from racket-complete.el

(register-definition-prefixes "racket-complete" '("racket--"))

;;;***

;;;### (autoloads nil "racket-custom" "racket-custom.el" (0 0 0 0))
;;; Generated autoloads from racket-custom.el

(register-definition-prefixes "racket-custom" '("defface-racket" "racket-"))

;;;***

;;;### (autoloads nil "racket-debug" "racket-debug.el" (0 0 0 0))
;;; Generated autoloads from racket-debug.el

(autoload 'racket--debug-on-break "racket-debug" "\


\(fn RESPONSE)" nil nil)

(register-definition-prefixes "racket-debug" '("racket-"))

;;;***

;;;### (autoloads nil "racket-describe" "racket-describe.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from racket-describe.el

(register-definition-prefixes "racket-describe" '("racket-"))

;;;***

;;;### (autoloads nil "racket-doc" "racket-doc.el" (0 0 0 0))
;;; Generated autoloads from racket-doc.el

(register-definition-prefixes "racket-doc" '("racket--"))

;;;***

;;;### (autoloads nil "racket-edit" "racket-edit.el" (0 0 0 0))
;;; Generated autoloads from racket-edit.el

(add-to-list 'hs-special-modes-alist '(racket-mode "(" ")" ";" nil nil))

(register-definition-prefixes "racket-edit" '("racket-"))

;;;***

;;;### (autoloads nil "racket-eldoc" "racket-eldoc.el" (0 0 0 0))
;;; Generated autoloads from racket-eldoc.el

(register-definition-prefixes "racket-eldoc" '("racket--do-eldoc"))

;;;***

;;;### (autoloads nil "racket-font-lock" "racket-font-lock.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from racket-font-lock.el

(register-definition-prefixes "racket-font-lock" '("load-theme" "racket-"))

;;;***

;;;### (autoloads nil "racket-imenu" "racket-imenu.el" (0 0 0 0))
;;; Generated autoloads from racket-imenu.el

(register-definition-prefixes "racket-imenu" '("racket-"))

;;;***

;;;### (autoloads nil "racket-indent" "racket-indent.el" (0 0 0 0))
;;; Generated autoloads from racket-indent.el

(register-definition-prefixes "racket-indent" '("racket-"))

;;;***

;;;### (autoloads nil "racket-keywords-and-builtins" "racket-keywords-and-builtins.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-keywords-and-builtins.el

(register-definition-prefixes "racket-keywords-and-builtins" '("racket-"))

;;;***

;;;### (autoloads nil "racket-logger" "racket-logger.el" (0 0 0 0))
;;; Generated autoloads from racket-logger.el

(register-definition-prefixes "racket-logger" '("racket-"))

;;;***

;;;### (autoloads nil "racket-mode" "racket-mode.el" (0 0 0 0))
;;; Generated autoloads from racket-mode.el

(autoload 'racket-mode "racket-mode" "\
Major mode for editing Racket source files.

\\{racket-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

(add-to-list 'auto-mode-alist '("\\.rktd\\'" . racket-mode))

(add-to-list 'auto-mode-alist '("\\.rktl\\'" . racket-mode))

(modify-coding-system-alist 'file "\\.rkt[dl]?\\'" 'utf-8)

(add-to-list 'interpreter-mode-alist '("racket" . racket-mode))

(autoload 'racket-mode-start-faster "racket-mode" "\
Compile Racket Mode's .rkt files for faster startup.

Racket Mode is implemented as an Emacs Lisp \"front end\" that
talks to a Racket process \"back end\". Because Racket Mode is
delivered as an Emacs package instead of a Racket package,
installing it does not do the `raco setup` that is normally done
for Racket packages.

This command will do a `raco make` of Racket Mode's .rkt files,
creating bytecode files in `compiled/` subdirectories. As a
result, when a command must start the Racket process, it will
start somewhat faster.

On many computers, the resulting speed up is negligible, and
might not be worth the complication.

If you run this command, ever, you will need to run it again
after:

- Installing an updated version of Racket Mode. Otherwise, you
  might lose some of the speed-up.

- Installing a new version of Racket and/or changing the value of
  the variable `racket-program'. Otherwise, you might get an
  error message due to the bytecode being different versions.

To revert to compiling on startup, use
`racket-mode-start-slower'. " t nil)

(register-definition-prefixes "racket-mode" '("racket-"))

;;;***

;;;### (autoloads nil "racket-parens" "racket-parens.el" (0 0 0 0))
;;; Generated autoloads from racket-parens.el

(register-definition-prefixes "racket-parens" '("racket-"))

;;;***

;;;### (autoloads nil "racket-ppss" "racket-ppss.el" (0 0 0 0))
;;; Generated autoloads from racket-ppss.el

(register-definition-prefixes "racket-ppss" '("racket--ppss-"))

;;;***

;;;### (autoloads nil "racket-profile" "racket-profile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from racket-profile.el

(register-definition-prefixes "racket-profile" '("racket-"))

;;;***

;;;### (autoloads nil "racket-repl" "racket-repl.el" (0 0 0 0))
;;; Generated autoloads from racket-repl.el

(autoload 'racket-repl "racket-repl" "\
Show a Racket REPL buffer in some window.

*IMPORTANT*

The main, intended use of Racket Mode's REPL is that you
`find-file' some specific .rkt file, then run it using
`racket-run' or `racket-run-module-at-point'. The resulting REPL
will correspond to those definitions and match your expectations.

If you really want to start a REPL for no file in particular,
then you could use this `racket-repl' command. But the resulting
REPL will have a minimal \"#lang racket/base\" namespace. You
could enter \"(require racket)\" if you want the equivalent of
\"#lang racket\". You could also \"(require racket/enter)\" if
you want things like \"enter!\". But in some sense you'd be
\"using it wrong\". If you actually don't want to use Racket
Mode's REPL as intended, then consider using a plain Emacs
`shell' buffer to run command-line Racket.

\(fn &optional NOSELECT)" t nil)

(autoload 'racket-run "racket-repl" "\
Save the buffer in REPL and run your program.

As well as evaluating the outermost, file module, automatically
runs the submodules specified by the customization variable
`racket-submodules-to-run'.

See also `racket-run-module-at-point', which runs just the
specific module at point.

The command varies based on how many \\[universal-argument]
prefix arguments you supply.
\\<racket-mode-map>

- \\[racket-run-and-switch-to-repl]

  Follows the `racket-error-context' setting.

- \\[universal-argument] \\[racket-run-and-switch-to-repl]

  Uses errortrace for improved stack traces, as if
  `racket-error-context' were set to \"high\".

  This lets you keep `racket-error-context' set to a faster
  value like \"low\" or \"medium\", then conveniently re-run
  when you need a better strack trace.

- \\[universal-argument] \\[universal-argument] \\[racket-run-and-switch-to-repl]

  Instruments code for step debugging. See `racket-debug-mode'
  and the variable `racket-debuggable-files'.

Each run occurs within a Racket custodian. Any prior run's
custodian is shut down, releasing resources like threads and
ports. Each run's evaluation environment is reset to the contents
of the source file. In other words, like Dr Racket, this provides
the benefit that your source file is the \"single source of
truth\". At the same time, the run gives you a REPL inside the
namespace of the module, giving you the ability to explore it
interactively. Any explorations are temporary, unless you also
make them to your source file, they will be lost on the next run.

See also `racket-run-and-switch-to-repl', which is even more like
Dr Racket's Run command because it selects the REPL window after
running.

In the `racket-repl-mode' buffer, output that describes a file
and position is automatically \"linkified\". Examples of such
text include:

- Racket error messages.
- rackunit test failure location messages.
- print representation of path objects.

To visit these locations, move point there and press RET or mouse
click. Or, use the standard `next-error' and `previous-error'
commands.

\(fn &optional PREFIX)" t nil)

(autoload 'racket-run-module-at-point "racket-repl" "\
Save the buffer and run the module at point.

Like `racket-run' but runs the innermost module around point,
which is determined textually by looking for \"module\",
\"module*\", or \"module+\" forms nested to any depth, else
simply the outermost, file module.

\(fn &optional PREFIX)" t nil)

(register-definition-prefixes "racket-repl" '("racket-" "with-racket-repl-buffer"))

;;;***

;;;### (autoloads nil "racket-repl-buffer-name" "racket-repl-buffer-name.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-repl-buffer-name.el

(autoload 'racket-call-racket-repl-buffer-name-function "racket-repl-buffer-name" nil nil nil)

(autoload 'racket-repl-buffer-name-shared "racket-repl-buffer-name" "\
All `racket-mode' edit buffers share one `racket-repl-mode' buffer per back end.

A value for the variable `racket-repl-buffer-name-function'." t nil)

(autoload 'racket-repl-buffer-name-unique "racket-repl-buffer-name" "\
Each `racket-mode' edit buffer gets its own `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'." t nil)

(autoload 'racket-repl-buffer-name-project "racket-repl-buffer-name" "\
All `racket-mode' buffers in a project share a `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'.

The \"project\" is determined by `racket-project-root'." t nil)

(register-definition-prefixes "racket-repl-buffer-name" '("racket-"))

;;;***

;;;### (autoloads nil "racket-scribble" "racket-scribble.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from racket-scribble.el

(register-definition-prefixes "racket-scribble" '("racket--"))

;;;***

;;;### (autoloads nil "racket-shell" "racket-shell.el" (0 0 0 0))
;;; Generated autoloads from racket-shell.el

(register-definition-prefixes "racket-shell" '("racket-"))

;;;***

;;;### (autoloads nil "racket-show" "racket-show.el" (0 0 0 0))
;;; Generated autoloads from racket-show.el

(register-definition-prefixes "racket-show" '("racket-"))

;;;***

;;;### (autoloads nil "racket-smart-open" "racket-smart-open.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-smart-open.el

(autoload 'racket-smart-open-bracket-mode "racket-smart-open" "\
Minor mode to let you always type `[`' to insert `(` or `[` automatically.

This is a minor mode.  If called interactively, toggle the
`Racket-Smart-Open-Bracket mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `racket-smart-open-bracket-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Behaves like the \"Automatically adjust opening square brackets\"
feature in Dr. Racket.

By default, inserts a `(`. Inserts a `[` in the following cases:

  - `let`-like bindings -- forms with `let` in the name as well
    as things like `parameterize`, `with-handlers`, and
    `with-syntax`.

  - `case`, `cond`, `match`, `syntax-case`, `syntax-parse`, and
    `syntax-rules` clauses.

  - `for`-like bindings and `for/fold` accumulators.

  - `class` declaration syntax, such as `init` and `inherit`.

When the previous s-expression in a sequence is a compound
expression, uses the same kind of delimiter.

To force insert `[`, use `quoted-insert'.

Combined with `racket-insert-closing' this means that you can
press the unshifted `[` and `]` keys to get whatever delimiters
follow the Racket conventions for these forms. When something
like `electric-pair-mode' or `paredit-mode' is active, you need
not even press `]`.

Tip: When also using `paredit-mode', enable that first so that
the binding for the `[`' key in the map for
`racket-smart-open-bracket-mode' has higher priority. See also
the variable `minor-mode-map-alist'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "racket-smart-open" '("racket-"))

;;;***

;;;### (autoloads nil "racket-stepper" "racket-stepper.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from racket-stepper.el

(register-definition-prefixes "racket-stepper" '("racket-"))

;;;***

;;;### (autoloads nil "racket-unicode-input-method" "racket-unicode-input-method.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-unicode-input-method.el

(autoload 'racket-unicode-input-method-enable "racket-unicode-input-method" "\
Set input method to racket-unicode.

The racket-unicode input method lets you easily type various
Unicode symbols that might be useful when writing Racket code.

To automatically enable the racket-unicode input method in
racket-mode and racket-repl-mode buffers, put the following code
in your Emacs init file:

#+BEGIN_SRC elisp
    (add-hook \\='racket-mode-hook #\\='racket-unicode-input-method-enable)
    (add-hook \\='racket-repl-mode-hook #\\='racket-unicode-input-method-enable)
#+END_SRC

To temporarily enable this input method for a single buffer you
can use \"M-x racket-unicode-input-method-enable\".

Use the standard Emacs key C-\\ to toggle the input method.

When the racket-unicode input method is active, you can for
example type \"All\" and it is immediately replaced with \"∀\". A
few other examples:

| omega     | ω                        |
| x_1       | x₁                       |
| x^1       | x¹                       |
| A         | 𝔸                        |
| test-->>E | test-->>∃ (racket/redex) |
| vdash     | ⊢                        |

To see a table of all key sequences use \"M-x
describe-input-method <RET> racket-unicode\".

If you want to add your own mappings to the \"racket-unicode\"
input method, you may add code like the following example in your
Emacs init file:

#+BEGIN_SRC elisp
    ;; Either (require \\='racket-mode) here, or, if you use
    ;; use-package, put the code below in the :config section.
    (with-temp-buffer
      (racket-unicode-input-method-enable)
      (set-input-method \"racket-unicode\")
      (let ((quail-current-package (assoc \"racket-unicode\"
                                          quail-package-alist)))
        (quail-define-rules ((append . t))
                            (\"^o\" [\"ᵒ\"]))))
#+END_SRC

If you don’t like the highlighting of partially matching tokens you
can turn it off by setting `input-method-highlight-flag' to nil." t nil)

;;;***

;;;### (autoloads nil "racket-util" "racket-util.el" (0 0 0 0))
;;; Generated autoloads from racket-util.el

(register-definition-prefixes "racket-util" '("racket-"))

;;;***

;;;### (autoloads nil "racket-visit" "racket-visit.el" (0 0 0 0))
;;; Generated autoloads from racket-visit.el

(register-definition-prefixes "racket-visit" '("racket--"))

;;;***

;;;### (autoloads nil "racket-wsl" "racket-wsl.el" (0 0 0 0))
;;; Generated autoloads from racket-wsl.el

(register-definition-prefixes "racket-wsl" '("racket-"))

;;;***

;;;### (autoloads nil "racket-xp" "racket-xp.el" (0 0 0 0))
;;; Generated autoloads from racket-xp.el

(autoload 'racket-xp-mode "racket-xp" "\
A minor mode that analyzes expanded code to explain and explore.

This is a minor mode.  If called interactively, toggle the
`Racket-Xp mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `racket-xp-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This minor mode is an optional enhancement to `racket-mode' edit
buffers. Like any minor mode, you can turn it on or off for a
specific buffer. If you always want to use it, put the following
code in your Emacs init file:

#+BEGIN_SRC elisp
    (require \\='racket-xp)
    (add-hook \\='racket-mode-hook #\\='racket-xp-mode)
#+END_SRC

Note: This mode won't do anything unless/until the Racket Mode
back end is running. It will try to start the back end
automatically. You do /not/ need to `racket-run' the buffer you
are editing.

This mode uses the drracket/check-syntax package to analyze
fully-expanded programs, without needing to evaluate a.k.a.
\"run\" them. The resulting analysis provides information for:

- Visually annotating bindings -- local or imported definitions
  and references to them.

- Visually annotating expressions in a tail position, as well as
  the enclosing expression with respect to which they are in a
  tail position.

- Completion candidates.

- Defintions' source and documentation.

When point is on a definition or use, related items are
highlighted using `racket-xp-def-face' and `racket-xp-use-face'
-- instead of drawing arrows as in Dr Racket. Information is
displayed using the function(s) in the hook variable
`racket-show-functions'; it is also available when hovering the
mouse cursor.

Note: If you find these point-motion features too distracting
and/or slow, in your `racket-xp-mode-hook' you may disable them:

#+BEGIN_SRC elisp
  (require \\='racket-xp)
  (add-hook \\='racket-xp-mode-hook
            (lambda ()
              (remove-hook \\='pre-redisplay-functions
                           #\\='racket-xp-pre-redisplay
                           t)))
#+END_SRC

The remaining features discussed below will still work.

You may also use commands to navigate among a definition and its
uses, or to rename a local definitions and all its uses:

  - `racket-xp-next-definition'
  - `racket-xp-previous-definition'
  - `racket-xp-next-use'
  - `racket-xp-previous-use'

In the following little example, not only does
drracket/check-syntax distinguish the various \"x\" bindings, it
understands the two different imports of \"define\":

#+BEGIN_SRC racket
  #lang racket/base
  (define x 1)
  x
  (let ([x x])
    (+ x 1))
  (module m typed/racket/base
    (define x 2)
    x)
#+END_SRC

When point is on the opening parenthesis of an expression in tail
position, it is highlighted using the face
`racket-xp-tail-position-face'.

When point is on the opening parenthesis of an enclosing
expression with respect to which one or more expressions are in
tail position, it is highlighted using the face
`racket-xp-tail-target-face'.

Furthermore, when point is on the opening parenthesis of either
kind of expression, all of the immediately related expressions
are also highlighted. Various commands move among them:

  - `racket-xp-tail-up'
  - `racket-xp-tail-down'
  - `racket-xp-tail-next-sibling'
  - `racket-xp-tail-previous-sibling'

The function `racket-xp-complete-at-point' is added to the
variable `completion-at-point-functions'. Note that in this case,
it is not smart about submodules; identifiers are assumed to be
definitions from the file's module or its imports. In addition to
supplying completion candidates, it supports the
\":company-location\" property to inspect the definition of a
candidate and the \":company-doc-buffer\" property to view its
documentation.

When you edit the buffer, existing annotations are retained;
their positions are updated to reflect the edit. Annotations for
new or deleted text are not requested until after
`racket-xp-after-change-refresh-delay' seconds. The request is
made asynchronously so that Emacs will not block -- for
moderately complex source files, it can take some seconds simply
to fully expand them, as well as a little more time for the
drracket/check-syntax analysis. When the results are ready, all
annotations for the buffer are completely refreshed.

You may also set `racket-xp-after-change-refresh-delay' to nil
and use the `racket-xp-annotate' command manually.

The mode line changes to reflect the current status of
annotations, and whether or not you had a syntax error.

If you have one or more syntax errors, `racket-xp-next-error' and
`racket-xp-previous-error' navigate among them. Although most
languages will stop after the first syntax error, some like Typed
Racket will try to collect and report multiple errors.

You may use `xref-find-definitions' \\[xref-find-definitions],
`xref-pop-marker-stack' \\[xref-pop-marker-stack], and
`xref-find-references': `racket-xp-mode' adds a backend to the
variable `xref-backend-functions'. This backend uses information
from the drracket/check-syntax static analysis. Its ability to
find references is limited to the current file; when it finds
none it will try the default xref backend implementation which is
grep-based.

Tip: This mode follows the convention that a minor mode may only
use a prefix key consisting of \"C-c\" followed by a punctuation
key. As a result, `racket-xp-control-c-hash-keymap' is bound to
\"C-c #\" by default. Although you might find this awkward to
type, remember that as an Emacs user, you are free to bind this
map to a more convenient prefix, and/or bind any individual
commands directly to whatever keys you prefer.

\\{racket-xp-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "racket-xp" '("racket-"))

;;;***

;;;### (autoloads nil "racket-xp-complete" "racket-xp-complete.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-xp-complete.el

(register-definition-prefixes "racket-xp-complete" '("racket-"))

;;;***

;;;### (autoloads nil nil ("racket-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; racket-mode-autoloads.el ends here
