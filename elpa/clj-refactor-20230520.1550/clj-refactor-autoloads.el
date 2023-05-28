;;; clj-refactor-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "clj-refactor" "clj-refactor.el" (0 0 0 0))
;;; Generated autoloads from clj-refactor.el

(autoload 'cljr-add-keybindings-with-prefix "clj-refactor" "\
Bind keys in `cljr--all-helpers' under a PREFIX key.

\(fn PREFIX)" nil nil)

(autoload 'cljr-add-keybindings-with-modifier "clj-refactor" "\
Bind keys in `cljr--all-helpers' under a MODIFIER key.

\(fn MODIFIER)" nil nil)

(autoload 'cljr-rename-file-or-dir "clj-refactor" "\
Rename a file or directory of files.
Buffers visiting any affected file are killed and the
corresponding files are revisited.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-rename-file-or-dir

\(fn OLD-PATH NEW-PATH)" t nil)

(autoload 'cljr-rename-file "clj-refactor" "\


\(fn NEW-PATH)" t nil)

(autoload 'cljr-add-require-to-ns "clj-refactor" "\
Add a require statement to the ns form in current buffer.

With a prefix act on the cljs part of the ns declaration.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-require-to-ns

\(fn CLJS\\=\\?)" t nil)

(autoload 'cljr-add-use-to-ns "clj-refactor" "\
Add a use statement to the buffer's ns form.

With a prefix act on the cljs part of the ns declaration.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-use-to-ns

\(fn CLJS\\=\\?)" t nil)

(autoload 'cljr-add-import-to-ns "clj-refactor" "\
Add an import statement to the buffer's ns form.

With a prefix act on the cljs part of the ns declaration.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-import-to-ns

\(fn &optional CLJS\\=\\?)" t nil)

(autoload 'cljr-require-macro "clj-refactor" "\
Add a require statement for a macro to the ns form in current buffer.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-require-macro" t nil)

(autoload 'cljr-stop-referring "clj-refactor" "\
Stop referring to vars in the namespace at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-stop-referring" t nil)

(autoload 'cljr-move-form "clj-refactor" "\
Move the form containing POINT to a new namespace.

If REGION is active, move all forms contained by region.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-form" t nil)

(autoload 'cljr-add-declaration "clj-refactor" "\
Add a declare for the current def near the top of the buffer.

With a prefix add a declaration for the symbol under the cursor instead.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-declaration

\(fn FOR-THING-AT-POINT-P)" t nil)

(autoload 'cljr-extract-constant "clj-refactor" "\
Extract form at (or above) point as a constant.
Create a def for it at the top level, and replace its current
occurrence with the defined name.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-constant" t nil)

(autoload 'cljr-extract-def "clj-refactor" "\
Extract form at (or above) point as a def.
Create a def for it at the top level, and replace its current
occurrence with the defined name.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-def" t nil)

(autoload 'cljr-cycle-thread "clj-refactor" "\
Cycle a threading macro between -> and ->>.
Also applies to other versions of the macros, like cond->.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-thread" t nil)

(autoload 'cljr-introduce-let "clj-refactor" "\
Create a let form, binding the form at point.
The resulting let form can then be expanded with `\\[cljr-expand-let]'.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-introduce-let

\(fn &optional N)" t nil)

(autoload 'cljr-expand-let "clj-refactor" "\
Expand the let form above point by one level.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-expand-let" t nil)

(autoload 'cljr-move-to-let "clj-refactor" "\
Move the form at point to a binding in the nearest let.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-to-let" t nil)

(autoload 'cljr-destructure-keys "clj-refactor" "\
Change a symbol binding at point to a destructuring bind.
Keys to use in the destructuring are inferred from the code, and
their usage is replaced with the new local variables.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-destructure-keys" t nil)

(autoload 'cljr-raise-sexp "clj-refactor" "\
Like paredit-raise-sexp, but removes # in front of function literals and sets.

\(fn &optional ARGUMENT)" t nil)

(autoload 'cljr-splice-sexp-killing-backward "clj-refactor" "\
Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets.

\(fn &optional ARGUMENT)" t nil)

(autoload 'cljr-splice-sexp-killing-forward "clj-refactor" "\
Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets.

\(fn &optional ARGUMENT)" t nil)

(autoload 'cljr-slash "clj-refactor" "\
Inserts / as normal, but also checks for common namespace shorthands to require.
If `cljr-magic-requires' is non-nil, executing this command after one of the aliases
listed in `cljr-magic-require-namespaces', or any alias used elsewhere in the project,
will add the corresponding require statement to the ns form." t nil)

(autoload 'cljr-project-clean "clj-refactor" "\
Run `cljr-project-clean-functions' on every clojure file, then
sorts the project's dependency vectors.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-project-clean" t nil)

(autoload 'cljr-sort-project-dependencies "clj-refactor" "\
Sorts all dependency vectors in project.clj

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-sort-project-dependencies" t nil)

(autoload 'cljr-add-project-dependency "clj-refactor" "\
Add a dependency to the project.clj file.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-project-dependency

\(fn FORCE)" t nil)

(autoload 'cljr-update-project-dependency "clj-refactor" "\
Update the version of the dependency at point.

\(fn &optional VERSION)" t nil)

(autoload 'cljr-update-project-dependencies "clj-refactor" "\
Update all project dependencies.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-update-project-dependencies" t nil)

(autoload 'cljr-promote-function "clj-refactor" "\
Promote a function literal to an fn, or an fn to a defn.
With prefix PROMOTE-TO-DEFN, promote to a defn even if it is a
function literal.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-promote-function

\(fn PROMOTE-TO-DEFN)" t nil)

(autoload 'cljr-find-usages "clj-refactor" "\
Find all usages of the symbol at point in the project.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-find-usages" t nil)

(autoload 'cljr-rename-symbol "clj-refactor" "\
Rename the symbol at point and all of its occurrences.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-rename-symbol

\(fn &optional NEW-NAME)" t nil)

(autoload 'cljr-clean-ns "clj-refactor" "\
Clean the ns form for the current buffer.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-clean-ns" t nil)

(autoload 'cljr-add-missing-libspec "clj-refactor" "\
Requires or imports the symbol at point.

If the symbol at point is of the form str/join then the ns
containing join will be aliased to str.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-missing-libspec" t nil)

(autoload 'cljr-hotload-dependency "clj-refactor" "\
Download a dependency (if needed) and hotload it into the current repl session.

Defaults to the dependency vector at point, but prompts if none is found.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-hotload-dependency" t nil)

(autoload 'cljr-extract-function "clj-refactor" "\
Extract the form at (or above) point as a top-level defn.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-function" t nil)

(autoload 'cljr-add-stubs "clj-refactor" "\
Adds implementation stubs for the interface or protocol at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-stubs" t nil)

(autoload 'cljr-inline-symbol "clj-refactor" "\
Inline the symbol at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-inline-symbol" t nil)

(autoload 'cljr-version "clj-refactor" "\
Returns the version of the middleware as well as this package." t nil)

(autoload 'cljr-toggle-debug-mode "clj-refactor" nil t nil)

(autoload 'cljr-create-fn-from-example "clj-refactor" "\
Create a top-level defn for the symbol at point.
The context in which symbol is being used should be that of a
function, and the arglist of the defn is guessed from this
context.

For instance, if the symbol is the first argument of a `map'
call, the defn is created with one argument. If it is the first
argument of a `reduce', the defn will take two arguments.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-create-fn-from-example" t nil)

(autoload 'cljr-describe-refactoring "clj-refactor" "\
Show the wiki page, in emacs, for one of the available refactorings.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-describe-refactoring

\(fn CLJR-FN)" t nil)

(autoload 'cljr-change-function-signature "clj-refactor" "\
Change the function signature of the function at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-change-function-signature" t nil)

(autoload 'cljr--inject-middleware-p "clj-refactor" "\
Return non-nil if nREPL middleware should be injected.

\(fn &rest _)" nil nil)

(autoload 'cljr--inject-jack-in-dependencies "clj-refactor" "\
Inject the REPL dependencies of clj-refactor at `cider-jack-in'.
If injecting the dependencies is not preferred set `cljr-inject-dependencies-at-jack-in' to nil." nil nil)

(eval-after-load 'cider '(cljr--inject-jack-in-dependencies))

(autoload 'clj-refactor-mode "clj-refactor" "\
A mode to keep the clj-refactor keybindings.

This is a minor mode.  If called interactively, toggle the
`Clj-Refactor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `clj-refactor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{clj-refactor-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "clj-refactor" '("*cljr--noninteractive*" "cjr--occurrence-count" "clj" "hydra-cljr-"))

;;;***

;;;### (autoloads nil nil ("clj-refactor-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clj-refactor-autoloads.el ends here
