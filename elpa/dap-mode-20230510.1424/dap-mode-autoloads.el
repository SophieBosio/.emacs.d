;;; dap-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dap-chrome" "dap-chrome.el" (0 0 0 0))
;;; Generated autoloads from dap-chrome.el

(register-definition-prefixes "dap-chrome" '("dap-chrome-"))

;;;***

;;;### (autoloads nil "dap-codelldb" "dap-codelldb.el" (0 0 0 0))
;;; Generated autoloads from dap-codelldb.el

(register-definition-prefixes "dap-codelldb" '("dap-codelldb-"))

;;;***

;;;### (autoloads nil "dap-cpptools" "dap-cpptools.el" (0 0 0 0))
;;; Generated autoloads from dap-cpptools.el

(register-definition-prefixes "dap-cpptools" '("dap-cpptools-"))

;;;***

;;;### (autoloads nil "dap-dlv-go" "dap-dlv-go.el" (0 0 0 0))
;;; Generated autoloads from dap-dlv-go.el

(register-definition-prefixes "dap-dlv-go" '("dap-dlv-go-"))

;;;***

;;;### (autoloads nil "dap-docker" "dap-docker.el" (0 0 0 0))
;;; Generated autoloads from dap-docker.el

(register-definition-prefixes "dap-docker" '("dap-docker-"))

;;;***

;;;### (autoloads nil "dap-edge" "dap-edge.el" (0 0 0 0))
;;; Generated autoloads from dap-edge.el

(register-definition-prefixes "dap-edge" '("dap-edge-"))

;;;***

;;;### (autoloads nil "dap-elixir" "dap-elixir.el" (0 0 0 0))
;;; Generated autoloads from dap-elixir.el

(register-definition-prefixes "dap-elixir" '("dap-elixir--populate-start-file-args"))

;;;***

;;;### (autoloads nil "dap-erlang" "dap-erlang.el" (0 0 0 0))
;;; Generated autoloads from dap-erlang.el

(register-definition-prefixes "dap-erlang" '("dap-erlang--populate-start-file-args"))

;;;***

;;;### (autoloads nil "dap-firefox" "dap-firefox.el" (0 0 0 0))
;;; Generated autoloads from dap-firefox.el

(register-definition-prefixes "dap-firefox" '("dap-firefox-"))

;;;***

;;;### (autoloads nil "dap-gdb-lldb" "dap-gdb-lldb.el" (0 0 0 0))
;;; Generated autoloads from dap-gdb-lldb.el

(register-definition-prefixes "dap-gdb-lldb" '("dap-gdb-lldb-"))

;;;***

;;;### (autoloads nil "dap-gdscript" "dap-gdscript.el" (0 0 0 0))
;;; Generated autoloads from dap-gdscript.el

(register-definition-prefixes "dap-gdscript" '("dap-gdscript-"))

;;;***

;;;### (autoloads nil "dap-go" "dap-go.el" (0 0 0 0))
;;; Generated autoloads from dap-go.el

(register-definition-prefixes "dap-go" '("dap-go-"))

;;;***

;;;### (autoloads nil "dap-hydra" "dap-hydra.el" (0 0 0 0))
;;; Generated autoloads from dap-hydra.el

(autoload 'dap-hydra "dap-hydra" "\
Run `dap-hydra/body'." t nil)

(register-definition-prefixes "dap-hydra" '("dap-hydra"))

;;;***

;;;### (autoloads nil "dap-kotlin" "dap-kotlin.el" (0 0 0 0))
;;; Generated autoloads from dap-kotlin.el

(register-definition-prefixes "dap-kotlin" '("dap-kotlin-"))

;;;***

;;;### (autoloads nil "dap-launch" "dap-launch.el" (0 0 0 0))
;;; Generated autoloads from dap-launch.el

(register-definition-prefixes "dap-launch" '("dap-"))

;;;***

;;;### (autoloads nil "dap-lldb" "dap-lldb.el" (0 0 0 0))
;;; Generated autoloads from dap-lldb.el

(register-definition-prefixes "dap-lldb" '("dap-lldb-"))

;;;***

;;;### (autoloads nil "dap-magik" "dap-magik.el" (0 0 0 0))
;;; Generated autoloads from dap-magik.el

(register-definition-prefixes "dap-magik" '("dap-magik-"))

;;;***

;;;### (autoloads nil "dap-mode" "dap-mode.el" (0 0 0 0))
;;; Generated autoloads from dap-mode.el

(autoload 'dap-debug "dap-mode" "\
Run debug configuration DEBUG-ARGS.

If DEBUG-ARGS is not specified the configuration is generated
after selecting configuration template.

:dap-compilation specifies a shell command to be run using
`compilation-start' before starting the debug session. It could
be used to compile the project, spin up docker, ....

\(fn DEBUG-ARGS)" t nil)

(defvar dap-mode nil "\
Non-nil if Dap mode is enabled.
See the `dap-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-mode'.")

(custom-autoload 'dap-mode "dap-mode" nil)

(autoload 'dap-mode "dap-mode" "\
Global minor mode for DAP mode.

This is a minor mode.  If called interactively, toggle the `Dap
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dap-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defvar dap-auto-configure-mode nil "\
Non-nil if Dap-Auto-Configure mode is enabled.
See the `dap-auto-configure-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-auto-configure-mode'.")

(custom-autoload 'dap-auto-configure-mode "dap-mode" nil)

(autoload 'dap-auto-configure-mode "dap-mode" "\
Auto configure dap minor mode.

This is a minor mode.  If called interactively, toggle the
`Dap-Auto-Configure mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dap-auto-configure-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "dap-mode" '("dap-"))

;;;***

;;;### (autoloads nil "dap-mouse" "dap-mouse.el" (0 0 0 0))
;;; Generated autoloads from dap-mouse.el

(defvar dap-tooltip-mode nil "\
Non-nil if Dap-Tooltip mode is enabled.
See the `dap-tooltip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-tooltip-mode'.")

(custom-autoload 'dap-tooltip-mode "dap-mouse" nil)

(autoload 'dap-tooltip-mode "dap-mouse" "\
Toggle the display of GUD tooltips.

This is a minor mode.  If called interactively, toggle the
`Dap-Tooltip mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dap-tooltip-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "dap-mouse" '("dap-"))

;;;***

;;;### (autoloads nil "dap-netcore" "dap-netcore.el" (0 0 0 0))
;;; Generated autoloads from dap-netcore.el

(register-definition-prefixes "dap-netcore" '("dap-netcore-"))

;;;***

;;;### (autoloads nil "dap-node" "dap-node.el" (0 0 0 0))
;;; Generated autoloads from dap-node.el

(register-definition-prefixes "dap-node" '("dap-node-"))

;;;***

;;;### (autoloads nil "dap-overlays" "dap-overlays.el" (0 0 0 0))
;;; Generated autoloads from dap-overlays.el

(register-definition-prefixes "dap-overlays" '("dap-overlays-"))

;;;***

;;;### (autoloads nil "dap-php" "dap-php.el" (0 0 0 0))
;;; Generated autoloads from dap-php.el

(register-definition-prefixes "dap-php" '("dap-php-"))

;;;***

;;;### (autoloads nil "dap-pwsh" "dap-pwsh.el" (0 0 0 0))
;;; Generated autoloads from dap-pwsh.el

(register-definition-prefixes "dap-pwsh" '("dap-pwsh-"))

;;;***

;;;### (autoloads nil "dap-python" "dap-python.el" (0 0 0 0))
;;; Generated autoloads from dap-python.el

(register-definition-prefixes "dap-python" '("dap-python-"))

;;;***

;;;### (autoloads nil "dap-ruby" "dap-ruby.el" (0 0 0 0))
;;; Generated autoloads from dap-ruby.el

(register-definition-prefixes "dap-ruby" '("dap-ruby-"))

;;;***

;;;### (autoloads nil "dap-swi-prolog" "dap-swi-prolog.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dap-swi-prolog.el

(register-definition-prefixes "dap-swi-prolog" '("dap-swi-prolog-"))

;;;***

;;;### (autoloads nil "dap-tasks" "dap-tasks.el" (0 0 0 0))
;;; Generated autoloads from dap-tasks.el

(register-definition-prefixes "dap-tasks" '("dap-tasks-"))

;;;***

;;;### (autoloads nil "dap-ui" "dap-ui.el" (0 0 0 0))
;;; Generated autoloads from dap-ui.el

(defvar dap-ui-mode nil "\
Non-nil if Dap-Ui mode is enabled.
See the `dap-ui-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-mode'.")

(custom-autoload 'dap-ui-mode "dap-ui" nil)

(autoload 'dap-ui-mode "dap-ui" "\
Displaying DAP visuals.

This is a minor mode.  If called interactively, toggle the
`Dap-Ui mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dap-ui-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'dap-ui-breakpoints-list "dap-ui" "\
List breakpoints." t nil)

(defvar dap-ui-controls-mode nil "\
Non-nil if Dap-Ui-Controls mode is enabled.
See the `dap-ui-controls-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-controls-mode'.")

(custom-autoload 'dap-ui-controls-mode "dap-ui" nil)

(autoload 'dap-ui-controls-mode "dap-ui" "\
Displaying DAP visuals.

This is a minor mode.  If called interactively, toggle the
`Dap-Ui-Controls mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dap-ui-controls-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'dap-ui-sessions "dap-ui" "\
Show currently active sessions." t nil)

(autoload 'dap-ui-locals "dap-ui" nil t nil)

(autoload 'dap-ui-show-many-windows "dap-ui" "\
Show auto configured feature windows." t nil)

(autoload 'dap-ui-hide-many-windows "dap-ui" "\
Hide all debug windows when sessions are dead." t nil)

(autoload 'dap-ui-repl "dap-ui" "\
Start an adapter-specific REPL.
This could be used to evaluate JavaScript in a browser, to
evaluate python in the context of the debugee, ...." t nil)

(register-definition-prefixes "dap-ui" '("dap-"))

;;;***

;;;### (autoloads nil "dap-unity" "dap-unity.el" (0 0 0 0))
;;; Generated autoloads from dap-unity.el

(register-definition-prefixes "dap-unity" '("dap-unity-"))

;;;***

;;;### (autoloads nil "dap-utils" "dap-utils.el" (0 0 0 0))
;;; Generated autoloads from dap-utils.el

(register-definition-prefixes "dap-utils" '("dap-utils-"))

;;;***

;;;### (autoloads nil "dap-variables" "dap-variables.el" (0 0 0 0))
;;; Generated autoloads from dap-variables.el

(register-definition-prefixes "dap-variables" '("dap-variables-"))

;;;***

;;;### (autoloads nil nil ("dap-mode-pkg.el" "dapui.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dap-mode-autoloads.el ends here
