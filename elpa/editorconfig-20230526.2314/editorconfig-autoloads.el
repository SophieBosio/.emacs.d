;;; editorconfig-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "editorconfig" "editorconfig.el" (0 0 0 0))
;;; Generated autoloads from editorconfig.el

(autoload 'editorconfig-apply "editorconfig" "\
Get and apply EditorConfig properties to current buffer.

This function does not respect the values of `editorconfig-exclude-modes' and
`editorconfig-exclude-regexps' and always applies available properties.
Use `editorconfig-mode-apply' instead to make use of these variables." t nil)

(defvar editorconfig-mode nil "\
Non-nil if Editorconfig mode is enabled.
See the `editorconfig-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `editorconfig-mode'.")

(custom-autoload 'editorconfig-mode "editorconfig" nil)

(autoload 'editorconfig-mode "editorconfig" "\
Toggle EditorConfig feature.

This is a minor mode.  If called interactively, toggle the
`Editorconfig mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='editorconfig-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

To disable EditorConfig in some buffers, modify
`editorconfig-exclude-modes' or `editorconfig-exclude-regexps'.

\(fn &optional ARG)" t nil)

(autoload 'editorconfig-find-current-editorconfig "editorconfig" "\
Find the closest .editorconfig file for current file." t nil)

(autoload 'editorconfig-display-current-properties "editorconfig" "\
Display EditorConfig properties extracted for current buffer." t nil)

(defalias 'describe-editorconfig-properties 'editorconfig-display-current-properties)

(autoload 'editorconfig-format-buffer "editorconfig" "\
Format buffer according to .editorconfig indent_style and indent_width." t nil)

(autoload 'editorconfig-version "editorconfig" "\
Get EditorConfig version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

\(fn &optional SHOW-VERSION)" t nil)

(register-definition-prefixes "editorconfig" '("editorconfig-"))

;;;***

;;;### (autoloads nil "editorconfig-conf-mode" "editorconfig-conf-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from editorconfig-conf-mode.el

(autoload 'editorconfig-conf-mode "editorconfig-conf-mode" "\
Major mode for editing .editorconfig files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.editorconfig\\'" . editorconfig-conf-mode))

(register-definition-prefixes "editorconfig-conf-mode" '("editorconfig-conf-mode-"))

;;;***

;;;### (autoloads nil "editorconfig-core" "editorconfig-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from editorconfig-core.el

(autoload 'editorconfig-core-get-nearest-editorconfig "editorconfig-core" "\
Return path to .editorconfig file that is closest to DIRECTORY.

\(fn DIRECTORY)" nil nil)

(autoload 'editorconfig-core-get-properties "editorconfig-core" "\
Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This function returns an alist of properties.  Each element will
look like (KEY . VALUE).

\(fn &optional FILE CONFNAME CONFVERSION)" nil nil)

(autoload 'editorconfig-core-get-properties-hash "editorconfig-core" "\
Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This function is almost same as `editorconfig-core-get-properties', but returns
hash object instead.

\(fn &optional FILE CONFNAME CONFVERSION)" nil nil)

(register-definition-prefixes "editorconfig-core" '("editorconfig-core--"))

;;;***

;;;### (autoloads nil "editorconfig-core-handle" "editorconfig-core-handle.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from editorconfig-core-handle.el

(register-definition-prefixes "editorconfig-core-handle" '("editorconfig-core-handle"))

;;;***

;;;### (autoloads nil "editorconfig-fnmatch" "editorconfig-fnmatch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from editorconfig-fnmatch.el

(autoload 'editorconfig-fnmatch-p "editorconfig-fnmatch" "\
Test whether STRING match PATTERN.

Matching ignores case if `case-fold-search' is non-nil.

PATTERN should be a shell glob pattern, and some zsh-like wildcard matchings can
be used:

*           Matches any string of characters, except path separators (/)
**          Matches any string of characters
?           Matches any single character
\[name]      Matches any single character in name
\[^name]     Matches any single character not in name
{s1,s2,s3}  Matches any of the strings given (separated by commas)
{min..max}  Matches any number between min and max

\(fn STRING PATTERN)" nil nil)

(register-definition-prefixes "editorconfig-fnmatch" '("editorconfig-fnmatch-"))

;;;***

;;;### (autoloads nil nil ("editorconfig-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; editorconfig-autoloads.el ends here
