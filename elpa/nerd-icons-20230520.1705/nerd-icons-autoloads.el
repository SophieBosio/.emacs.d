;;; nerd-icons-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nerd-icons" "nerd-icons.el" (0 0 0 0))
;;; Generated autoloads from nerd-icons.el

(autoload 'nerd-icons-install-fonts "nerd-icons" "\
Helper function to download and install the latests fonts based on OS.
The provided Nerd Font is Symbols Nerd Font Mono.
When PFX is non-nil, ignore the prompt and just install

\(fn &optional PFX)" t nil)

(autoload 'nerd-icons-insert "nerd-icons" "\
Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When GLYPH-SET is non-nil, limit the candidates to the icon set matching it.

\(fn &optional ARG GLYPH-SET)" t nil)

(autoload 'nerd-icons-icon-for-dir "nerd-icons" "\
Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn DIR &rest ARG-OVERRIDES)" nil nil)

(autoload 'nerd-icons-icon-for-file "nerd-icons" "\
Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn FILE &rest ARG-OVERRIDES)" nil nil)

(autoload 'nerd-icons-icon-for-extension "nerd-icons" "\
Get the formatted icon for EXT.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn EXT &rest ARG-OVERRIDES)" nil nil)

(autoload 'nerd-icons-icon-for-mode "nerd-icons" "\
Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn MODE &rest ARG-OVERRIDES)" nil nil)

(autoload 'nerd-icons-icon-for-url "nerd-icons" "\
Get the formatted icon for URL.
If an icon for URL isn't found in `nerd-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn URL &rest ARG-OVERRIDES)" nil nil)

(autoload 'nerd-icons-icon-for-buffer "nerd-icons" "\
Get the formatted icon for the current buffer.

This function prioritises the use of the buffers file extension to
discern the icon when its `major-mode' matches its auto mode,
otherwise it will use the buffers `major-mode' to decide its
icon." nil nil)

(register-definition-prefixes "nerd-icons" '("nerd-icons-"))

;;;***

;;;### (autoloads nil nil ("nerd-icons-data.el" "nerd-icons-faces.el"
;;;;;;  "nerd-icons-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nerd-icons-autoloads.el ends here
