;;; auto-save-buffers-enhanced-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-save-buffers-enhanced" "auto-save-buffers-enhanced.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from auto-save-buffers-enhanced.el

(autoload 'auto-save-buffers-enhanced "auto-save-buffers-enhanced" "\
If `flag' is non-nil, activate `auto-save-buffers-enhanced' and start
auto-saving.

\(fn FLAG)" nil nil)

(autoload 'auto-save-buffers-enhanced-include-only-checkout-path "auto-save-buffers-enhanced" "\
If `flag' is non-nil, `auto-save-buffers-enhanced' saves only
the directories under VCS.

\(fn FLAG)" nil nil)

(autoload 'auto-save-buffers-enhanced-toggle-activity "auto-save-buffers-enhanced" "\
Toggle `auto-save-buffers-enhanced' activity" t nil)

(autoload 'auto-save-buffers-enhanced-reload-svk "auto-save-buffers-enhanced" "\
Reload the checkout paths from SVK configuration file." t nil)

(register-definition-prefixes "auto-save-buffers-enhanced" '("auto-save-buffers-enhanced-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-save-buffers-enhanced-autoloads.el ends here
