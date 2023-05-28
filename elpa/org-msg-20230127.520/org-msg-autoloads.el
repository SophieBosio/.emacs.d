;;; org-msg-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-msg" "org-msg.el" (0 0 0 0))
;;; Generated autoloads from org-msg.el

(defvar org-msg-mode nil "\
Non-nil if Org-Msg mode is enabled.
See the `org-msg-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-msg-mode'.")

(custom-autoload 'org-msg-mode "org-msg" nil)

(autoload 'org-msg-mode "org-msg" "\
Toggle OrgMsg mode.
With a prefix argument ARG, enable Delete Selection mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

When OrgMsg mode is enabled, the Message mode behavior is
modified to make use of Org Mode for mail composition and build
HTML emails.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-msg" '("org-msg-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-msg-autoloads.el ends here
