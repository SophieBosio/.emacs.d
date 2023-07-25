;; We need org in order to make use of the tangling functionality
(require 'org)
;; Open the org-mode configuration
(find-file (concat user-emacs-directory "init.org"))
;; Tangle the file
(org-babel-tangle)
;; Load the tangled file
(load-file (concat user-emacs-directory "init.el"))
;; Byte-compile it
(byte-compile-file (concat user-emacs-directory "init.el"))
