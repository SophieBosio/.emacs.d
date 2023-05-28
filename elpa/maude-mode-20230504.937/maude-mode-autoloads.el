;;; maude-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "maude-mode" "maude-mode.el" (0 0 0 0))
;;; Generated autoloads from maude-mode.el

(unless (assoc "\\.maude\\'" auto-mode-alist) (add-to-list 'auto-mode-alist '("\\.maude\\'" . maude-mode)))

(autoload 'maude-mode "maude-mode" "\
Major mode for editing Maude files.
Provides syntax highlighting.
\\[maude-indent-line] indents current line.
\\[run-maude] starts an interactive maude process.
\\[run-full-maude] starts an interactive full maude process.
\\[maude-send-paragraph] sends current paragraph to the (full)
maude process.
\\[maude-send-region] sends current region to the (full) maude
process.
\\[maude-send-buffer] sends the entire buffer to the process.
\\[maude-switch-to-inferior-maude] jumps between source buffer
and maude process buffer.

If you want certain keywords (try operator attributes) to be
automatically expanded, put
 (add-hook 'maude-mode-hook 
            '(lambda () 
               (abbrev-mode t)))
in your init file.

If you don't want the red warnings, put
  (add-hook 'maude-mode-hook
            '(lambda () 
                (setq maude-warnings nil)))
in your init file.

The following keys are set:
\\{maude-mode-map}

\(fn)" t nil)

(register-definition-prefixes "maude-mode" '("inferior-maude-" "maude-" "run-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; maude-mode-autoloads.el ends here
