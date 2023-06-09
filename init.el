;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "If the current buffer is init.org the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(setq gc-cons-threshold (* 50 1000 1000))

;; Set and reset threshold
(let ((old-gc-treshold gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold old-gc-treshold))))

(setq user-emacs-directory "~/.emacs.d/")
(setq default-directory "~/")

(set-language-environment "UTF-8")

(require 'package)
(require 'use-package)
(setq use-package-always-ensure t)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("GNU ELPA"     . 20)
        ("MELPA"        . 15)
        ("ORG"          . 10)
        ("MELPA Stable" . 5)
        ("nongnu"       . 0)))

(package-initialize)

(defvar local-extensions "~/.emacs.d/local-extensions/")
(add-to-list 'load-path  local-extensions)
(let ((default-directory local-extensions))
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom keybindings.")

(setq inhibit-startup-message      t         ;; No startup message
      initial-scratch-message      nil       ;; Empty scratch buffer
      ring-bell-function          'ignore    ;; No bell
      display-time-default-load-average nil  ;; Don't show me load time
      default-directory            "~/"      ;; Set default directory
      scroll-margin                0         ;; Space between top/bottom
      use-dialog-box               nil       ;; Disable dialog
      auto-revert-interval         1         ;; Refresh buffers fast
      echo-keystrokes              0.1       ;; Show keystrokes fast
      frame-inhibit-implied-resize 1         ;; Don't resize frame implicitly
      sentence-end-double-space    nil       ;; No double spaces
      recentf-max-saved-items 1000           ;; Show more recent files
      save-interprogram-paste-before-kill t  ;; Save copies between programs
)

(set-fringe-mode 10)  ;; Fringe of 10

(dolist (mode
    '(tool-bar-mode        ;; Remove toolbar
      scroll-bar-mode      ;; Remove scollbars
      menu-bar-mode        ;; Remove menu bar
      blink-cursor-mode))  ;; Solid cursor, not blinking
    (funcall mode 0))

(setq-default tab-width 4                       ;; Smaller tabs
              fill-column 80                    ;; Maximum line width
              split-width-threshold 160         ;; Split vertically by default
              split-height-threshold nil        ;; Split vertically by default
              frame-resize-pixelwise t          ;; Fine-grained frame resize
              auto-fill-function 'do-auto-fill  ;; Auto-fill-mode everywhere
)

(fset 'yes-or-no-p 'y-or-n-p)

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed            nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse                  't) ;; scroll window under mouse
(setq scroll-step                                1) ;; keyboard scroll one line at a time
(setq use-dialog-box                           nil) ;; Disable dialog

(add-to-list 'default-frame-alist '(internal-border-width . 22))
(set-fringe-mode 10)            ;; Set fringe width to 10

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist     '(fullscreen . maximized))
(add-hook 'window-setup-hook          'toggle-frame-fullscreen t)  ;; F11

(use-package rainbow-delimiters
  :ensure t)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(dolist (mode
    '(column-number-mode        ;; Show current column number in mode line
      delete-selection-mode     ;; Replace selected text when yanking
      dirtrack-mode             ;; Directory tracking in shell
      editorconfig-mode         ;; Use the editorconfig plugin
      global-diff-hl-mode       ;; Highlight uncommitted changes
      global-so-long-mode       ;; Mitigate performance for long lines
      global-visual-line-mode   ;; Break lines instead of truncating them
	  global-auto-revert-mode   ;; Revert buffers automatically when they change
      recentf-mode              ;; Remember recently opened files
	  savehist-mode             ;; Remember minibuffer prompt history
	  save-place-mode           ;; Remember last cursor location in file
      show-paren-mode           ;; Highlight matching parentheses
	  which-key-mode))          ;; Available key-bindings in popup
    (funcall mode 1))

(setq history-length 25)        ;; Only save the last 25 minibuffer prompts
(setq global-auto-revert-non-file-buffers t) ;; Revert Dired and other buffers

(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; Only line numbers when coding

(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)

(setq auto-save-buffers-enhanced-exclude-regexps '("init.org"))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(setq indent-tabs-mode nil)
(infer-indentation-style)

(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

(setq backward-delete-char-untabify-method 'hungry)

(defun custom/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace,
   otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space behind us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (custom/delete-dont-kill 1)))

;; Delete a word without adding it to the kill ring
(defun custom/delete-dont-kill (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times.
Don't kill, just delete."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key [C-backspace] 'custom/backward-kill-word)

(use-package browse-kill-ring
  :ensure t)

(use-package expand-region
  :bind (:map custom-bindings-map ("C-+" . er/expand-region)))

(use-package evil-nerd-commenter
  :ensure t
  :bind (:map custom-bindings-map ("C-'" . evilnc-comment-or-uncomment-lines)))

(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

(when (member "Roboto Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Roboto Mono" :height 108)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono"))

(when (member "Source Sans 3" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.8))

(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta"  . ?Δ)
                                       ("gamma"  . ?Γ)
                                       ("phi"    . ?φ)
                                       ("psi"    . ?ψ)))

(require 'ligature)

(defvar ligature-def '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                       "\\\\" "://"))

(ligature-set-ligatures 'prog-mode ligature-def)
(global-ligature-mode t)

(require 'emojify)
(add-hook 'after-init-hook #'global-emojify-mode)
(when (member "Apple Color Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t))

(defvar favourite-themes '(doom-nord doom-nord-light doom-flatwhite))

(defun cycle-themes ()
  "Returns a function that lets you cycle your themes."
  (let ((themes favourite-themes))
    (lambda ()
      (interactive)
      ;; Rotates the thme cycle and changes the current theme.
      (let ((rotated (nconc (cdr themes) (list (car themes)))))
        (load-theme (car (setq themes rotated)) t))
      (message (concat "Switched to " (symbol-name (car themes)))))))

(defadvice load-theme
    (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

(require 'nano-modeline)
(nano-modeline-mode 1)
(setq nano-modeline-space-top      +0.60    ;; Space above the text
      nano-modeline-space-bottom   -0.60    ;; Space below the text
      nano-modeline-prefix         'status  ;; I want icons, not RW/RO signifiers
      nano-modeline-prefix-padding t)       ;; Padding between prefix and text

(use-package vterm
  :defer  t
  :preface
  (let ((last-vterm ""))
    (defun toggle-vterm ()
      (interactive)
      (cond ((string-match-p "^\\vterm<[1-9][0-9]*>$" (buffer-name))
             (goto-non-vterm-buffer))
            ((get-buffer last-vterm) (switch-to-buffer last-vterm))
            (t (vterm (setq last-vterm "vterm<1>")))))

    (defun goto-non-vterm-buffer ()
      (let* ((r "^\\vterm<[1-9][0-9]*>$")
             (vterm-buffer-p (lambda (b) (string-match-p r (buffer-name b))))
             (non-vterms (cl-remove-if vterm-buffer-p (buffer-list))))
        (when non-vterms
          (switch-to-buffer (car non-vterms)))))

	(defun switch-vterm (n)
      (let ((buffer-name (format "vterm<%d>" n)))
        (setq last-vterm buffer-name)
        (cond ((get-buffer buffer-name)
               (switch-to-buffer buffer-name))
              (t (vterm buffer-name)
                 (rename-buffer buffer-name))))))

  :bind (:map custom-bindings-map
              ("C-z" . toggle-vterm)
              ("M-1" . (lambda () (interactive) (switch-vterm 1)))
              ("M-2" . (lambda () (interactive) (switch-vterm 2)))
              ("M-3" . (lambda () (interactive) (switch-vterm 3)))
              ("M-4" . (lambda () (interactive) (switch-vterm 4)))
              ("M-5" . (lambda () (interactive) (switch-vterm 5)))
              ("M-6" . (lambda () (interactive) (switch-vterm 6)))
              ("M-7" . (lambda () (interactive) (switch-vterm 7)))
              ("M-8" . (lambda () (interactive) (switch-vterm 8)))
              ("M-9" . (lambda () (interactive) (switch-vterm 9))))

  :config
  ;; Don't query about killing vterm buffers, just kill it
  (defadvice vterm (after kill-with-no-query nil activate)
    (set-process-query-on-exit-flag (get-buffer-process ad-return-value) nil))
  ;; Center the terminal window by default with Olivetti
  (add-hook 'vterm-mode-hook 'olivetti-mode))

(require 'dashboard)
(setq dashboard-display-icons-p     t) ;; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner         "~/.emacs.d/images/lambda.png"
      dashboard-image-banner-max-width 100
      dashboard-banner-logo-title      "Welcome back!"
      dashboard-center-content         t
      dashboard-set-footer             nil
      dashboard-page-separator         "\n\n\n"
      dashboard-items '((projects     . 5)
                        (recents      . 10)
                        (agenda       . 5)))

(use-package olivetti
  :defer t
  :bind (:map custom-bindings-map ("C-c o" . olivetti-mode))
  :config
  (setq-default olivetti-body-width (+ fill-column 14)))

(pdf-loader-install)

(add-hook 'pdf-view-mode-hook
          (lambda () (setq header-line-format nil)))

(vertico-mode 1)
(setq vertico-count 25                       ; Show more candidates
    ; Hide unavailable commands
    read-extended-command-predicate 'command-completion-default-include-p
    read-file-name-completion-ignore-case t  ; Ignore case of file names
    read-buffer-completion-ignore-case t     ; Ignore case in buffer completion
    completion-ignore-case t                 ; Ignore case in completion
)

(use-package vertico-posframe
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-width 100
        vertico-posframe-height vertico-count))

(use-package consult
  :bind (:map custom-bindings-map
              ("C-x b"   . consult-buffer)
              ("C-s"     . consult-ripgrep)
			  ("C-c C-g" . consult-goto-line)))

(use-package marginalia
  :init 
  (marginalia-mode 1))

(use-package corfu
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 2
        corfu-popupinfo-delay 0.5))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic partial-completion)
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator "[ |]"))

(use-package helpful
  :bind (:map custom-bindings-map
			  ("C-h f" . #'helpful-callable)
			  ("C-h v" . #'helpful-variable)
			  ("C-h k" . #'helpful-key)
			  ("C-h x" . #'helpful-command)
			  ("C-h d" . #'helpful-at-point)
			  ("C-h F" . #'helpful-function)))

(use-package magit
  :bind (:map custom-bindings-map ("C-c m" . magit-status)))

(use-package projectile
  :bind (:map custom-bindings-map ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/Dropbox/projects/")))

(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "openai-key")))))

;; The file ~/.authinfo has this line:
;; machine api.openai.com password OPENAI_KEY
(setq chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))

(require 'org)

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-src-fontify-natively t
      org-edit-src-content-indentation 0)

(setq org-log-done t)

(add-hook 'org-mode-hook 'olivetti-mode)

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(plist-put org-format-latex-options :scale 2)

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(global-org-modern-mode)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.4)
                (org-level-2 . 1.35)
                (org-level-3 . 1.3)
                (org-level-4 . 1.2)
                (org-level-5 . 1.2)
                (org-level-6 . 1.2)
                (org-level-7 . 1.2)
                (org-level-8 . 1.2)))
  (set-face-attribute (car face) nil :font "Source Sans Pro" :weight 'bold :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font "Source Sans Pro" :weight
'bold :height 1.8)

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(use-package mixed-pitch
  :defer t
  :hook (text-mode . mixed-pitch-mode)
  :config
  ;;(setq mixed-pitch-set-heigth t)
  ;;(set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.3)
)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list "inbox.org"))

(setq org-capture-templates
       `(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))))
(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(define-key custom-bindings-map (kbd "C-c l") 'org-store-link)
(define-key custom-bindings-map (kbd "C-c a") 'org-agenda)
(define-key custom-bindings-map (kbd "C-c c") 'org-capture)
(define-key custom-bindings-map (kbd "C-c t") 'org-todo)

(define-key custom-bindings-map (kbd "C-c i") 'org-capture-inbox)

(with-eval-after-load 'ox
  (require 'ox-hugo))

(require 'obsidian)
(obsidian-specify-path "~/Dropbox/obsidian-personal")
;; If you want a different directory of `obsidian-capture':
(setq obsidian-inbox-directory "Inbox")

(add-hook
 'obsidian-mode-hook
 (lambda ()
   ;; Replace standard command with Obsidian.el's in obsidian vault:
   (local-set-key (kbd "C-c C-o") 'obsidian-follow-link-at-point)


   ;; Use either `obsidian-insert-wikilink' or `obsidian-insert-link':
   (local-set-key (kbd "C-c C-l") 'obsidian-insert-wikilink)

   ;; Following backlinks
   (local-set-key (kbd "C-c C-b") 'obsidian-backlink-jump)
   
   ;; Jump to another Obsidian note
   (local-set-key (kbd "C-c C-j") 'obsidian-jump)

   ;; Capture Obsidian note
   (local-set-key (kbd "C-c C-a") 'obsidian-capture)
))

(global-obsidian-mode t)

(use-package haskell-mode
  :defer t
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode)))

(use-package emacs
  :config
  (define-key custom-bindings-map (kbd "C-c C-t") (cycle-themes)))

(define-minor-mode custom-bindings-mode
  "A mode that activates custom keybindings."
  :init-value t
  :keymap custom-bindings-map)
