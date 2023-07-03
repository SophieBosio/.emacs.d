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

;; First, we need package!
(require 'package)

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

(let* ((package--builtins nil)
   (packages
	'(all-the-icons        ; Icon pack for pretty displays
	  async                ; Async library
	  auctex               ; Integrated environment for *TeX*
	  auto-compile         ; Automatically compile Emacs Lisp libraries
	  auto-save-buffers-enhanced ; Auto-save buffers on change
	  chatgpt-shell        ; Interaction mode for ChatGPT
	  cider                ; Clojure Interactive Development Environment
	  clj-refactor         ; Commands for refactoring Clojure code
	  company              ; Modular text completion framework
	  counsel              ; Various completion functions using Ivy
	  consult              ; Completion, navigation and search with Vertico
	  counsel-projectile   ; Ivy integration for Projectile
	  dashboard            ; A startup screen extracted from Spacemacs
	  define-word          ; display the definition of word at point
	  diff-hl              ; Highlight uncommitted changes using VC
	  direnv               ; direnv integration
	  doom-themes          ; An opinionated pack of modern color-themes
	  editorconfig         ; EditorConfig Emacs Plugin
	  emojify              ; Display and insert emojis
	  evil-nerd-commenter  ; Language-specific commenting
	  focus                ; Dim surrounding font colour to focus on region
	  golden-ratio         ; Automatic resizing windows to golden ratio
	  haskell-mode         ; A Haskell editing mode
	  helpful              ; Easy-to-read docs that work with Counsel
	  ivy                  ; Incremental Vertical completYon
	  ivy-posframe         ; Using posframe to show Ivy
	  ivy-fuz              ; Fuzzy searching with Ivy and fuz
	  ivy-prescient        ; Sort and filter Ivy candidates
	  ivy-rich             ; Friendly display transformer for Ivy
	  jedi                 ; Python auto-completion for Emacs
	  js2-mode             ; Improved JavaScript editing mode
	  json                 ; JSON file format
	  ligature             ; Font ligatures for Emacs
	  lua-mode             ; Major-mode for editing Lua scripts
	  magit                ; Control Git from Emacs
	  markdown-mode        ; Emacs Major mode for Markdown-formatted files
      mixed-pitch          ; Intelligently decide what pitch font to use
	  multiple-cursors     ; Multiple cursors for Emacs
	  nano-modeline        ; N Λ N O modeline
	  nano-theme           ; N Λ N O theme
	  nerd-icons           ; Nerdy icons for every occasion
      obsidian             ; Obsidian note taking integration
	  ob-chatgpt-shell     ; Org babel functions for ChatGPT evaluation
	  olivetti             ; Minor mode for a nice writing environment
	  org                  ; Outline-based notes management and organizer
	  org-bullets          ; Show bullets in org-mode as UTF-8 characters
      org-modern           ; Prettify Org mode files
	  org-msg              ; Org mode to send and reply to email in HTML
	  org-superstar        ; Prettier Org mode bullets
	  ox-gfm               ; Export Github Flavored Markdown from Org
	  ox-hugo              ; Export to Blackfriday markdown, for Hugo sites
	  paredit              ; minor mode for editing parentheses
	  pdf-tools            ; Emacs support library for PDF files
	  projectile           ; Manage and navigate projects in Emacs easily
	  proof-general        ; A generic Emacs interface for proof assistants
	  racket-mode          ; Major mode for Racket language
	  rainbow-delimiters   ; Coloured bracket pairs
	  slime                ; Superior Lisp Interaction Mode for Emacs
	  smex                 ; M-x interface with Ido-style fuzzy matching
      spacemacs-theme      ; Light and dark theme from Spacemacs
	  svg-tag-mode         ; Display beautified SVG tags
	  treemacs             ; Interactive file tree
	  try                  ; Try out Emacs packages
	  vertico              ; VERTical Interactive COmpletion
	  vertico-posframe     ; Separate frame for Vertico minibuffer
	  visual-fill-column   ; Center text
	  vterm                ; A terminal via libvterm
	  which-key)))         ; Display available keybindings in popup
  (let ((packages (seq-remove 'package-installed-p packages)))
	(when packages
  ;; Install uninstalled packages
  (package-refresh-contents)
  (mapc 'package-install packages))))

(defvar local-extensions "~/.emacs.d/local-extensions/")
(add-to-list 'load-path  local-extensions)
(let ((default-directory local-extensions))
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq inhibit-startup-message      t         ;; No startup message
      initial-scratch-message      nil       ;; Empty scratch buffer
      ring-bell-function          'ignore    ;; No bell
      display-time-24hr-format     t         ;; Use 24h clock
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

(setq-default tab-width 4                       ;; Smaller tabs
              fill-column 79                    ;; Maximum line width
              split-width-threshold 160         ;; Split vertically by default
              split-height-threshold nil        ;; Split vertically by default
              frame-resize-pixelwise t          ;; Fine-grained frame resize
              auto-fill-function 'do-auto-fill  ;; Auto-fill-mode everywhere
)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'nerd-icons)

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

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist     '(fullscreen . maximized))
(add-hook 'window-setup-hook          'toggle-frame-fullscreen t)  ;; F11

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

(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom keybindings.")

(dolist (mode
    '(tool-bar-mode        ;; Remove toolbar
      scroll-bar-mode      ;; Remove scollbars
      menu-bar-mode        ;; Remove menu bar
      blink-cursor-mode))  ;; Solid cursor, not blinking
    (funcall mode 0))

(dolist (mode
    '(column-number-mode        ;; Show current column number in mode line
      delete-selection-mode     ;; Replace selected text when yanking
      dirtrack-mode             ;; Directory tracking in shell
      display-battery-mode      ;; Display battery percentage in mode line
      display-time-mode         ;; Display time in mode line
      editorconfig-mode         ;; Use the editorconfig plugin
      global-company-mode       ;; Auto-completion everywhere
      global-diff-hl-mode       ;; Highlight uncommitted changes
      global-so-long-mode       ;; Mitigate performance for long lines
      global-visual-line-mode   ;; Break lines instead of truncating them
      counsel-projectile-mode   ;; Manage and navigate projects
      recentf-mode              ;; Recently opened files
      show-paren-mode           ;; Highlight matching parentheses
      which-key-mode))          ;; Available key-bindings in popup
    (funcall mode 1))
(set-fringe-mode 10)            ;; Set fringe width to 10

(add-hook
   'prog-mode-hook 'display-line-numbers-mode) ;; Only line numbers when coding

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-heigth t)
  (set-face-attribute 'default nil        :font   "Roboto Mono-10.5")
  (set-face-attribute 'fixed-pitch nil    :family "Roboto Mono" :height 1.0)
  (set-face-attribute 'variable-pitch nil :family "Liberation Sans" :height 1.3))

(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta"  . ?Δ)
                                       ("gamma"  . ?Γ)
                                       ("phi"    . ?φ)
                                       ("psi"    . ?ψ)))

(require 'ligature)

(defvar ligatures '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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

(ligature-set-ligatures 'prog-mode ligatures)
(global-ligature-mode t)

(require 'emojify)
(add-hook 'after-init-hook #'global-emojify-mode)
(when (member "Apple Color Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; Set theme
(require' doom-themes)
(load-theme 'doom-nord)

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

(require 'olivetti)
(setq-default olivetti-body-width (+ fill-column 14))

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

(use-package consult)

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

(setq counsel-describe-function-function #'helpful-callable  ;; C-h f
      counsel-describe-variable-function #'helpful-variable) ;; C-h v
(global-set-key (kbd "C-h x")   #'helpful-command)           ;; C-h x
(global-set-key (kbd "C-h k")   #'helpful-key)               ;; C-h k
(global-set-key (kbd "C-c C-d") #'helpful-at-point)          ;; C-c C-d
(global-set-key (kbd "C-h F")   #'helpful-function)          ;; C-h F

(use-package projectile
  :bind (:map custom-bindings-map ("C-c p" . projectile-command-map)))

(require 'org)

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-src-fontify-natively t
      org-edit-src-content-indentation 0)

(setq org-log-done t)

;; Enable text centering and line breaks for Org Mode
(defun my/org-mode-visual-style ()
  (olivetti-mode 1))

(add-hook 'org-mode-hook 'my/org-mode-visual-style)

(use-package org-superstar
    :config
    (setq org-superstar-special-todo-items t)
    (add-hook 'org-mode-hook (lambda ()
                               (org-superstar-mode 1))))

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

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(define-minor-mode custom-bindings-mode
  "A mode that activates custom keybindings."
  :init-value t
  :keymap custom-bindings-map)

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

(define-key custom-bindings-map (kbd "C-s")     'consult-ripgrep)
(define-key custom-bindings-map (kbd "C-x b")   'consult-buffer)
(define-key custom-bindings-map (kbd "C-c C-g") 'consult-goto-line)

(define-key custom-bindings-map (kbd "C-c C-t") (cycle-themes))

(define-key custom-bindings-map (kbd "C-c C-o") 'olivetti-mode)

;; (define-key custom-bindings-map (kbd "C-c C-t") 'treemacs)

(define-key custom-bindings-map (kbd "C-c l") 'org-store-link)
(define-key custom-bindings-map (kbd "C-c a") 'org-agenda)
(define-key custom-bindings-map (kbd "C-c c") 'org-capture)
(define-key custom-bindings-map (kbd "C-c t") 'org-todo)

(define-key custom-bindings-map (kbd "C-c i") 'org-capture-inbox)

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)
