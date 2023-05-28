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

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile Emacs startup
(let ((old-gc-treshold gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold old-gc-treshold))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq user-emacs-directory "~/.emacs.d/")
(setq default-directory "~/")

;; First, we need package!
(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5)
        ("ORG"          . 3)
        ("MELPA Stable" . 0)))

(package-initialize)

(let* ((package--builtins nil)
   (packages
	'(all-the-icons        ; Icon pack for pretty displays
	  auctex               ; Integrated environment for *TeX*
	  auto-compile         ; automatically compile Emacs Lisp libraries
	  chatgpt-shell        ; Interaction mode for ChatGPT
	  cider                ; Clojure Interactive Development Environment
	  clj-refactor         ; Commands for refactoring Clojure code
	  company              ; Modular text completion framework
	  counsel              ; Various completion functions using Ivy
	  counsel-projectile   ; Ivy integration for Projectile
	  dashboard            ; A startup screen extracted from Spacemacs
	  define-word          ; display the definition of word at point
	  diff-hl              ; Highlight uncommitted changes using VC
	  direnv               ; direnv integration
	  doom-themes          ; An opinionated pack of modern color-themes
	  doom-modeline        ; Mode line used in Doom Emacs
	  editorconfig         ; EditorConfig Emacs Plugin
          evil-nerd-commenter  ; Language-specific commenting
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
	  ligature             ; Font ligatures for Emacs
	  lua-mode             ; Major-mode for editing Lua scripts
	  magit                ; Control Git from Emacs
	  markdown-mode        ; Emacs Major mode for Markdown-formatted files
	  maude-mode           ; Emacs mode for the programming language Maude
	  multiple-cursors     ; Multiple cursors for Emacs
	  nano-modeline        ; N Λ N O modeline
	  nano-theme           ; N Λ N O theme
	  ob-chatgpt-shell     ; Org babel functions for ChatGPT evaluation
	  olivetti             ; Minor mode for a nice writing environment
	  org                  ; Outline-based notes management and organizer
	  org-bullets          ; Show bullets in org-mode as UTF-8 characters
	  org-msg              ; Org mode to send and reply to email in HTML
	  ox-gfm               ; Export Github Flavored Markdown from Org
	  paredit              ; minor mode for editing parentheses
	  pdf-tools            ; Emacs support library for PDF files
	  projectile           ; Manage and navigate projects in Emacs easily
	  proof-general        ; A generic Emacs interface for proof assistants
	  racket-mode          ; Major mode for Racket language
	  rainbow-delimiters   ; Coloured bracket pairs
	  slime                ; Superior Lisp Interaction Mode for Emacs
	  smex                 ; M-x interface with Ido-style fuzzy matching
	  svg-tag-mode         ; Display beautified SVG tags
	  try                  ; Try out Emacs packages
	  visual-fill-column   ; Center text
	  vterm                ; A terminal via libvterm
	  which-key)))         ; Display available keybindings in popup
  (let ((packages (seq-remove 'package-installed-p packages)))
    (when packages
  ;; Install uninstalled packages
  (package-refresh-contents)
  (mapc 'package-install packages))))

(add-to-list 'load-path "~/.emacs.d/local-extentions/")

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
      mouse-wheel-follow-mouse    't         ;; Scroll window under mouse
      scroll-step                  1         ;; Keyboard scroll one line
      mouse-wheel-scroll-amount   '(1 ((shift) . 1)) ;; Scroll one line at a time
)

(set-fringe-mode 10)  ;; Fringe of 10

(setq-default tab-width 4                       ;; Smaller tabs
              fill-column 79                    ;; Maximum line width
              indent-tabs-mode nil              ;; Use spaces instead of tabs
              split-width-threshold 160         ;; Split vertically by default
              split-height-threshold nil        ;; Split vertically by default
              frame-resize-pixelwise t          ;; Fine-grained frame resize
              auto-fill-function 'do-auto-fill  ;; Auto-fill-mode everywhere
)

;; Create a variable for our preferred tab width
(setq custom-tab-width 2)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to Enable Tabs
(add-hook 'prog-mode-hook 'enable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Language-Specific Tweaks
(setq-default python-indent-offset custom-tab-width) ;; Python
(setq-default js-indent-level custom-tab-width)      ;; Javascript

;; Making electric-indent behave sanely
;; (setq-default electric-indent-inhibit t)

;; Inhibit electric indent mode when changing to new major mode
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;;Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere

(defun custom/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
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

(fset 'yes-or-no-p 'y-or-n-p)

(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)

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

(set-language-environment "UTF-8")

(add-to-list 'default-frame-alist '(internal-border-width . 22))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist     '(fullscreen . maximized))
(add-hook 'window-setup-hook          'toggle-frame-fullscreen t)  ;; F11

(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

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

(defvar efs/default-font-size          102)   ;; Define default font size
(defvar efs/default-variable-font-size 102)   ;; Define default variable-pitch font size

;; Set font face
(set-face-attribute 'default nil :font "JetBrainsMono NFM" :height efs/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono NFM" :height efs/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Segoe UI" :height efs/default-variable-font-size)

;; Font ligatures
(defvar ligatures-JetBrainsMono
  '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++"
   "***" ";;" "!!" "??" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>" "<<" ">>" "||" "-|"
   "_|_" "|-" "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:"
   "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+ +>" "<*>" "<* *>" "</" "</>" "/>" "<!--"
   "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>"
   "=>>" ">=>" ">>=" ">>-" ">-" ">--" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<-"
   "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]"
   "|>" "<|" "||>" "<||" "|||>" "|||>" "<|>" "..." ".." ".=" ".-" "..<" ".?" "::" ":::"
   ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))
(use-package ligature
  :load-path "~/.emacs.d/elpa/ligature-20220808.1225/ligature.el"
  :config
  (ligature-set-ligatures 'prog-mode ligatures-JetBrainsMono)
  (global-ligature-mode t))

;; Prettify greek letters
(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))

;; Set theme
(load-theme 'doom-nord)

;; Cycle through themes
(setq cycle-themes-theme-list
      '(doom-nord doom-miramare doom-flatwhite doom-nord-light doom-nord-aurora doom-opera))

(require 'cycle-themes)
(cycle-themes-mode)

;; Change colour of fringes to match
(add-hook 'cycle-themes-after-cycle-hook
          #'(lambda ()
              (dolist (frame (frame-list))
                (set-face-attribute 'fringe frame 
                   :background (face-background 'default)))))

(require 'nerd-icons)
(nano-modeline-mode 1)
(setq nano-modeline-space-top      +0.60  ;; Space above the text
      nano-modeline-space-bottom   -0.60  ;; Space below the text
      nano-modeline-prefix         'icon  ;; I want icons, not RW/RO signifiers
      nano-modeline-prefix-padding t)     ;; Padding between prefix and text

(require 'olivetti)
(setq-default olivetti-body-width (+ fill-column 14))
(olivetti-mode 1)
(global-set-key (kbd "C-c C-o") 'olivetti-mode)

(pdf-loader-install)

(add-hook 'pdf-view-mode-hook
          (lambda () (setq header-line-format nil)))

(setq ivy-wrap t                         ;; Scrolling up brings me to last cand.
      ivy-height 25                      ;; Make Ivy taller
      ivy-use-virtual-buffers t          ;; C-x b displays recents and bookmarks
      ivy-on-del-error-function 'ignore  ;; Let me hold in backspace
      ivy-virtual-abbreviate 'abbreviate ;; Disambiguate same file diff dirs
)
(ivy-mode 1)

(require 'counsel)
(setq enable-recursive-minibuffers t
      search-default-mode #'char-fold-to-regexp)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Posframe, to display minibuffer as a child frame
(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '(
        (t . ivy-posframe-display)                 ;; Display the posframe
        (t . ivy-posframe-display-at-frame-center) ;; Display at frame center
    )
    ivy-posframe-width 85                          ;; Narrow box
    ivy-posframe-border-width 0                    ;; No surrounding border
)
(ivy-posframe-mode 1)

;; Fuzzy search
(setq ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
(setq ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))
(with-eval-after-load 'ivy
  (require 'ivy-fuz)
  (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

;; Sorting and filtering candidates
(with-eval-after-load 'counsel
    (require' ivy-prescient)
    (setq ivy-prescient-enable-filtering nil)
    (ivy-prescient-mode 1))

;; Display transformer for Ivy
;;(with-eval-after-load 'ivy
;;    (ivy-rich-mode 1))

(setq counsel-describe-function-function #'helpful-callable  ;; C-h f
      counsel-describe-variable-function #'helpful-variable) ;; C-h v
(global-set-key (kbd "C-h x") #'helpful-command)             ;; C-h x
(global-set-key (kbd "C-h k") #'helpful-key)                 ;; C-h k
(global-set-key (kbd "C-c C-d") #'helpful-at-point)          ;; C-c C-d
(global-set-key (kbd "C-h F") #'helpful-function)            ;; C-h F

(setq company-idle-delay 0
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))

(require 'svg-tag-mode)

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                    :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag value nil
                           :stroke 0 :margin 0)) :ascent 'center)))

(setq svg-tag-tags
      `(
        ;; Org tags
        (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
        
        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority 
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))
        
        ;; TODO / DONE
        ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


        ;; Citation of the form [cite:@Knuth:1984] 
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))

        
        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (with or without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

(global-svg-tag-mode t)

;; To do:         TODO DONE  
;; Tags:          :TAG1:TAG2:TAG3:
;; Priorities:    [#A] [#B] [#C]
;; Progress:      [1/3]
;;                [42%]
;; Active date:   <2021-12-24>
;;                <2021-12-24 Fri>
;;                <2021-12-24 14:00>
;;                <2021-12-24 Fri 14:00>
;; Inactive date: [2021-12-24]
;;                [2021-12-24 Fri]
;;                [2021-12-24 14:00]
;;                [2021-12-24 Fri 14:00]
;; Citation:      [cite:@Knuth:1984]

;; Enable text centering and line breaks for Org Mode
(defun my/org-mode-visual-style ()
  (visual-fill-column-mode t)
  (visual-fill-line-mode t))

(add-hook 'org-mode-hook 'my/org-mode-visual-style)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
