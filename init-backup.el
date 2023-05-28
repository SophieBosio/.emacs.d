;; -*- lexical-binding: t; -*-

;; Custom variables ------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "e7ba99d0f4c93b9c5ca0a3f795c155fa29361927cadb99cfce301caf96055dfd" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" default))
 '(global-writeroom-mode t nil (writeroom-mode))
 '(mini-frame-show-parameters '((top . 10) (width . 0.4) (left . 0.5)))
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("elpa" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(auto-save-buffers-enhanced esup eshell-up company treemacs-nerd-icons olivetti nerd-icons-ivy-rich org-bullets nano-modeline nano-theme svg-tag-mode ivy-posframe ivy-fuz mini-frame topspace writeroom-mode forge magit visual-fill-column org-present smooth-scrolling ligature twilight-theme lua-mode a haskell-mode all-the-icons-completion dap-mode lsp-ivy lsp-treemacs lsp-ui lsp-mode all-the-icons-ivy which-key doom-modeline all-the-icons doom-themes use-package auto-package-update))
 '(writeroom-header-line t)
 '(writeroom-mode-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#636363")))))

;; Startup &  Performance ------------------------------------------------------
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Set user directory
(setq user-emacs-directory "~/.emacs.d")
(setq default-directory "~/")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Automatic package updates
;; You can also use 'M-x auto-package-update-now'
(use-package auto-package-update
  :custom
  (auto-package-update-interval             7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results         t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Local extensions
;; (add-to-list 'load-path "~/.emacs.d/local-extensions/")
(let ((default-directory "~/.emacs.d/local-extensions/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Appearance & UI -------------------------------------------------------------
(defvar efs/default-font-size          102)
(defvar efs/default-variable-font-size 102)

;; Add small margin around the edges of the frame
(add-to-list 'default-frame-alist '(internal-border-width . 22))

;; Toggle transparency with C-c t
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 95) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Declutter
(setq inhibit-startup-message t
      initial-scratch-message nil)
(set-fringe-mode               10)

(dolist (mode
    '(tool-bar-mode        ;; Remove toolbar
      scroll-bar-mode      ;; Remove scollbars
      menu-bar-mode        ;; Remove menu bar
      blink-cursor-mode))  ;; Solid cursor, not blinking
    (funcall mode 0))

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed            nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse                  't) ;; scroll window under mouse
(setq scroll-step                                1) ;; keyboard scroll one line at a time
(setq use-dialog-box                           nil) ;; Disable dialog

;; Maximise by default
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist     '(fullscreen . maximized))
(add-hook 'window-setup-hook          'toggle-frame-fullscreen t)  ;; Open in fullscreen, aka F11


;; Theme
(require 'doom-themes)
(load-theme 'doom-nord)
;; Nice themes include:
;; - doom-miramare
;; - doom-flatwhite
;; - doom-nord
;; - doom-nord-light
;; - doom-nord-aurora
;; - nano-light / -dark

;; Font
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

;; Column and line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Time and battery, but not load average in modeline
(display-time-mode 1)
(setq display-time-24hr-format t)
(display-battery-mode 1)
(setq display-time-default-load-average nil)

;; Modeline
(require 'all-the-icons)
(require 'nerd-icons)
(nano-modeline-mode 1)
(setq nano-modeline-space-top      +0.60  ;; More modeline space above the ml text
      nano-modeline-space-bottom   -0.60  ;; More modeline space below the ml text
      nano-modeline-prefix         'icon  ;; I want icons, not RW/RO signifiers
      nano-modeline-prefix-padding t)     ;; Ensure padding between prefix and text
;; TODO Time, battery, column number in mode line

;; (use-package all-the-icons)
;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1))
;; (setq doom-modeline-bar-width 6           ;; bar width
;;       doom-modeline-height 30             ;; modeline height
;;       doom-modeline-buffer-state-icon nil ;; remove buffer state icon
;;       doom-modeline-buffer-modification-icon nil ;; remove buffer modification icon
;;       doom-modeline-buffer-encoding nil   ;; remove buffer encoding
;;       doom-modeline-time-icon nil         ;; remove time icon - only want actual time
;;       doom-modeline-time t                ;; time, but only if 'display-time-mode' is used!
;;       doom-modeline-battery t             ;; battery, but only if 'display-battery-mode' is used!
;;       doom-modeline-buffer-file-name-style 'file-name ;; display file name, not entire path
;;       )

;; Put modeline in the header instead of the default, in the footer
;; (add-to-list 'load-path  "~/.emacs.d/elpa/mode-line-in-header/")
;; (require 'mode-line-in-header)
;; (global-mode-line-in-header 1)

;; Display minibuffer as separate frame, in conjunction with Ivy
(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;(setq ivy-posframe-height ivy-height)      ;; Maintain the height given by ivy
(setq ivy-posframe-border-width 0)         ;; No border please
(setq ivy-posframe-width 85)
;; (setq ivy-posframe-min-width 80)
;; (setq ivy-posframe-max-width 100)
(ivy-posframe-mode 1)

;; Make symlinks show up as their actual filename
(setq find-file-visit-truename t)

;; Break lines
(global-visual-line-mode 1)

;; Center text by default with Olivetti
(require 'olivetti)
(setq-default olivetti-body-width (+ fill-column 14))
(olivetti-mode 1)
(global-set-key (kbd "C-c C-o") 'olivetti-mode)

;; Writeroom mode
(use-package writeroom-mode)


;; Interaction -----------------------------------------------------------------

;; Autosave file
(setq global-auto-save-visited-mode 1)
;; TODO
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)

;; Which key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(setq ivy-height 25)

;; Ivy Rich
;; (use-package ivy-rich
;;   :after ivy
;;   :init
;;   (ivy-rich-mode 1))
;; (setq ivy-rich-path-style 'abbrev)

;; Counsel
(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; Prescient
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;; Fuzzy searching for Ivy
(use-package ivy-fuz
  :ensure t
  :demand t
  :after ivy
  :custom
  (ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
  (ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))
  :config
  (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

;; Helpful
(use-package helpful
 :commands (helpful-callable helpful-variable helpful-command helpful-key)
 :custom
 (counsel-describe-function-function #'helpful-callable)
 (counsel-describe-variable-function #'helpful-variable)
 :bind
 ([remap describe-function] . counsel-describe-function)
 ([remap describe-command] . helpful-command)
 ([remap describe-variable] . counsel-describe-variable)
 ([remap describe-key] . helpful-key))

;; Auto-completion with company-mode
(require 'company)
(setq company-idle-delay 0
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))
(add-hook 'after-init-hook 'global-company-mode)

;; File tree view with treemacs
(global-set-key (kbd "C-c C-t") 'treemacs)


;; Git -------------------------------------------------------------------------
(setq auth-sources '("~/.authinfo"))

(use-package magit
 :commands magit-status
 :custom
 (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
 :after magit)

;; Development & Programming Setups --------------------------------------------
;; lsp mode and accompanying packages
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Gives you tree-like file browser
(use-package lsp-treemacs
  :after lsp)

;; lsp-ivy lets you search for things by name in your code
(use-package lsp-ivy
  :after lsp)


;; ;; Debugging with the debugger adapter protocol!
(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  :custom
  (lsp-enable-dap-auto-configure nil)
  :commands dap-debug
  :config (dap-ui-mode 1)

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;; Python using lsp and dap, with the pyls language server
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))
(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))


;; Programming Keybindings & Behaviours ----------------------------------------

;; Comment/uncomment with C-'
(use-package evil-nerd-commenter
  :bind ("C-'" . evilnc-comment-or-uncomment-lines))

;; Coloured bracket pairs
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Auto-pair brackets
(electric-pair-mode t)

;; Change ctrl + backspace behaviour to calm it down
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


;; Sane tab/indentation behaviour ----------------------------------------------
;; TODO Backtab to un-indent

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

;; Inhibit electric indent mode when changing to new major mode
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;;Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))

(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere


;; Presentations ---------------------------------------------------------------
(unless (package-installed-p 'org-present)
  (package-install 'org-present))

(defun my/org-present-start ()
  ;; Tweak font sizes in presentation mode
  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
				    (header-line (:height 5.0) variable-pitch)
				    (org-document-title (:height 2.0) org-document-title)
				    (org-code (:height 1.25) org-code)
				    (org-verbatim (:height 1.25) org-verbatim)
				    (org-block (:height 1.25) org-block)
				    (org-block-begin-line (:height 3.0) org-block)))
  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " "))

(defun my/org-present-end ()
  ;; Reset font customisations
  (setq-local face-remapping-alist '((default variable-pitch default)))
  ;; Clear the header line format by setting to `nil'
  (setq header-line-format nil))

;; Registering start and end hooks in org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)


;; Org Mode --------------------------------------------------------------------
;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.5)
                (org-level-2 . 1.3)
                (org-level-3 . 1.1)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font "Segoe UI" :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font "Segoe UI" :weight 'bold :height 1.85)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(unless (package-installed-p 'visual-fill-column)
  (package-install 'visual-fill-column))

;; Enable text centering and line breaks for Org Mode
;; (defun my/org-mode-visual-style ()
;;   (visual-fill-column-mode t)
;;   (visual-fill-line-mode t))

;; (add-hook 'org-mode-hook 'my/org-mode-visual-style)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Other visual effects --------------------------------------------------------

;; Prettify my tags with svg-tag-mode!
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
