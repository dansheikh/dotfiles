;;; init.el --- Emacs configuration file

;; Author: Dan Sheikh

;;; Commentary:

;; Custom Emacs configuration.

;;; Code:

;; (require 'package)
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("melpa" . "https://melpa.org/packages/")
;;                          ("melpa-stable" . "https://stable.melpa.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/"))
;;       package-archive-priorities '(("gnu" . 9)
;;                                    ("melpa" . 10)
;;                                    ("melpa-stable" . 8)
;;                                    ("org" . 9)))

;; (package-initialize)

;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)

;; (use-package auto-package-update
;;   :config
;;   (auto-package-update-maybe)
;;   :custom
;;   (auto-package-update-interval 7)
;;   (auto-package-update-at-time "08:00")
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-show-preview t)
;;   (auto-package-update-delete-old-versions t)
;;   (auto-package-update-hide-results t))

;; Bootstrap straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'config "~/dotfiles/emacs.d/config.el")

;; Disable backup and auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Enable (explicit) auto-save
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 3)

;; Auto-reload buffers on disk file changes
(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)

;; Set root directory
(setq root-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))

;; Enable windmove with default bindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Set width space
(setq preferred-tab-width 2)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Set frame size
(setq default-frame-alist '((width . 225) (height . 65)))

;; Set cursor type
(setq-default cursor-type 'box)

;; Disable menu, scroll, & tool bars
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

;; Vertical window split default
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Enable line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers t)
(dolist (modes '(term-mode-hook
                 shell-mode-hook
                 eshell-mode-hook))
  (add-hook modes (lambda () (display-line-numbers-mode 0))))

;; Set tab (space) width
(setq-default tab-width 2
              indent-tabs-mode nil)

(show-paren-mode 1)

;; Cause escape to quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Auto-indent
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;; Set style
(setq indent-tabs-mode nil)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "bsd")))

(setq c-basic-offset 2)
(setq sh-basic-offset 2)

;; Enable icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Set theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t))

(use-package doom-modeline
  :custom
  (doom-modeline-height 15)
  (doom-modeline-unicode-fallback t)
  :hook
  (after-init . doom-modeline-mode))

;; Enable powerline
;; (use-package powerline)

;; Enable airline
;; (use-package airline-themes
;;   :init
;;   (setq power
;;         line-default-separator           'utf-8
;;         powerline-utf-8-separator-left        #xe0b0
;;         powerline-utf-8-separator-right       #xe0b2
;;         airline-utf-glyph-separator-left      #xe0b0
;;         airline-utf-glyph-separator-right     #xe0b2
;;         airline-utf-glyph-subseparator-left   #xe0b1
;;         airline-utf-glyph-subseparator-right  #xe0b3
;;         airline-utf-glyph-branch              #xe0a0
;;         airline-utf-glyph-readonly            #xe0a2
;;         airline-utf-glyph-linenumber          #xe0a1
;;         airline-cursor-colors                 t
;;         airline-display-directory             'airline-directory-shortened)
;;   :config
;;   (load-theme 'airline-base16_nord t))

;; Set default font
(add-to-list 'default-frame-alist '(font . "Source Code Pro for Powerline-12"))
(set-frame-font "Source Code Pro for Powerline-12")
;; Globally prettify symbols
(global-prettify-symbols-mode 1)
(defun configure-prettify-symbols-alist ()
  "Set prettify symbols alist."
  (setq prettify-symbols-alist '(("map" . ?↦)
                                 ("&&" . ?∧)
                                 ("||" . ?∨)
                                 ("not" . ?¬))))

;; Enable ido mode
(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t
        ido-use-virutal-buffers t))

(use-package ivy
  :diminish
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        enable-recursive-minibuffers t)
  :init
  (ivy-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package swiper
  :after ivy
  :bind
  (("C-s" . swiper)))

(use-package counsel
  :bind
  (:map minibuffer-local-map
        ("C-r" . 'counsel-minibuffer-history))
  :config
  (defun counsel-fzf-dir (arg)
    (counsel-fzf ivy-text (read-directory-name
                           (concat
                            (car (split-string counsel-fzf-cmd)) " in directory: "))))
  (ivy-add-actions
   'counsel-fzf
   '(("s" counsel-fzf-dir "search directory")))
  :init
  (setq counsel-fzf-cmd "fd --type f | fzf -f \"%s\""))

;; Enable avy
(use-package avy
  :config
  (global-set-key (kbd "C-x ,") 'avy-goto-char-timer))

;; Enable company mode
(use-package company
  :after
  lsp-mode
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-echo-delay 0.0)
  (company-backends '(company-capf
                      company-keywords
                      company-semantic
                      company-files
                      company-ispell
                      company-yasnippet))
  (company-selection-wrap-around t)
  (company-tooltip-limit 25)
  (company-show-numbers t)
  :hook
  (prog-mode . company-mode))

(use-package dap-mode)

(use-package helpful
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; Enable Hydra
(use-package hydra)

(defhydra hydra-buffer (:timeout 5)
  "switch buffer"
  ("n" next-buffer "next buffer")
  ("p" previous-buffer "previous buffer")
  ("e" nil "exit" :exit t))

(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("e" nil "exit" :exit t))

;; Enable which-key
(use-package which-key
  :config
  (which-key-mode 1)
  :init
  (setq which-key-idle-delay 0.2)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-add-column-padding 4)
  (setq which-key-max-display-columns 6)
  (setq which-key-separator " » ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-show-remaining-keys t)
  (setq which-key-allow-evil-operators t))

;; Enable general
(use-package general
  :after which-key
  :config
  (general-override-mode 1)
  (general-create-definer benevolent-dictator
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"
    :non-normal-prefix "M-SPC")
  (defun shrink-horizontally ()
    (interactive)
    (shrink-window-horizontally 10))
  (defun enlarge-horizontally ()
    (interactive)
    (enlarge-window-horizontally 10))
  (defun shrink-vertically ()
    (interactive)
    (shrink-window 10))
  (defun enlarge-vertically ()
    (interactive)
    (enlarge-window 10))
  (benevolent-dictator
    ";" (general-simulate-key ";" :which-key ";")
    "c" (general-simulate-key "C-c" :which-key "C-c")
    "h" (general-simulate-key "C-h" :which-key "C-h")
    "x" (general-simulate-key "C-x" :which-key "C-x")
    "TAB" '(ivy-switch-buffer :which-key "switch buffer")
    "SPC" '(counsel-M-x :which-key "M-x")
    "/"   '(counsel-rg :which-key "ripgrep")
    ;; Buffer functionality
    "b"  '(:ignore t :which-key "buffer")
    "bh" '(hydra-buffer/body :which-key "hydra buffer")
    "bk" '(ido-kill-buffer :which-key "buffer kill")
    "bl" '(counsel-ibuffer :which-key "buffer list")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "br" '(counsel-recentf :which-key "recent buffers")
    "bS" '(save-some-buffers :which-key "buffer any save")
    "bs" '(save-buffer :which-key "buffer save")
    ;; Describe functionality
    "d"  '(:ignore t :which-key "describe")
    "df" '(counsel-describe-function :which-key "describe function")
    "dv" '(counsel-describe-variable :which-key "describe variable")
    ;; File functionality
    "f"  '(:ignore t :which-key "file")
    "f." '(counsel-find-file :which-key "file search")
    "ff" '(counsel-fzf :which-key "file fuzzy search")
    "fr" '(ranger :which-key "ranger")
    "."  '(counsel-find-file :which-key "file search")
    ;; Git functionality
    "g"  '(:ignore t :which-key "git")
    "gc" '(counsel-git :which-key "git counsel")
    "gd" '(magit-dispatch-popup :which-key "git dispatch")
    "gs" '(magit-status :which-key "git status")
    ;; Interface functionality
    "i"  '(:ignore t :which-key "interface")
    "ie" '(eshell :which-key "open eshell")
    "im" '(mini-eshell :which-key "open mini-eshell")
    ;; Navigation functionality
    "n"  '(:ignore t :which-key "navigation")
    "nc" '(avy-goto-char :which-key "go-to char")
    "nl" '(avy-goto-line :which-key "go-to line")
    "ns" '(avy-goto-word-0 :which-key "go-to word")
    "nt" '(avy-goto-char-timer :which-key "timed go-to char")
    "nw" '(avy-goto-word-1 :which-key "go-to search word")
    ;; Org
    "o"  '(:ignore t :which-key "org")
    "od" '(org-deadline :which-key "deadline")
    "ot" '(org-time-stamp :which-key "timestamp")
    ;; Project functionality
    "p"  '(:ignore t :which-key "project")
    "pf" '(project--files-in-directory :which-key "find file in directory")
    ;; Quit functionality
    "q"  '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "save & quit")
    "qQ" '(kill-emacs :which-key "quit")
    ;; Search functionality
    "s"  '(:ignore t :which-key "search")
    "ss" '(swiper :which-key "swiper")
    ;; Tree functionality
    "t"  '(:ignore t :which-key "tree")
    "tt" '(neotree-toggle :which-key "neotree-toggle")
    ;; Window functionality
    "w"  '(:ignore t :which-key "window")
    "wh" '(windmove-left :which-key "move left")
    "wj" '(windmove-down :which-key "move down")
    "wk" '(windmove-up :which-key "move up")
    "wl" '(windmove-right :which-key "move right")
    "wo" '(delete-other-windows :which-key "delete other window")
    "wx" '(delete-window :which-key "delete window")
    "w+" '(split-window-right :which-key "split right")
    "w-" '(split-window-below :which-key "split below")
    "w=" '(balance-windows :which-key "balance")
    "w<" '(shrink-horizontally :which-key "shrink horizontally")
    "w>" '(enlarge-horizontally :which-key "enlarge horizontally")
    "w_" '(shrink-vertically :which-key "shrink vertically")
    "w^" '(enlarge-vertically :which-key "enlarge vertically")
    ;; Zoom functionality
    "z" '(:ignore t :which-key "hydra")
    "zz" '(hydra-text-scale/body :which-key "zoom in/out")))

;; Enable evil
(use-package evil
  :config
  (evil-set-initial-state 'term-mode 'emacs)
  (setq evil-default-cursor 'box
        evil-emacs-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-insert-state-cursor 'box
        evil-replace-state-cursor 'box
        evil-operator-state-cursor 'box)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-s") 'swiper)
  (define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point))))
  :custom
  (evil-want-keybinding nil)
  (eval-want-integration t)
  :init
  (evil-mode t))

;; Enable evil collection
(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init))

;; Enable evil surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Enable evil embrace
(use-package evil-embrace
  :config
  (evil-embrace-enable-evil-surround-integration))

;; Enable evil easymotion
(use-package evil-easymotion
  :config
  (evilem-default-keybindings ","))

(use-package sass-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode)))

;; Enable web development support
(use-package web-mode)

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

;; Enable go
(use-package go-mode)

(use-package lispy
  :hook
  (clojure-mode . (lambda () (lispy-mode 1)))
  (clojurec-mode . (lambda () (lispy-mode 1)))
  (clojurescript-mode . (lambda () (lispy-mode 1)))
  (emacs-lisp-mode . (lambda () (lispy-mode 1)))
  (minibuffer-setup . conditionally-enable-lispy))

(use-package lispyville
  :config
  (lispyville-set-key-theme '(operators c-w additional slurp/barf-cp))
  :hook
  (lispy-mode . lispyville-mode))

;; (use-package paredit)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; (use-package cider
;;   :init
;;   (setq cider-show-error-buffer nil)
;;   (setq cider-repl-display-help-banner nil)
;;   (setq cider-repl-shortcut-dispatch-char ?\;)
;;   (cider-auto-test-mode 1)
;;   (add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)
;;   (add-hook 'cider-repl-mode-hook 'cider-company-enable-fuzzy-completion))

(use-package clojure-mode
  :hook
  (clojure-mode . inf-clojure-minor-mode)
  (clojurec-mode . inf-clojure-minor-mode)
  (clojurescript-mode . inf-clojure-minor-mode)
  ;; (add-hook 'clojure-mode-hook 'cider-mode)
  ;; (add-hook 'clojurescript-mode-hook 'cider-mode)
  ;; (add-hook 'cider-repl-mode-hook 'lispy-mode)
  ;; (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (clojure-mode . eldoc-mode)
  (clojurec-mode . eldoc-mode)
  (clojurescript-mode . eldoc-mode)
  (clojure-mode . rainbow-delimiters-mode)
  (clojurec-mode . rainbow-delimiters-mode)
  (clojurescript-mode . rainbow-delimiters-mode))

(use-package inf-clojure
  :custom
  (inf-clojure-prompt-read-only nil)
  (inf-clojure-custom-repl-type "clj")
  (inf-clojure-custom-startup "clj -A:compliment")
  :hook
  (inf-clojure-mode . eldoc-mode)
  (inf-clojure-mode . (lambda () (setq completion-at-point-functions nil))))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :init
  (setq js-indent-level preferred-tab-width)
  :interpreter ("node" . js2-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1))

(use-package tide
  :config
  (setq js-indent-level preferred-tab-width
        tide-completion-detailed t
        tide-always-show-documentation t
        tide-server-max-response-length 524288))

(use-package typescript-mode
  :hook ((typescript-mode . rainbow-delimiters-mode)
         (typescript-mode . setup-tide-mode))
  :init
  (setq typescript-indent-level preferred-tab-width))

(use-package prettier-js
  :init
  (setq prettier-js-args '("--arrow-parens" "avoid"))
  (add-hook 'j2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package json-mode
  :init
  (add-hook 'json-mode-hook 'prettier-js-mode))

;; Enable markdown
(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

;; Enable markdown preview
(use-package markdown-preview-mode
  :requires markdown-mode)

;; Enable python
(use-package python-mode
  :config
  (setq python-shell-interpreter (substring (shell-command-to-string "which ipython") 0 -1)
        python-shell-interpreter-args "--simple-prompt -i")
  :hook
  (python-mode . (lambda ()
                   (setq tab-width 4)
                   (setq python-indent-offset 4)))
  (python-mode . lsp-deferred))

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

;; Enable rust
(use-package rust-mode
  :init
  (setq rust-format-on-save t))

;; Enable scala and sbt
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command)

;; Enable LSP
(use-package lsp-mode
  :commands lsp
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration t)
  (dolist (modes '(clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,modes . "clojure")))
  (setq lsp-clojure-custom-server-command '("bash" "-c" "~/.emacs.d/.cache/lsp/clojure/clojure-lsp")
        lsp-enable-indentation nil)
  :hook
  ((clojure-mode . lsp-deferred)
   (clojurec-mode . lsp-deferred)
   (clojurescript-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (scala-mode . lsp-deferred)
   (go-mode . lsp-deferred)
   (terraform-mode . lsp-deferred)
   (vue-mode . lsp-deferred)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-enable nil)
  :requires
  lsp-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t))

;; Enable project
(use-package project)

;; Enable projectile
;; (use-package projectile
;;   :config
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1)
;;   :init
;;   (when (file-directory-p "~/projects")
;;     (setq projectile-project-search-path '("~/projects")))
;;   (setq projectile-completion-system 'ivy
;;         projectile-switch-project-action 'neotree-projectile-action))

(use-package ranger
  :init
  (setq ranger-override-dired 'ranger
        ranger-cleanup-eagerly t
        ranger-modify-header t
        ranger-header-func 'ranger-header-line
        ranger-parent-header-func 'ranger-parent-header-line
        ranger-preview-header-func 'ranger-preview-header-line
        ranger-hide-cursor nil
        ranger-footer-delay 0.2
        ranger-preview-delay 0.2
        ranger-parent-depth 2
        ranger-preview-file t
        ranger-width-preview 0.5
        ranger-dont-show-binary t
        ranger-excluded-extensions '("iso" "mkv" "mp3" "mp4")))

(use-package eshell
  :after
  evil
  :config
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (define-key evil-normal-state-map (kbd "C-r") 'counsel-esh-history)
  (define-key evil-insert-state-map (kbd "C-r") 'counsel-esh-history)
  (define-key evil-visual-state-map (kbd "C-r") 'counsel-esh-history)
  (define-key evil-normal-state-map (kbd "<home>") 'eshell-bol)
  (define-key evil-insert-state-map (kbd "<home>") 'eshell-bol)
  (define-key evil-visual-state-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  :custom
  (eshell-history-size 1000)
  (eshell-buffer-maximum-lines 1000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input t)
  :hook
  (eshell-pre-command . eshell-save-some-history))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; Enable YASnippet
(use-package yasnippet
  :init
  (yas-global-mode t))

;; Enable flycheck
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (provide 'init-flycheck)
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled idle-buffer-switch idle-change save)
        flycheck-idle-buffer-switch-delay 1.0
        flycheck-idle-change-delay 3.0))

(use-package flycheck-color-mode-line
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip)

;; Enable neotree
(use-package neotree
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-smart-open t
        neo-autorefresh nil))

;; Enable magit
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

;; Enable org mode
(use-package org
  :config
  (auto-fill-mode 0)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (add-hook 'org-mode-hook (lambda () (org-babel-do-load-languages
                                       'org-babel-load-languages
                                       '((emacs-lisp . t)))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dms/org-babel-tangle-config)))
  :init
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(i)" "|" "CANCELLED(c)" "DONE(d)")))
  (setq org-log-done 'time)
  (setq org-hide-leading-stars t)
  (setq org-ellipsis " \u25BE")
  (setq org-agenda-files
        (append (file-expand-wildcards "~/org/agendas/*.org"))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("\u2605" "\u29BF" "\u25EC" "\u29BE" "\u25CF" "\u25E6" "\u2022"))
  :requires org)

(use-package org-sticky-header
  :config
  (add-hook 'org-mode-hook (lambda () (org-sticky-header-mode)))
  :requires org)

(use-package org-journal
  :requires org)

;; (use-package org-projectile
;;   :after (org projectile)
;;   :config
;;   (setq org-projectile-projects-file "~/.org/projects/todos.org"
;;         org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
;;   (push (org-projectile-project-todo-entry) org-capture-templates))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("yml" . "src yaml"))

(provide 'init)

;;; init.el ends here
