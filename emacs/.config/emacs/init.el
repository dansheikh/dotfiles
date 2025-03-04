;; [[file:../../emacs.org::*Notes][Notes:1]]
;;; init.el --- Emacs configuration file

;; Author: Dan Sheikh

;;; Commentary:

;; Custom Emacs configuration.

;;; Code:
;; Notes:1 ends here

;; [[file:../../emacs.org::*Package Management][Package Management:1]]
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/"))
      package-archive-priorities '(("gnu" . 9)
                                   ("melpa" . 10)
                                   ("melpa-stable" . 8)
                                   ("org" . 9)))

(package-initialize)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq use-package-ensure-function 'quelpa)
(setq use-package-always-ensure t)
(setq quelpa-upgrade-interval 7)
(add-hook #'after-init-hook #'quelpa-upgrade-all-maybe)

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
;; Package Management:1 ends here

;; [[file:../../emacs.org::*Prefixes][Prefixes:1]]
(defvar local-leader-key "C-.")
(defvar next-prefix "M-]")
(defvar prev-prefix "M-[")
;; Prefixes:1 ends here

;; [[file:../../emacs.org::*Requirements][Requirements:1]]
(require 'config "~/.config/emacs/config.el")

(let* ((path (expand-file-name "lisp" user-emacs-directory))
       (local-pkgs (mapcar (lambda (file-path) (directory-file-name (file-name-directory file-path))) (directory-files-recursively path "\\.el$"))))
  (if (file-accessible-directory-p path)
      (mapc (apply-partially 'add-to-list 'load-path) local-pkgs)
    (make-directory path :parents)))
;; Requirements:1 ends here

;; [[file:../../emacs.org::*Customizations][Customizations:1]]
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))
;; Customizations:1 ends here

;; [[file:../../emacs.org::*Shell Environment][Shell Environment:1]]
(when (or (daemonp) (memq window-system '(mac ns x)))
  (danish--set-exec-path-by-shell))
;; Shell Environment:1 ends here

;; [[file:../../emacs.org::*Buffer & File Management][Buffer & File Management:1]]
;; Disable auto-save, backup and lock files
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Enable (explicit) auto-save
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 10)

;; Auto-reload buffers on disk file changes
(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)

;; Set root directory
(setq root-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))
;; Buffer & File Management:1 ends here

;; [[file:../../emacs.org::*History Management][History Management:1]]
(setq history-length 25)
(recentf-mode 1)
(savehist-mode 1)
;; History Management:1 ends here

;; [[file:../../emacs.org::*Window Management][Window Management:1]]
;; Enable windmove with default bindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

(use-package ace-window
  :bind
  (("M-o" . 'ace-window))
  :custom
  (ace-window-display-mode 1))
;; Window Management:1 ends here

;; [[file:../../emacs.org::*Keybindings][Keybindings:1]]
;; Cause escape to quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Tab indentation/completion
(setq tab-always-indent 'complete)

;; Auto-indent
;; (define-key global-map (kbd "RET") 'newline-and-indent)
;; Keybindings:1 ends here

;; [[file:../../emacs.org::*Styles][Styles:1]]
;; Set style
(setq indent-tabs-mode nil)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "bsd")))

(setq c-basic-offset 2)
(setq sh-basic-offset 2)
;; Styles:1 ends here

;; [[file:../../emacs.org::*Core][Core:1]]
;; Disable startup screen
(setq inhibit-startup-screen t)

;; Set width space
(setq preferred-tab-width 2)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Set cursor type
(setq-default cursor-type 'box)
(setq-default blink-cursor-blinks 0)

;; Vertical window split default
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Enable line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(dolist (modes '(term-mode-hook
                 shell-mode-hook
                 eshell-mode-hook))
  (add-hook modes (lambda () (display-line-numbers-mode 0))))

;; Set tab (space) width
(setq-default tab-width 2
              indent-tabs-mode nil)

(electric-pair-mode 1)

(show-paren-mode 1)

;; Enable icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Set theme
(use-package ef-themes
  :config (load-theme 'ef-elea-dark :no-confirm)
  :custom
  (ef-themes-headings '((0 variable-pitch light 1.9)
                        (1 variable-pitch light 1.8)
                        (2 variable-pitch regular 1.7)
                        (3 variable-pitch regular 1.6)
                        (4 variable-pitch regular 1.5)
                        (5 variable-pitch 1.4)
                        (6 variable-pitch 1.3)
                        (7 variable-pitch 1.2)
                        (t variable-pitch 1.1)))
  (ef-themes-mixed-fonts t)
  (ef-themes-to-toggle '(ef-elea-dark ef-owl))
  (ef-themes-variable-pitch-ui nil)
  :quelpa (ef-themes :fetcher git
                     :url "https://github.com/protesilaos/ef-themes.git"))

  ;; (use-package modus-themes
  ;;   :config (load-theme 'modus-vivendi t)
  ;;   :custom
  ;;   (modus-themes-bold-constructs t)
  ;;   (modus-themes-completion '(opinionated))
  ;;   (modus-themes-italic-constructs t)
  ;;   (modus-themes-mode-line '(accented borderless padded))
  ;;   (modus-themes-org-blocks 'tinted-background)
  ;;   (modus-themes-paren-match '(bold intense))
  ;;   (modus-themes-prompts '(bold intense))
  ;;   (modus-themes-region '(bg-only))
  ;;   (modus-themes-scale-headings t))

  ;; Prettify symbols
  (defun configure-prettify-symbols-alist ()
    "Set prettify symbols alist."
    (interactive)
    (setq prettify-symbols-alist '(("map" . ?↦)
                                   ("&&" . ?∧)
                                   ("||" . ?∨)
                                   ("not" . ?¬)))
    (prettify-symbols-mode 1))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . (lambda ()
                 (configure-prettify-symbols-alist)
                 (flymake-mode))))

(require 'danish-mode-line "~/.config/emacs/lisp/packages/mode-line/danish-mode-line.el")
;; Core:1 ends here

;; [[file:../../emacs.org::*Completion][Completion:1]]
(use-package avy
  :bind
  (("M-j" . 'avy-goto-char-timer)))

(use-package corfu
  :bind
  (:map corfu-map
   ("M-SPC" . corfu-insert-separator)
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous)
   ("C-g" . corfu-quit)
   ([return] . corfu-insert))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-preview-current 'insert)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package cape
  :after (:all corfu eglot)
  :bind
  (("C-c p p" . completion-at-point)
   ("C-c p d" . cape-dabbrev)
   ("C-c p e" . cape-elisp-block)
   ("C-c p f" . cape-file))
  :hook
  ((prog-mode special-mode text-mode) . (lambda () (setq-local completion-at-point-functions
                                                               (list (cape-capf-super
                                                                      #'cape-dabbrev
                                                                      #'cape-file
                                                                      #'cape-keyword
                                                                      #'cape-elisp-block
                                                                      #'cape-dict))))))

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot (styles orderless)))))

(use-package consult
  :bind
  (("C-c h" . 'consult-history)
   ("C-x b" . 'consult-buffer)
   ("M-g e" . 'consult-compile-error)
   ("M-g f" . 'consult-flymake)
   ("M-g g" . 'consult-goto-line)
   ("M-g M-g" . 'consult-goto-line)
   ("M-g o" . 'consult-outline)
   ("M-g i" . 'consult-imenu)
   ("M-g I" . 'consult-imenu-multi)
   ("M-s d" . 'consult-fd)
   ("M-s D" . 'consult-locate)
   ("M-s g" . 'consult-grep)
   ("M-s G" . 'consult-git-grep)
   ("M-s r" . 'consult-ripgrep)
   ("M-s l" . 'consult-line)
   ("M-s L" . 'consult-line-multi)
   :map isearch-mode-map
   ("M-s l" . 'consult-line))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :demand t
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (:all consult embark)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :after (:all vertico)
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package vertico
  :custom
  (vertico-cycle t)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode))
;; Completion:1 ends here

;; [[file:../../emacs.org::*Debugging][Debugging:1]]
;; (use-package dap-mode)
;; Debugging:1 ends here

;; [[file:../../emacs.org::*Environment Management][Environment Management:1]]
(use-package envrc
  :hook (after-init . envrc-global-mode))
;; Environment Management:1 ends here

;; [[file:../../emacs.org::*Help][Help:1]]
(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))
;; Help:1 ends here

;; [[file:../../emacs.org::*Interactivity][Interactivity:1]]
(use-package ido
  :config
  (setq ido-enable-flex-matching t
        ido-use-virutal-buffers t)
  (ido-mode t)
  :ensure nil)
;; Interactivity:1 ends here

;; [[file:../../emacs.org::*Key Definitions][Key Definitions:1]]
(use-package hydra
  :config
  (defhydra hydra-buffer (:timeout 5)
    "switch buffer"
    ("n" next-buffer "next buffer")
    ("p" previous-buffer "previous buffer")
    ("e" nil "exit" :exit t))
  (defhydra hydra-text-scale (:timeout 5)
    "scale text"
    ("j" text-scale-decrease "out")
    ("k" text-scale-increase "in")
    ("e" nil "exit" :exit t)))

(use-package which-key
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1)
  :custom
  (which-key-add-column-padding 4)
  (which-key-allow-evil-operators t)
  (which-key-idle-delay 0.2)
  (which-key-max-display-columns 6)
  (which-key-popup-type 'side-window)
  (which-key-prefix-prefix "+")
  (which-key-separator " » ")
  (which-key-show-remaining-keys t)
  (which-key-side-window-max-height 0.33)
  :demand t)

(use-package general
  :after (:all which-key)
  :config
  (general-override-mode 1)
  (general-evil-setup t)
  (general-def
    :keymaps 'global
    [escape] 'keyboard-escape-quit)
  (general-def
    :keymaps 'global
    :states '(normal visual)
    "C-d" 'evil-scroll-down
    "C-u" 'evil-scroll-up)
  (general-def
    :keymaps 'global
    :states '(insert)
    "C-u" '(lambda ()
             (interactive)
             (evil-delete (point-at-bol) (point))))
  (general-create-definer benevolent-dictator
    :states '(emacs insert normal visual)
    :prefix "SPC"
    :global-prefix "M-SPC")
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
    "TAB" '(consult-buffer :which-key "switch buffer")
    "SPC" '(execute-extended-command :which-key "M-x")
    "/"   '(consult-ripgrep :which-key "ripgrep")
    ;; Buffer functionality
    "b"  '(:ignore t :which-key "buffer")
    "bh" '(hydra-buffer/body :which-key "hydra buffer")
    "bk" '(ido-kill-buffer :which-key "buffer kill")
    "bl" '(consult-buffer :which-key "buffer list")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "bS" '(save-some-buffers :which-key "buffer any save")
    "bs" '(save-buffer :which-key "buffer save")
    ;; Describe functionality
    "d"  '(:ignore t :which-key "describe")
    "df" '(describe-function :which-key "describe function")
    "dv" '(describe-variable :which-key "describe variable")
    ;; File functionality
    "f"  '(:ignore t :which-key "file")
    "fd" '(dired :which-key "dired")
    "fl" '(consult-locate :which-key "locate")
    "ff" '(consult-fd :which-key "find")
    "fs" '(consult-ripgrep :which-key "fuzzy search")
    ;; Git functionality
    "g"  '(:ignore t :which-key "git")
    "gc" '(consult-git-grep :which-key "git consult")
    "gd" '(magit-dispatch-popup :which-key "git dispatch")
    "gs" '(magit-status :which-key "git status")
    ;; Interface functionality
    "i"  '(:ignore t :which-key "interface")
    "ie" '(eshell :which-key "open eshell")
    "im" '(mini-eshell :which-key "open mini-eshell")
    ;; Lint functionality
    "l"  '(:ignore t :which-key "lint")
    "lf" '(consult-flymake :which-key "flymake consult")
    "ln" '(flymake-goto-next-error :which-key "next flymake error")
    "lp" '(flymake-goto-prev-error :which-key "previous flymake error")
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
    "sl" '(consult-line :which-key "consult line")
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
    "zz" '(hydra-text-scale/body :which-key "zoom in/out"))
  :demand t)

(use-package evil
  :after (:all ef-themes general)
  :custom
  (evil-emacs-state-cursor '(box "medium purple"))
  (evil-insert-state-cursor '((bar . 2) "dodger blue"))
  (evil-motion-state-cursor '(box "light sea green"))
  (evil-normal-state-cursor '(box "medium sea green"))
  (evil-operator-state-cursor '(box "medium spring green"))
  (evil-replace-state-cursor '(hollow "magenta"))
  (evil-visual-state-cursor) '(hollow "gold")
  (evil-set-undo-system 'undo-redo)
  (evil-default-state 'normal)
  (evil-search-module 'evil-search)
  (evil-split-window-below nil)
  (evil-vsplit-window-right t)
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-fine-undo t)
  :init
  (evil-mode 1))

(use-package evil-collection
  :after (:all evil)
  :config
  (evil-collection-init)
  (evil-set-initial-state 'eat-mode 'emacs))

(use-package evil-surround
  :after (:all evil)
  :config
  (global-evil-surround-mode 1))

(use-package evil-embrace
  :after (:all evil evil-surround)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-easymotion
  :after (:all evil)
  :config
  (evilem-default-keybindings ","))
;; Key Definitions:1 ends here

;; [[file:../../emacs.org::*Apheleia][Apheleia:1]]
(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'nixpkgs-fmt apheleia-formatters) '("nixpkgs-fmt"))
  (setf (alist-get 'prettierd apheleia-formatters) '("prettierd" "--stdin-filepath" filepath))
  (setf (alist-get 'ruff apheleia-formatters) '("ruff" "format" "-"))
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixpkgs-fmt)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'terraform-mode apheleia-mode-alist) 'terraform)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettierd))
;; Apheleia:1 ends here

;; [[file:../../emacs.org::*AsciiDoc][AsciiDoc:1]]
(use-package adoc-mode)
;; AsciiDoc:1 ends here

;; [[file:../../emacs.org::*CSS][CSS:1]]
(use-package sass-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode)))
;; CSS:1 ends here

;; [[file:../../emacs.org::*F#][F#:1]]
(use-package fsharp-mode)
;; F#:1 ends here

;; [[file:../../emacs.org::*Go][Go:1]]
(use-package go-mode)
;; Go:1 ends here

;; [[file:../../emacs.org::*Groovy][Groovy:1]]
(use-package groovy-mode)
;; Groovy:1 ends here

;; [[file:../../emacs.org::*HCL][HCL:1]]
(use-package hcl-mode)
;; HCL:1 ends here

;; [[file:../../emacs.org::*HTML][HTML:1]]
(use-package web-mode)

(use-package emmet-mode
  :hook
  ((css-mode sgml-mode) . emmet-mode))
;; HTML:1 ends here

;; [[file:../../emacs.org::*JavaScript & TypeScript][JavaScript & TypeScript:1]]
(use-package js2-mode
  :mode "\\.js\\'"
  :hook
  (js2-mode . rainbow-delimiters-mode)
  :init
  (setq js-indent-level preferred-tab-width)
  :interpreter ("node" . js2-mode))

(use-package typescript-mode
  :hook
  (typescript-mode . rainbow-delimiters-mode)
  :init
  (setq typescript-indent-level preferred-tab-width))

(use-package json-mode)
;; JavaScript & TypeScript:1 ends here

;; [[file:../../emacs.org::*Lisp][Lisp:1]]
(use-package rainbow-delimiters
  :hook
  ((cider-mode cider-repl-mode clojure-mode clojurec-mode clojurescript-mode emacs-lisp-mode eval-expression-minibuffer-setup ielm-mode lisp-interaction-mode lisp-mode scheme-mode prog-mode) . rainbow-delimiters-mode))

(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

(use-package cider
  :custom
  (cider-show-error-buffer nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-shortcut-dispatch-char ?\;)
  :hook
  ((clojure-mode clojurescript-mode) . cider-mode)
  :init
  (cider-auto-test-mode 1)
  :requires queue)

(use-package clojure-mode
  :hook
  ;; ((clojure-mode clojurec-mode clojurescript-mode) . inf-clojure-minor-mode)
  ((clojure-mode clojurec-mode clojurescript-mode) . eldoc-mode))

;; (use-package inf-clojure
;;   :custom
;;   (inf-clojure-prompt-read-only nil)
;;   (inf-clojure-custom-repl-type "clj")
;;   (inf-clojure-custom-startup "clj -A:compliment")
;;   :hook
;;   (inf-clojure-mode . eldoc-mode)
;;   (inf-clojure-mode . (lambda () (setq completion-at-point-functions nil))))

;; (use-package smartparens
;;   :bind
;;   (:map smartparens-mode-map
;;    ("C-M-a" . 'sp-beginning-of-sexp)
;;    ("C-M-e" . 'sp-end-of-sexp)
;;    ("C-M-d" . 'sp-down-sexp)
;;    ("C-M-u" . 'sp-up-sexp)
;;    ("C-S-d" . 'sp-backward-down-sexp)
;;    ("C-S-u" . 'sp-backward-up-sexp)
;;    ("C-M-f" . 'sp-forward-sexp)
;;    ("C-M-b" . 'sp-backward-sexp)
;;    ("C-M-n" . 'sp-next-sexp)
;;    ("C-M-p" . 'sp-previous-sexp)
;;    ("C-S-f" . 'sp-forward-symbol)
;;    ("C-S-b" . 'sp-backward-symbol)
;;    ("M-[" . 'sp-backward-unwrap-sexp)
;;    ("M-]" . 'sp-unwrap-sexp)
;;    ("C-)" . 'sp-forward-slurp-sexp)
;;    ("M-}" . 'sp-forward-barf-sexp)
;;    ("C-(" . 'sp-backward-slurp-sexp)
;;    ("M-{" . 'sp-backward-barf-sexp))
;;   :config
;;   (require 'smartparens-config)
;;   (smartparens-global-mode t)
;;   (smartparens-global-strict-mode t)
;;   (sp-pair "(" ")" :wrap "C-c )")
;;   (sp-pair "[" "]" :wrap "C-c ]")
;;   (sp-pair "{" "}" :wrap "C-c }")
;;   (sp-pair "'" "'" :wrap "C-c '")
;;   (sp-pair "\"" "\"" :wrap "C-c \"")
;;   (sp-pair "_" "_" :wrap "C-c _")
;;   (sp-pair "`" "`" :wrap "C-c `"))

(use-package paredit
  :hook
  ((cider-mode cider-repl-mode clojure-mode clojurec-mode clojurescript-mode emacs-lisp-mode eval-expression-minibuffer-setup ielm-mode lisp-interaction-mode lisp-mode scheme-mode) . enable-paredit-mode))
;; Lisp:1 ends here

;; [[file:../../emacs.org::*Markdown][Markdown:1]]
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package markdown-preview-mode
  :requires markdown-mode)
;; Markdown:1 ends here

;; [[file:../../emacs.org::*Nix][Nix:1]]
(use-package nix-mode
  :mode "\\.nix\\'")
;; Nix:1 ends here

;; [[file:../../emacs.org::*Python][Python:1]]
(use-package python-mode
  :custom
  (python-shell-interpreter (substring (shell-command-to-string "which ipython") 0 -1))
  (python-shell-interpreter-args "--simple-prompt -i")
  :hook
  ((python-mode python-ts-mode) . (lambda ()
                                    (setq tab-width 4)
                                    (setq python-indent-offset 4))))
;; Python:1 ends here

;; [[file:../../emacs.org::*R][R:1]]
(use-package ess)
(use-package quarto-mode
  :mode ("\\.Rmd\\'" . poly-quarto-mode))
;; R:1 ends here

;; [[file:../../emacs.org::*Rust][Rust:1]]
(use-package rust-mode
  :custom
  (rust-format-on-save t))
;; Rust:1 ends here

;; [[file:../../emacs.org::*Scala][Scala:1]]
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command)
;; Scala:1 ends here

;; [[file:../../emacs.org::*Terraform][Terraform:1]]
(use-package terraform-mode)
;; Terraform:1 ends here

;; [[file:../../emacs.org::*Tree-Sitter][Tree-Sitter:1]]
(setq treesit-language-source-alist
      '((css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (groovy "https://github.com/murtaza64/tree-sitter-groovy" "main" "src")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown" "master" "src")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml" "master" "src")))

(dolist (treesit-lang-src treesit-language-source-alist)
  (let ((name (car treesit-lang-src)))
    (unless (treesit-language-available-p name)
      (treesit-install-language-grammar name))))

(setq major-mode-remap-alist
      '((css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (html-mode . html-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)
        (toml-mode . toml-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)))
;; Tree-Sitter:1 ends here

;; [[file:../../emacs.org::*YAML][YAML:1]]
(use-package yaml-pro
  :after (:all yaml-mode)
  :hook
  ((yaml-mode yaml-ts-mode) . yaml-pro-ts-mode))
;; YAML:1 ends here

;; [[file:../../emacs.org::*Language Server Protocol][Language Server Protocol:1]]
(use-package eglot
  :after (:all project)
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-stay-out-of 'flymake)
  :ensure nil
  :hook
  (((clojure-mode ess-mode go-mode go-ts-mode js2-mode js-ts-mode markdown-mode nix-mode python-mode python-ts-mode terraform-mode typescript-mode typescript-ts-mode) . eglot-ensure)
   (eglot-managed-mode . (lambda ()
                           (setq-local completion-at-point-functions
                                       (list (cape-capf-super
                                              #'eglot-completion-at-point
                                              #'cape-dabbrev 
                                              #'cape-file))
                                       eldoc-documentation-strategy
                                       'eldoc-documentation-compose-eagerly)
                           (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
                           (flymake-mode t)))))
;; Language Server Protocol:1 ends here

;; [[file:../../emacs.org::*Project Management][Project Management:1]]
(use-package project)
;; Project Management:1 ends here

;; [[file:../../emacs.org::*Search][Search:1]]
(use-package dired
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  :custom
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (file-name-shadow-mode 1)
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (search-whitespace-regexp ".*?")
  :ensure nil)
;; Search:1 ends here

;; [[file:../../emacs.org::*Shells][Shells:1]]
(use-package eshell
  :after (:all evil)
  :config
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
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
  :ensure nil
  :hook
  ((eshell-pre-command . eshell-save-some-history)))

(use-package eshell-git-prompt
  :config (eshell-git-prompt-use-theme 'powerline))
;; Shells:1 ends here

;; [[file:../../emacs.org::*Snippets][Snippets:1]]
(use-package tempel
  :bind
  (("M-+" . tempel-complete)
   ("M-*" . tempel-insert))
  :init
  (defun setup-tempel-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'setup-tempel-capf))

;; (use-package yasnippet
;;   :init (yas-global-mode t))

;; (use-package yasnippet-snippets
;;   :after (:all yasnippet))

;; (use-package cape-yasnippet
;;   :after (:all yasnippet)
;;   :ensure (cape-yasnippet
;;            :branch "master"
;;            :host github
;;            :repo "elken/cape-yasnippet"
;;            :type git)
;;   :init (add-to-list 'completion-at-point-functions #'cape-yasnippet))
;; Snippets:1 ends here

;; [[file:../../emacs.org::*Syntax][Syntax:1]]
(use-package flymake
  :bind
  (("M-g n" . 'flymake-goto-next-error)
   ("M-g p" . 'flymake-goto-prev-error))
  :ensure nil)

(use-package flymake-eslint
  :hook
  (envrc-mode . (lambda () (when (or (derived-mode-p 'js-ts-mode)
                                     (derived-mode-p 'typescript-ts-mode))
                             (flymake-eslint-enable))))
  :init
  (setq flymake-eslint-defer-binary-check t
        flymake-eslint-show-rule-name t))

(use-package flymake-ruff
  :hook
  ((python-mode python-ts-mode) . setup-flymake-ruff-backend)
  :load-path "lisp/packages/flymake/ruff")

(use-package flymake-sqlfluff
  :hook
  (sql-mode . setup-flymake-sqlfluff-backend)
  :load-path "lisp/packages/flymake/sqlfluff")

;; (use-package flycheck
;;   :config
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   (provide 'init-flycheck)
;;   :init
;;   (setq flycheck-check-syntax-automatically '(mode-enabled idle-buffer-switch idle-change save)
;;         flycheck-idle-buffer-switch-delay 1.0
;;         flycheck-idle-change-delay 3.0))

;; (use-package flycheck-color-mode-line
;;   :config
;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; (use-package flycheck-pos-tip)
;; Syntax:1 ends here

;; [[file:../../emacs.org::*Terminals][Terminals:1]]
(use-package eat
  :after (:all evil evil-collection)
  :quelpa (eat :fetcher git
               :url "https://codeberg.org/akib/emacs-eat.git"
               :files ("*.el" "dir"
                       "*.info" "*.texi"
                       "*.ti" ("e" "e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (evil-set-initial-state 'eat-mode 'emacs)
  :hook
  (eshell-load . eat-eshell-mode))

;; (use-package vterm)
;; Terminals:1 ends here

;; [[file:../../emacs.org::*Version Control][Version Control:1]]
(use-package transient)

(use-package magit
  :after (:all transient)
  :bind
  (("C-x g" . 'magit-status)
   ("C-x M-g" . 'magit-dispatch-popup)))
;; Version Control:1 ends here

;; [[file:../../emacs.org::*Core][Core:1]]
(use-package org
  :config
  (auto-fill-mode 0)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "IN PROGRESS(i)" "|" "CANCELLED(c)" "DONE(d)")))
  (org-log-done 'time)
  (org-hide-leading-stars t)
  (org-ellipsis " \u25BE")
  (org-agenda-files
   (append (file-expand-wildcards "~/org/agendas/*.org")))
  :ensure nil
  :hook
  ((org-mode . (lambda () (org-babel-do-load-languages
                           'org-babel-load-languages
                           '((emacs-lisp . t)))))
   (org-mode . (lambda () (add-hook 'after-save-hook #'danish--org-babel-tangle-config)))))

(use-package org-bullets
  :config (setq org-bullets-bullet-list '("\u2605" "\u29BF" "\u25EC" "\u29BE" "\u25CF" "\u25E6" "\u2022"))
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :requires org)

(use-package org-journal
  :requires org)

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :requires org)
;; Core:1 ends here

;; [[file:../../emacs.org::*Late Hooks][Late Hooks:1]]
(add-hook 'envrc-mode-hook #'danish--add-local-node-bin-to-exec-path)
;; Late Hooks:1 ends here

;; [[file:../../emacs.org::*Structure Templates][Structure Templates:1]]
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("yml" . "src yaml"))
;; Structure Templates:1 ends here

;; [[file:../../emacs.org::*Provisions & Footnotes][Provisions & Footnotes:1]]
(provide 'init)

;;; init.el ends here
;; Provisions & Footnotes:1 ends here
