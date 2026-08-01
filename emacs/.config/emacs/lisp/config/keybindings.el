;;; keybindings.el --- Centralized keybinding configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>

;;; Commentary:
;;
;; This file contains all general.el definers and global keybinding configurations.
;; Uses SPC as leader in normal/visual, M-SPC in insert/emacs.
;; Uses , as local-leader for major-mode specific bindings.
;;
;; Organization:
;; - Global definers and non-modal bindings
;; - Mode-specific keybindings (Eglot, Eshell, Dired, etc.)
;; - Leader key bindings organized by category

;;; Code:

;;; =============================================================================
;;; General.el Definers
;;; =============================================================================

;; Use 'general-override-mode-map rather than the 'override alias on all
;; definers. In current General.el, 'override combined with :states goes
;; through an internal set-local code path that is broken. The explicit
;; keymap symbol bypasses that path entirely while preserving identical
;; runtime behaviour — general-override-mode-map is exactly what 'override
;; resolves to, and general-override-mode (enabled in emacs.org) ensures it
;; sits above all other keymaps in precedence.

;; Main leader key definer
;; Reference: https://github.com/noctuid/general.el#general-create-definer
(general-create-definer +general-global-leader
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

;; Local/major-mode leader
(general-create-definer +general-local-leader
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix ","
  :non-normal-prefix "M-,")

;; Category-specific definers (prefix under SPC)
(general-create-definer +general-global-ai
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC a"
  :non-normal-prefix "M-SPC a")

(general-create-definer +general-global-buffer
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC b"
  :non-normal-prefix "M-SPC b")

(general-create-definer +general-global-file
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC f"
  :non-normal-prefix "M-SPC f")

(general-create-definer +general-global-git
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC g"
  :non-normal-prefix "M-SPC g")

(general-create-definer +general-global-help
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC h"
  :non-normal-prefix "M-SPC h")

(general-create-definer +general-global-application
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC o"
  :non-normal-prefix "M-SPC o")

(general-create-definer +general-global-project
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC p"
  :non-normal-prefix "M-SPC p")

(general-create-definer +general-global-search
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC s"
  :non-normal-prefix "M-SPC s")

(general-create-definer +general-global-toggle
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC t"
  :non-normal-prefix "M-SPC t")

(general-create-definer +general-global-window
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC w"
  :non-normal-prefix "M-SPC w")

(general-create-definer +general-global-notes
  :states '(normal visual motion)
  :keymaps 'general-override-mode-map
  :prefix "SPC n"
  :non-normal-prefix "M-SPC n")

;;; =============================================================================
;;; Global Keybindings (Non-Modal)
;;; =============================================================================

;; These keybindings work in all modes, regardless of Evil state
(general-define-key
 :keymaps 'override
 "M-o" 'ace-window)  ; Quick window switching

;;; =============================================================================
;;; Evil State Management
;;; =============================================================================

;; Evil state transitions: ESC returns to normal state from insert/visual modes
;; Reference: https://github.com/emacs-evil/evil
(general-define-key
 :keymaps '(evil-insert-state-map evil-visual-state-map evil-replace-state-map)
 [escape] 'evil-normal-state)

;; ESC in normal state should cancel operations/quit minibuffer
(general-define-key
 :keymaps 'evil-normal-state-map
 [escape] 'keyboard-escape-quit)

;;; =============================================================================
;;; Evil Window Navigation & Utilities
;;; =============================================================================

;; Evil-specific global bindings for window navigation and utilities
(general-define-key
 :states '(normal visual)
 :keymaps 'override
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-k" 'evil-window-up
 "C-l" 'evil-window-right
 "C-S-d" '+duplicate-line-or-region)

;;; =============================================================================
;;; Mode-Specific Keybindings
;;; =============================================================================

;;; Eglot (LSP) Keybindings
;; These bindings are active when Eglot mode is enabled
;; Reference: https://www.gnu.org/software/emacs/manual/html_node/eglot/
(general-define-key
 :keymaps 'eglot-mode-map
 "C-h ." 'eldoc-box-help-at-point
 "M-." 'xref-find-definitions
 "M-," 'xref-go-back)

;;; Consult Keybindings
;; Enhanced command keybindings for Consult
;; Reference: https://github.com/minad/consult
(general-define-key
 "C-s" 'consult-line
 "C-x b" 'consult-buffer
 "M-y" 'consult-yank-pop
 "M-g g" 'consult-goto-line
 "M-g M-g" 'consult-goto-line
 "M-g i" 'consult-imenu
 "M-g I" 'consult-imenu-multi
 "M-s d" 'consult-fd
 "M-s D" 'consult-locate
 "M-s g" 'consult-grep
 "M-s G" 'consult-git-grep
 "M-s r" 'consult-ripgrep
 "M-s l" 'consult-line
 "M-s L" 'consult-line-multi
 "M-s k" 'consult-keep-lines
 "M-s u" 'consult-focus-lines)

;;; Embark Keybindings
;; Contextual action menu bindings
;; Reference: https://github.com/oantolin/embark
(general-define-key
 "C-." 'embark-act        ; Context action menu
 "C-;" 'embark-dwim       ; Do what I mean
 "C-h B" 'embark-bindings) ; Show all bindings

;;; Marginalia Keybindings
;; Cycle through annotation styles in minibuffer
;; Reference: https://github.com/minad/marginalia
(general-define-key
 :keymaps 'minibuffer-local-map
 "M-A" 'marginalia-cycle)

;;; Corfu Keybindings
;; In-buffer completion popup keybindings
;; Reference: https://github.com/minad/corfu
(general-define-key
 :keymaps 'corfu-map
 "TAB" 'corfu-next
 [tab] 'corfu-next
 "S-TAB" 'corfu-previous
 [backtab] 'corfu-previous
 "RET" 'corfu-insert
 [return] 'corfu-insert
 "M-d" 'corfu-show-documentation
 "M-l" 'corfu-show-location)

;;; Flymake Keybindings
;; Navigate syntax errors and warnings
;; Reference: https://www.gnu.org/software/emacs/manual/html_node/flymake/
(general-define-key
 :keymaps 'flymake-mode-map
 "M-g n" 'flymake-goto-next-error
 "M-g p" 'flymake-goto-prev-error
 "M-g M-n" 'flymake-goto-next-error
 "M-g M-p" 'flymake-goto-prev-error)

;;; Tempel Keybindings
;; Template expansion keybindings
;; Reference: https://github.com/minad/tempel
(general-define-key
 "M-+" 'tempel-complete
 "M-*" 'tempel-insert)

;;; Puni Keybindings
;; Structural editing for balanced expressions
;; Reference: https://github.com/AmaiKinono/puni
(general-define-key
 :keymaps 'puni-mode-map
 :states '(normal visual insert)
 "C-M-f" 'puni-forward-sexp-or-up-list
 "C-M-b" 'puni-backward-sexp-or-up-list
 "C-M-a" 'puni-beginning-of-sexp
 "C-M-e" 'puni-end-of-sexp
 "C-M-u" 'backward-up-list
 "C-M-d" 'down-list
 "C-M-n" 'forward-list
 "C-M-p" 'backward-list
 "C-M-k" 'kill-sexp
 "C-M-w" '+copy-sexp
 "C-M-t" 'puni-transpose
 "C-M-SPC" 'mark-sexp)

;;; Dired Keybindings
;; Directory editor keybindings
;; Reference: https://github.com/Fuco1/dired-hacks
(general-define-key
 :keymaps 'dired-mode-map
 "TAB" 'dired-subtree-toggle
 "<backtab>" 'dired-subtree-cycle)

;;; Eshell Evil Keybindings
;; Evil mode keybindings for Eshell
;; Reference: https://www.gnu.org/software/emacs/manual/html_node/eshell/
(with-eval-after-load 'evil
  (with-eval-after-load 'eshell
    (evil-define-key 'normal eshell-mode-map (kbd "<home>") 'eshell-bol)
    (evil-define-key 'insert eshell-mode-map (kbd "<home>") 'eshell-bol)
    (evil-define-key 'visual eshell-mode-map (kbd "<home>") 'eshell-bol)))

;;; =============================================================================
;;; Leader Key Bindings
;;; =============================================================================

;;; Top-Level Leader Bindings
;; Direct SPC prefix commands
(+general-global-leader
  "SPC" '(execute-extended-command :which-key "M-x")
  ":" '(eval-expression :which-key "eval")
  ";" '(pp-eval-last-sexp :which-key "eval last sexp")
  "u" '(universal-argument :which-key "universal arg")
  "!" '(shell-command :which-key "shell command")
  "X" '(org-capture :which-key "org capture"))

;;; AI Tools Bindings (SPC a)
;; gptel LLM chat client and agent-shell ACP agent integration
(+general-global-ai
 "" '(:ignore t :which-key "AI")
 
 ;; gptel
 "a" '(gptel :which-key "open chat")
 "s" '(gptel-send :which-key "send")
 "m" '(gptel-menu :which-key "menu")
 "r" '(gptel-rewrite :which-key "rewrite")
 "A" '(gptel-add :which-key "add context")
 
 ;; agent-shell
 "o" '(agent-shell :which-key "agent shell")
 "t" '(agent-shell-toggle :which-key "toggle agent")
 "n" '(agent-shell-new :which-key "new session")
 "c" '(agent-shell-anthropic-claude :which-key "claude agent"))

;;; Code/LSP Bindings (SPC c)
;; Language server protocol and code actions
(+general-global-leader
  "c" '(:ignore t :which-key "code")
  "ca" '(eglot-code-actions :which-key "code actions")
  "cF" '(eglot-format :which-key "format")
  "cc" '(consult-flymake :which-key "consult flymake")
  "cf" '(apheleia-format-buffer :which-key "format buffer")
  "cr" '(eglot-rename :which-key "rename")
  "ci" '(eglot-find-implementation :which-key "implementation")
  "ct" '(eglot-find-typeDefinition :which-key "type definition")
  "cd" '(eldoc-doc-buffer :which-key "show documentation")
  "cD" '(flymake-show-buffer-diagnostics :which-key "diagnostics")
  "cx" '(eglot-code-action-quickfix :which-key "quickfix")
  "ch" '(eglot-inlay-hints-mode :which-key "toggle hints"))

;;; Evaluation Bindings (SPC e)
;; Evaluate Lisp code
(+general-global-leader
  "e" '(:ignore t :which-key "eval")
  "eb" '(eval-buffer :which-key "eval buffer")
  "ed" '(eval-defun :which-key "eval defun")
  "ee" '(eval-last-sexp :which-key "eval last sexp")
  "er" '(eval-region :which-key "eval region")
  "ep" '(pp-eval-last-sexp :which-key "eval & pretty print"))

;;; Jump/Navigation Bindings (SPC j)
;; Navigate code and buffers
(+general-global-leader
  "j" '(:ignore t :which-key "jump")
  "jj" '(avy-goto-char-timer :which-key "avy char")
  "jw" '(avy-goto-word-1 :which-key "avy word")
  "jL" '(avy-goto-line :which-key "avy line")
  "jd" '(xref-find-definitions :which-key "definition")
  "jr" '(xref-find-references :which-key "references")
  "jb" '(xref-go-back :which-key "back")
  "jf" '(xref-go-forward :which-key "forward")
  "ji" '(consult-imenu :which-key "imenu")
  "jm" '(consult-mark :which-key "mark")
  "jl" '(consult-line :which-key "line"))

;;; Buffer Bindings (SPC b)
;; Buffer management operations
(+general-global-buffer
  "" '(:ignore t :which-key "buffer")
  "b" '(switch-to-buffer :which-key "switch buffer")
  "d" '(kill-current-buffer :which-key "kill buffer")
  "k" '(kill-buffer :which-key "kill buffer (select)")
  "n" '(next-buffer :which-key "next buffer")
  "p" '(previous-buffer :which-key "previous buffer")
  "r" '(revert-buffer :which-key "revert buffer")
  "s" '(save-buffer :which-key "save buffer")
  "w" '(read-only-mode :which-key "toggle read-only"))

;;; File Bindings (SPC f)
;; File operations
(+general-global-file
  "" '(:ignore t :which-key "file")
  "f" '(find-file :which-key "find file")
  "r" '(consult-recent-file :which-key "recent files")
  "s" '(save-buffer :which-key "save file")
  "S" '(write-file :which-key "save as")
  "D" '(delete-file :which-key "delete file")
  "R" '(rename-file :which-key "rename file"))

;;; Git Bindings (SPC g)
;; Version control with Magit
(+general-global-git
  "" '(:ignore t :which-key "git")
  "g" '(magit-status :which-key "magit status")
  "d" '(magit-dispatch :which-key "magit dispatch")
  "f" '(magit-file-dispatch :which-key "magit file")
  "b" '(magit-blame :which-key "magit blame")
  "l" '(magit-log-buffer-file :which-key "magit log file"))

;;; Help Bindings (SPC h)
;; Documentation and help
(+general-global-help
  "" '(:ignore t :which-key "help")
  "f" '(describe-function :which-key "describe function")
  "v" '(describe-variable :which-key "describe variable")
  "k" '(describe-key :which-key "describe key")
  "m" '(describe-mode :which-key "describe mode")
  "p" '(describe-package :which-key "describe package")
  "b" '(describe-bindings :which-key "describe bindings")
  "i" '(info :which-key "info")
  "?" '(help-for-help :which-key "help for help"))

;;; Application/Open Bindings (SPC o)
;; Launch applications and tools
(+general-global-application
  "" '(:ignore t :which-key "open/app")
  "a" '(agent-shell-toggle :which-key "agent shell")
  "d" '(dired-jump :which-key "dired")
  "e" '(eshell :which-key "eshell")
  "t" '(eat :which-key "eat")
  "T" '(vterm :which-key "vterm")
  "V" '(vterm-other-window :which-key "vterm other window")
  "m" '(+mini-eshell-safe :which-key "mini eshell"))

;;; Project Bindings (SPC p)
;; Project management
(+general-global-project
  "" '(:ignore t :which-key "project")
  "p" '(project-switch-project :which-key "switch project")
  "f" '(project-find-file :which-key "find file")
  "b" '(project-switch-to-buffer :which-key "switch buffer")
  "d" '(project-dired :which-key "dired")
  "k" '(project-kill-buffers :which-key "kill buffers")
  "c" '(project-compile :which-key "compile")
  "s" '(project-shell :which-key "shell")
  "e" '(project-eshell :which-key "eshell"))

;;; Search Bindings (SPC s)
;; Search and navigation
(+general-global-search
  "" '(:ignore t :which-key "search")
  "b" '(consult-line-multi :which-key "search buffers")
  "f" '(consult-fd :which-key "find file")
  "g" '(consult-grep :which-key "grep")
  "i" '(consult-imenu :which-key "imenu")
  "o" '(consult-outline :which-key "outline")
  "r" '(consult-ripgrep :which-key "ripgrep")
  "s" '(consult-line :which-key "search line"))

;;; Toggle Bindings (SPC t)
;; Toggle various modes and features
(+general-global-toggle
  "" '(:ignore t :which-key "toggle")
  "l" '(display-line-numbers-mode :which-key "line numbers")
  "w" '(whitespace-mode :which-key "whitespace")
  "f" '(auto-fill-mode :which-key "auto-fill")
  "v" '(visual-line-mode :which-key "visual line")
  "s" '(flyspell-mode :which-key "flyspell")
  "t" '(modus-themes-toggle :which-key "theme"))

;;; Window Bindings (SPC w)
;; Window management
(+general-global-window
  "" '(:ignore t :which-key "window")
  "d" '(delete-window :which-key "delete window")
  "D" '(delete-other-windows :which-key "delete other windows")
  "h" '(evil-window-left :which-key "focus left")
  "j" '(evil-window-down :which-key "focus down")
  "k" '(evil-window-up :which-key "focus up")
  "l" '(evil-window-right :which-key "focus right")
  "s" '(split-window-below :which-key "split below")
  "v" '(split-window-right :which-key "split right")
  "w" '(ace-window :which-key "ace window")
  "=" '(balance-windows :which-key "balance windows")
  "m" '(+maximize-window-safe :which-key "maximize")
  "u" '(winner-undo :which-key "undo window change")
  "r" '(winner-redo :which-key "redo window change"))

;;; Quit/Close Bindings (SPC q)
;; Exit and restart operations
(+general-global-leader
  "q" '(:ignore t :which-key "quit")
  "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
  "qQ" '(kill-emacs :which-key "quit without saving")
  "qr" '(restart-emacs :which-key "restart emacs")
  "qf" '(delete-frame :which-key "delete frame"))

;;; Register Bindings (SPC r)
;; Register operations for storing positions and text
(+general-global-leader
  "r" '(:ignore t :which-key "register")
  "rs" '(point-to-register :which-key "save point")
  "rj" '(jump-to-register :which-key "jump to")
  "rc" '(copy-to-register :which-key "copy to")
  "ri" '(insert-register :which-key "insert")
  "rr" '(consult-register :which-key "consult registers"))

;;; Bookmark Operations (SPC B)
;; Bookmark management
(+general-global-leader
  "B" '(:ignore t :which-key "bookmark")
  "Bs" '(bookmark-set :which-key "set")
  "Bj" '(bookmark-jump :which-key "jump")
  "Bd" '(bookmark-delete :which-key "delete")
  "Bl" '(bookmark-bmenu-list :which-key "list"))

;;; Insert Operations (SPC i)
;; Insert special characters and text
(+general-global-leader
  "i" '(:ignore t :which-key "insert")
  "iu" '(insert-char :which-key "unicode char")
  "iy" '(consult-yank-from-kill-ring :which-key "from kill ring"))

;;; Macro Operations (SPC M)
;; Keyboard macro management
(+general-global-leader
  "M" '(:ignore t :which-key "macro")
  "Ms" '(kmacro-start-macro :which-key "start")
  "Me" '(kmacro-end-macro :which-key "end")
  "Mr" '(kmacro-end-and-call-macro :which-key "run")
  "Mn" '(kmacro-name-last-macro :which-key "name"))

;;; Visual Undo (SPC U)
;; Visual undo tree navigation
(+general-global-leader
  "U" '(vundo :which-key "visual undo"))

;;; Notes & Knowledge Management (SPC n)
;; Org-roam note-taking and Zettelkasten system
;; Reference: https://www.orgroam.com/
(+general-global-notes
	"" '(:ignore t :which-key "notes")
	"f" '(org-roam-node-find :which-key "find node")
	"i" '(org-roam-node-insert :which-key "insert node")
	"c" '(org-roam-capture :which-key "capture")
	"b" '(org-roam-buffer-toggle :which-key "toggle backlinks")
	"g" '(org-roam-graph :which-key "show graph")
	"u" '(org-roam-ui-mode :which-key "toggle UI")
	"s" '(org-roam-db-sync :which-key "sync database"))

;; Org-roam dailies bindings (SPC n d)
(+general-global-notes
	"d" '(:ignore t :which-key "dailies")
	"dt" '(org-roam-dailies-goto-today :which-key "today")
	"dy" '(org-roam-dailies-goto-yesterday :which-key "yesterday")
	"dT" '(org-roam-dailies-goto-tomorrow :which-key "tomorrow")
	"dd" '(org-roam-dailies-goto-date :which-key "goto date")
	"dc" '(org-roam-dailies-capture-today :which-key "capture today"))

;;; Puni Structural Editing Bindings (,)
;; Structural editing operations for balanced expressions
;; Reference: https://github.com/AmaiKinono/puni
(+general-local-leader
  :keymaps 'puni-mode-map

  ;; Slurping and barfing
  ")" '(puni-slurp-forward :which-key "slurp forward")
  "}" '(puni-barf-forward :which-key "barf forward")
  "(" '(puni-slurp-backward :which-key "slurp backward")
  "{" '(puni-barf-backward :which-key "barf backward")

  ;; Wrapping / splicing
  "w" '(puni-wrap-round :which-key "wrap ()")
  "W" '(puni-splice :which-key "unwrap/splice")
  "[" '(puni-wrap-square :which-key "wrap []")
  "'" '(puni-wrap-curly :which-key "wrap {}")
  "s" '(puni-splice :which-key "splice")
  "k" '(puni-splice-killing-backward :which-key "splice kill backward")
  "j" '(puni-splice-killing-forward :which-key "splice kill forward")

  ;; Navigation
  "n" '(forward-list :which-key "next list")
  "p" '(backward-list :which-key "previous list")
  "f" '(puni-forward-sexp-or-up-list :which-key "forward sexp")
  "b" '(puni-backward-sexp-or-up-list :which-key "backward sexp")
  "d" '(down-list :which-key "down list")
  "u" '(backward-up-list :which-key "up list")
  "U" '(up-list :which-key "forward up list")

  ;; Manipulation
  "t" '(puni-transpose :which-key "transpose")
  "c" '(puni-convolute :which-key "convolute")
  "r" '(puni-raise :which-key "raise")

  ;; Selection / killing / copying
  "m" '(mark-sexp :which-key "mark sexp")
  "x" '(kill-sexp :which-key "kill sexp")
  "X" '(backward-kill-sexp :which-key "kill sexp backward")
  "y" '(+copy-sexp :which-key "copy sexp"))

;;; Kotlin Keybindings (,)
;; Local leader bindings for Kotlin development
;; Reference: https://gitlab.com/bricka/emacs-kotlin-ts-mode
(+general-local-leader
  :keymaps 'kotlin-ts-mode-map

  ;; Compile / Build (,c)
  "c"  '(:ignore t :which-key "compile/build")
  "cb" '((lambda () (interactive) (compile "./gradlew build")) :which-key "build")
  "ct" '((lambda () (interactive) (compile "./gradlew test"))  :which-key "test")
  "cr" '((lambda () (interactive) (compile "./gradlew run"))   :which-key "run")
  "cc" '((lambda () (interactive) (compile "./gradlew clean")) :which-key "clean")

  ;; Gradle tasks (,g)
  "g"  '(:ignore t :which-key "gradle")
  "gt" '(gradle-execute :which-key "execute task")
  "gl" '(gradle-tasks   :which-key "list tasks")

  ;; Reformat (,r)
  "r"  '(:ignore t :which-key "reformat")
  "rf" '(apheleia-format-buffer :which-key "format buffer")

  ;; Errors / lint (,e)
  "e"  '(:ignore t :which-key "errors/lint")
  "en" '(flymake-goto-next-error          :which-key "next error")
  "ep" '(flymake-goto-prev-error          :which-key "prev error")
  "el" '(flymake-show-buffer-diagnostics  :which-key "list errors"))

(provide 'keybindings)
;;; keybindings.el ends here
