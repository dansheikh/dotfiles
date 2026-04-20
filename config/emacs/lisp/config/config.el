;;; config.el --- Custom Emacs configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>

;;; Commentary:

;; This file contains environment-specific and custom configuration functions.
;; It is loaded from init.el via the +load-config-file function.
;;
;; Functions included:
;; - Frame styling and font configuration
;; - Shell environment synchronization
;; - Eshell utilities
;; - Node.js project path management
;; - Org-babel auto-tangling
;;
;; Reference: This file is loaded by emacs.org through the
;; +load-config-file helper defined in the "Load Path & External Configuration"
;; section.

;;; Code:

;;; =============================================================================
;;; Eshell Utilities
;;; =============================================================================

(defun cleanup-eshell-window ()
  "Delete eshell window after process exits.
This function is called as advice after `eshell-life-is-too-much'.
It ensures clean window management when closing eshell buffers.

Reference: Advised onto `eshell-life-is-too-much' at the end of this file."
  (when (not (one-window-p))
    (delete-window)))

(defun mini-eshell ()
  "Launch eshell in a mini-window at the bottom of the frame.
Creates a small horizontal split (10 lines) at the bottom for quick
shell access without disrupting the main editing workspace.

Reference: Bound to 'SPC o m' in keybindings.el"
  (interactive)
  (let ((w (split-window-below -10)))
    (select-window w)
    (eshell)))

;;; =============================================================================
;;; Org-Mode Utilities
;;; =============================================================================

(defun danish--org-babel-tangle-config ()
  "Automatically tangle emacs.org configuration file on save.
This function runs when saving the main configuration org file and
tangles all source blocks to generate the actual Emacs configuration.

The function checks if the current buffer is the emacs.org file before
tangling to avoid unnecessary processing on other org files.

Reference: https://orgmode.org/manual/Extracting-Source-Code.html
Hook: Added via 'org-mode-hook' in emacs.org for the specific config file."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/config/emacs/emacs.org"))
    ;; Disable confirmation prompt for babel evaluation during tangling
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;;; =============================================================================
;;; Frame Styling & UI Configuration
;;; =============================================================================

(defun danish--style-frame ()
  "Configure frame styling and appearance settings.
This function:
- Disables menu, scroll, and tool bars for a cleaner interface
- Sets the default font to Iosevka Nerd Font at size 16
- Maximizes the frame to use full screen space

The function handles both GUI and terminal contexts appropriately,
only disabling graphical elements when running in GUI mode.

Reference: https://www.gnu.org/software/emacs/manual/html_node/emacs/Frames.html
Called: During startup via daemon/non-daemon conditional at end of file."
  ;; Disable menu bar in all contexts
  (menu-bar-mode -1)
  
  ;; Disable graphical elements only in GUI mode
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tool-bar-mode -1))
  
  ;; Set default font - Iosevka Nerd Font includes programming ligatures
  ;; and icon support. Size 16 provides good readability on modern displays.
  ;; Reference: https://rubjo.github.io/victor-mono/
  (add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-16"))
  (set-frame-font "Iosevka Nerd Font-16" nil t)
  
  ;; Maximize frame to utilize full screen real estate
  (toggle-frame-maximized))

;;; =============================================================================
;;; Environment & Path Management
;;; =============================================================================

(defun danish--set-exec-path-by-shell ()
  "Synchronize Emacs' 'exec-path' and PATH with the user's shell environment.
This is critical on macOS and Linux where GUI applications don't inherit
the full shell environment. The function detects fish shell vs POSIX shells
and uses the appropriate command to extract PATH.

The synchronized PATH ensures:
- Language servers are discoverable (pyright, typescript-language-server, etc.)
- Development tools work correctly (npm, cargo, go, etc.)
- Formatters and linters are accessible (ruff, biome, prettier, etc.)

Reference: https://www.gnu.org/software/emacs/manual/html_node/emacs/General-Variables.html
Related: Similar to exec-path-from-shell package but with fish shell support."
  (interactive)
  (let ((shell-path
         (if (string-match-p "fish" (getenv "SHELL"))
             ;; Fish shell uses different syntax for path manipulation
             ;; Use 'string join' to convert fish list to colon-separated PATH
             (replace-regexp-in-string
              "[ \t\n]*$" ""
              (shell-command-to-string "$SHELL --login -c 'string join : $PATH'"))
           ;; POSIX-compliant shells (bash, zsh) use standard echo
           (replace-regexp-in-string
            "[ \t\n]*$" ""
            (shell-command-to-string "$SHELL --login -c 'echo $PATH'")))))
    ;; Set both PATH environment variable and exec-path list
    (setenv "PATH" shell-path)
    (setq exec-path (split-string shell-path path-separator))))

(defun danish--add-local-node-bin-to-exec-path ()
  "Add local node_modules/.bin directory to 'exec-path' for current project.
This enables Emacs to find project-local Node.js development tools without
requiring global installations. Particularly useful for:
- ESLint, Prettier, Biome (code quality tools)
- TypeScript compiler and language server
- Build tools and custom scripts

The function searches upward from the current file/directory to find the
nearest node_modules directory, adding its .bin folder to 'exec-path'.

Reference: https://docs.npmjs.com/cli/v10/configuring-npm/folders#executables
Hook: Typically added to js-ts-mode-hook or typescript-ts-mode-hook."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (bin (and root
                   (expand-file-name "node_modules/.bin" root))))
    (when (and bin (file-directory-p bin))
      ;; Use buffer-local exec-path to avoid polluting global path
      (setq-local exec-path (cons bin exec-path)))))

;;; =============================================================================
;;; Exec-Path & Path Sync
;;; =============================================================================

(when (or (daemonp) (memq window-system '(mac ns x)))
  (danish--set-exec-path-by-shell))

;;; =============================================================================
;;; Advice & Hook Configuration
;;; =============================================================================

;; Clean up eshell windows automatically after exiting
;; Reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
(advice-add 'eshell-life-is-too-much :after 'cleanup-eshell-window)

;;; =============================================================================
;;; Frame Initialization
;;; =============================================================================

;; Apply frame styling based on whether Emacs is running as a daemon or standalone.
;; 
;; When running as a daemon (emacs --daemon), frames are created dynamically as
;; clients connect. We need to style each new frame as it's created.
;; 
;; When running standalone, we style the initial frame immediately.
;;
;; Reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html

(cond
 ;; Daemon mode: style initial frame and hook for future frames
 ((daemonp)
  ;; Style the daemon's invisible frame (affects fallback behavior)
  (danish--style-frame)
  ;; Add hook to style each new client frame
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (danish--style-frame)))))
 
 ;; Standalone mode: style the initial frame immediately
 (t (danish--style-frame)))

(provide 'config)
;;; config.el ends here
