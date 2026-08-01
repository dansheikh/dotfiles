;;; danish-mode-line-icons.el --- Icon mappings for danish-mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>
;; Version: 1.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mode-line, icons
;; URL: https://github.com/dansheikh/dotfiles

;;; Commentary:

;; Icon mappings for danish-mode-line major modes using Nerd Font icons.
;; This file provides visual indicators for different file types and modes
;; in the mode line.
;;
;; Requirements:
;; - A Nerd Font installed and configured in Emacs
;;   (e.g., JetBrainsMono Nerd Font, FiraCode Nerd Font)
;;
;; Customization:
;;
;; To add custom icons:
;;   (with-eval-after-load 'danish-mode-line-icons
;;     (add-to-list 'danish-mode-line--mode-icons
;;                  '(my-mode . "󰻞")))
;;
;; To change existing icons:
;;   (with-eval-after-load 'danish-mode-line-icons
;;     (setf (alist-get 'python-mode danish-mode-line--mode-icons) ""))
;;
;; To change the default icon:
;;   (setq danish-mode-line--default-mode-icon "")
;;
;; Icon Reference:
;; - Nerd Fonts Cheat Sheet: https://www.nerdfonts.com/cheat-sheet
;; - Common prefixes: 󰌠 (nf-md-*), 󰊢 (nf-md-git), 󰅩 (nf-md-code)

;;; Code:

(defvar danish-mode-line--mode-icons
  '(
    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Programming Languages
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; Python
    (python-mode . "󰌠")
    (python-ts-mode . "󰌠")
    (inferior-python-mode . "󰌠")

    ;; JavaScript
    (javascript-mode . "󰌞")
    (js-mode . "󰌞")
    (js2-mode . "󰌞")
    (js-ts-mode . "󰌞")

    ;; TypeScript
    (typescript-mode . "󰛦")
    (typescript-ts-mode . "󰛦")
    (tsx-ts-mode . "󰛦")

    ;; Ruby
    (ruby-mode . "")
    (ruby-ts-mode . "")
    (inf-ruby-mode . "")

    ;; Java
    (java-mode . "")
    (java-ts-mode . "")

    ;; C
    (c-mode . "")
    (c-ts-mode . "")

    ;; C++
    (c++-mode . "")
    (c++-ts-mode . "")

    ;; C#
    (csharp-mode . "󰌛")
    (csharp-ts-mode . "󰌛")

    ;; Go
    (go-mode . "󰟓")
    (go-ts-mode . "󰟓")

    ;; Rust
    (rust-mode . "")
    (rust-ts-mode . "")
    (rustic-mode . "")

    ;; PHP
    (php-mode . "")
    (php-ts-mode . "")

    ;; Swift
    (swift-mode . "󰛥")
    (swift-ts-mode . "󰛥")

    ;; Kotlin
    (kotlin-mode . "")
    (kotlin-ts-mode . "")

    ;; Scala
    (scala-mode . "")
    (scala-ts-mode . "")

    ;; Elixir
    (elixir-mode . "")
    (elixir-ts-mode . "")
    (heex-ts-mode . "")

    ;; Erlang
    (erlang-mode . "")

    ;; Haskell
    (haskell-mode . "")
    (haskell-cabal-mode . "")

    ;; OCaml
    (ocaml-mode . "")
    (tuareg-mode . "")

    ;; Clojure
    (clojure-mode . "")
    (clojurescript-mode . "")
    (clojurec-mode . "")
    (cider-repl-mode . "")

    ;; Racket/Scheme
    (racket-mode . "")
    (scheme-mode . "")
    (geiser-repl-mode . "")

    ;; Common Lisp
    (lisp-mode . "")
    (slime-repl-mode . "")
    (sly-mrepl-mode . "")

    ;; Emacs Lisp
    (emacs-lisp-mode . "")
    (lisp-interaction-mode . "")
    (ielm-mode . "")

    ;; Lua
    (lua-mode . "")
    (lua-ts-mode . "")

    ;; Perl
    (perl-mode . "")
    (cperl-mode . "")

    ;; R
    (ess-r-mode . "󰟔")
    (r-mode . "󰟔")

    ;; Julia
    (julia-mode . "")
    (julia-ts-mode . "")

    ;; Zig
    (zig-mode . "")
    (zig-ts-mode . "")

    ;; Nim
    (nim-mode . "")

    ;; Crystal
    (crystal-mode . "")

    ;; D
    (d-mode . "")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Web Development
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; HTML
    (html-mode . "")
    (mhtml-mode . "")
    (html-ts-mode . "")
    (web-mode . "")
    (nxhtml-mode . "")

    ;; CSS
    (css-mode . "")
    (css-ts-mode . "")

    ;; SCSS/Sass
    (scss-mode . "")
    (sass-mode . "")

    ;; Less
    (less-css-mode . "")

    ;; Vue
    (vue-mode . "󰡄")
    (vue-ts-mode . "󰡄")

    ;; Svelte
    (svelte-mode . "")

    ;; Astro
    (astro-mode . "")
    (astro-ts-mode . "")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Shell & Terminal
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; Shell scripts
    (sh-mode . "")
    (bash-ts-mode . "")
    (shell-script-mode . "")

    ;; Fish
    (fish-mode . "")

    ;; PowerShell
    (powershell-mode . "󰨊")
    (powershell-ts-mode . "󰨊")

    ;; Terminal emulators
    (term-mode . "")
    (vterm-mode . "")
    (eat-mode . "")
    (ansi-term-mode . "")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Configuration & Data Formats
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; JSON
    (json-mode . "")
    (json-ts-mode . "")
    (jsonc-mode . "")

    ;; YAML
    (yaml-mode . "")
    (yaml-ts-mode . "")

    ;; TOML
    (toml-mode . "")
    (toml-ts-mode . "")

    ;; XML
    (xml-mode . "󰗀")
    (nxml-mode . "󰗀")

    ;; Config files
    (conf-mode . "")
    (conf-unix-mode . "")
    (conf-toml-mode . "")
    (conf-space-mode . "")
    (conf-colon-mode . "")
    (conf-windows-mode . "")

    ;; INI
    (ini-mode . "")

    ;; Properties
    (conf-javaprop-mode . "")

    ;; Dotenv
    (dotenv-mode . "")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Markup & Documentation
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; Markdown
    (markdown-mode . "")
    (gfm-mode . "")
    (markdown-ts-mode . "")

    ;; Org
    (org-mode . "")
    (org-agenda-mode . "")

    ;; reStructuredText
    (rst-mode . "")

    ;; LaTeX
    (tex-mode . "")
    (latex-mode . "")
    (LaTeX-mode . "")
    (plain-tex-mode . "")

    ;; BibTeX
    (bibtex-mode . "󱉟")

    ;; AsciiDoc
    (adoc-mode . "")
    (asciidoc-mode . "")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Database
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; SQL
    (sql-mode . "")
    (sql-interactive-mode . "")
    (sql-ts-mode . "")

    ;; GraphQL
    (graphql-mode . "󰡷")
    (graphql-ts-mode . "󰡷")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Version Control
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; Diff
    (diff-mode . "")

    ;; Magit
    (magit-status-mode . "")
    (magit-log-mode . "")
    (magit-diff-mode . "")
    (magit-revision-mode . "")
    (magit-stash-mode . "")
    (magit-process-mode . "")
    (magit-refs-mode . "")

    ;; Git commit/rebase
    (git-commit-mode . "")
    (git-rebase-mode . "")
    (gitignore-mode . "")
    (gitattributes-mode . "")
    (gitconfig-mode . "")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; DevOps & Infrastructure
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; Docker
    (dockerfile-mode . "󰡨")
    (dockerfile-ts-mode . "󰡨")
    (docker-compose-mode . "󰡨")

    ;; Terraform
    (terraform-mode . "󱁢")
    (hcl-mode . "󱁢")

    ;; Ansible
    (ansible-mode . "")
    (ansible-doc-mode . "")

    ;; Kubernetes
    (kubernetes-mode . "󱃾")
    (k8s-mode . "󱃾")

    ;; Nginx
    (nginx-mode . "")

    ;; Nix
    (nix-mode . "")
    (nix-ts-mode . "")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Build Systems & Package Managers
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; Make
    (makefile-mode . "")
    (makefile-gmake-mode . "")
    (makefile-bsdmake-mode . "")
    (makefile-automake-mode . "")

    ;; CMake
    (cmake-mode . "")
    (cmake-ts-mode . "")

    ;; Meson
    (meson-mode . "")

    ;; Gradle
    (gradle-mode . "")
    (groovy-mode . "")

    ;; Bazel
    (bazel-mode . "")
    (bazel-build-mode . "")
    (bazel-workspace-mode . "")
    (starlark-mode . "")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Data Science & Notebooks
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; Jupyter
    (jupyter-repl-mode . "")
    (ein:notebook-multilang-mode . "")

    ;; Pytest
    (python-pytest-mode . "󰙨")

    ;; ═══════════════════════════════════════════════════════════════════════════
    ;; Special Modes & Emacs Built-ins
    ;; ═══════════════════════════════════════════════════════════════════════════

    ;; Dired
    (dired-mode . "")
    (dired-sidebar-mode . "")
    (dirvish-mode . "")

    ;; Buffer lists
    (ibuffer-mode . "")
    (Buffer-menu-mode . "")

    ;; Help & Info
    (help-mode . "󰋖")
    (helpful-mode . "󰋖")
    (info-mode . "")
    (Info-mode . "")

    ;; Man pages
    (man-mode . "")
    (woman-mode . "")

    ;; Compilation
    (compilation-mode . "")
    (comint-mode . "")
    (grep-mode . "")

    ;; Messages
    (messages-buffer-mode . "")

    ;; Debugger
    (debugger-mode . "")
    (edebug-mode . "")
    (gdb-mode . "")
    (gud-mode . "")

    ;; Profiler
    (profiler-report-mode . "")

    ;; Customize
    (Custom-mode . "")
    (customize-mode . "")

    ;; Calendar
    (calendar-mode . "")
    (diary-mode . "")

    ;; Email
    (message-mode . "󰇮")
    (mu4e-main-mode . "󰇮")
    (mu4e-headers-mode . "󰇮")
    (mu4e-view-mode . "󰇮")
    (gnus-summary-mode . "󰇮")
    (gnus-article-mode . "󰇮")
    (notmuch-search-mode . "󰇮")
    (notmuch-show-mode . "󰇮")

    ;; RSS
    (elfeed-search-mode . "")
    (elfeed-show-mode . "")
    (newsticker-mode . "")

    ;; Chat/IRC
    (erc-mode . "󰻞")
    (rcirc-mode . "󰻞")
    (circe-mode . "󰻞")

    ;; PDF
    (pdf-view-mode . "")
    (doc-view-mode . "")

    ;; Image
    (image-mode . "")
    (image-dired-thumbnail-mode . "")

    ;; Music/Audio
    (emms-playlist-mode . "")
    (bongo-playlist-mode . "")

    ;; Web browsing
    (eww-mode . "󰖟")
    (w3m-mode . "󰖟")

    ;; Package management
    (package-menu-mode . "")

    ;; Process lists
    (process-menu-mode . "")
    (proced-mode . "")

    ;; Treemacs/Neotree
    (treemacs-mode . "")
    (neotree-mode . "")

    ;; Treesit explorer
    (treesit-explore-mode . "")

    ;; Base modes (fallbacks)
    (fundamental-mode . "")
    (text-mode . "")
    (prog-mode . "")
    (special-mode . "")
    (tabulated-list-mode . ""))

  "Mapping of major modes to Nerd Font icons.
Each entry is (MODE . ICON) where MODE is a major-mode symbol
and ICON is a Nerd Font icon string.

The icons require a Nerd Font to be installed and configured.
See https://www.nerdfonts.com for available fonts.")

(defvar danish-mode-line--default-mode-icon "󰈔"
  "Default icon for modes not in `danish-mode-line--mode-icons'.
This is used when a mode is not found in the icon mapping
or when the mapped icon is an empty string.
Default is a generic file icon (󰈔).")

(provide 'danish-mode-line-icons)
;;; danish-mode-line-icons.el ends here
