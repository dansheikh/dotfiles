# Dotfiles

Cross-platform dotfiles managed with **GNU Stow** and automated via **GNU Make**. Designed with strict adherence to the **XDG Base Directory Specification** for Arch Linux and macOS environments.

---

## 🏗️ Structure

Packages are modularized by application to allow selective stowing depending on the operating system.

```text
.
├── bin
│   └── .local
│       └── bin
├── clojure
│   └── .config
│       └── clojure
├── code
│   └── .config
│       └── code
├── efm-langserver
│   └── .config
│       └── efm-langserver
├── emacs
│   └── .config
│       └── emacs
├── ghostty
│   └── .config
│       └── ghostty
├── .git
├── git
│   └── .config
│       └── git
├── .gitignore
├── .gitmodules
├── hyprland
│   └── .config
│       └── hypr
├── kitty
│   └── .config
│       └── kitty
├── lazygit
│   └── .config
│       └── lazygit
├── lein
│   └── .config
│       └── lein
├── Makefile
├── micro
│   └── .config
│       └── micro
├── nix
│   ├── .config
│   │   └── nix
│   ├── darwin
│   │   ├── flake.lock
│   │   ├── flake.nix
│   │   ├── hosts
│   │   ├── lib
│   │   ├── modules
│   │   └── overlays
│   ├── lib
│   │   └── xontrib-prompt-starship.nix
│   └── os
│       ├── flake.lock
│       ├── flake.nix
│       ├── hosts
│       ├── lib
│       ├── modules
│       └── overlays
├── noctalia
│   └── .config
│       └── noctalia
├── nushell
│   └── .config
├── nvim
│   └── .config
│       └── nvim
├── oh-my-posh
│   └── .config
│       └── oh-my-posh
├── README.md
├── rio
│   └── .config
│       └── rio
├── services
│   └── emacs
│       ├── Emacsclient.app
│       └── gnu.emacs.daemon.plist
├── shells
│   ├── .config
│   │   ├── nushell
│   │   ├── xonsh
│   │   └── zsh
│   └── .zshenv
├── ssh
│   └── .ssh
│       └── config
├── starship
│   └── .config
│       └── starship
├── tmux
│   └── .config
│       └── tmux
├── waybar
│   └── .config
│       └── waybar
├── wezterm
│   └── .config
│       └── wezterm
├── wofi
│   └── .config
│       └── wofi
├── xonsh
│   └── .config
└── zed
    └── .config
        └── zed
```

## 🚀 Quick Start

### 1.
```bash
git clone -b main https://github.com/dansheikh/dotfiles.git ~/dotfiles
cd ~/dotfiles
```

### 2. Preview Changes (Dry Run)
Test what symlinks will be created without modifying your file system:
```bash
make dry-run
```

### 3. Deploy
Deploy common and OS-specific packages automatically:
```bash
make all
```
