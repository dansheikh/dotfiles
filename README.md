# Dotfiles

Collection of configuration files for customizing Linux and Mac OS.

# Setup

## Requirements
1. Install the Nix package manager for [Linux](https://nixos.org/download.html#nix-install-linux) or [Mac OS](https://nixos.org/download.html#nix-install-macos).
2. Install [Flakes](https://nixos.wiki/wiki/Flakes#Non-NixOS).

## Install

1. Clone dotfiles repository; the `$HOME` directory is the recommended clone location, however, any non-administrative or non-enhanced security directory will suffice.
2. Symbolic (Symlink) or Soft Link desired dotfiles to required locations. At minimum following are recommneded:
    - zshenv
    - zshrc
    - zsh_theme
    - aliases
    - tmux.conf
    - tmux
3. [Refresh Tmux environment](https://github.com/tmux-plugins/tpm#key-bindings) with `prefix` + `I`
