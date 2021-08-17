# Dotfiles

Collection of configuration files for customizing Linux and Mac OS.

# Setup

## Requirements
Install the following prior to cloning dotfiles repository:
1. [Homebrew on Mac OS](https://brew.sh/#install) or [Homebrew on Linux](https://docs.brew.sh/Homebrew-on-Linux)
    - If using Linux "build essentials" or "development tools" must be present prior to installing Homebrew
2. zsh
3. tmux
4. fd
5. ripgrep
6. clojure
7. clojure-lsp
8. asdf
9. [sdkman](https://sdkman.io/install)

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
