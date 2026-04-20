# .zshenv - Environment variables loaded for all shells
# Location: ~/.zshenv (symlink to ~/.config/zsh/.zshenv)
#
# Loaded for every shell (interactive, non-interactive, scripts).
# Keep this file lean: only exports that must be visible everywhere.

# ============================================================================
# XDG Base Directory Specification
# ============================================================================
export XDG_CONFIG_HOME=~/.config
export XDG_DATA_HOME=~/.local/share
export XDG_STATE_HOME=~/.local/state
export XDG_CACHE_HOME=~/.cache

# ============================================================================
# Zsh configuration directory
# ============================================================================
export ZDOTDIR=${XDG_CONFIG_HOME}/zsh

# ============================================================================
# Editor
# ============================================================================
export EDITOR=nvim
export VISUAL=nvim

# ============================================================================
# Starship prompt configuration
# ============================================================================
export STARSHIP_CONFIG=${XDG_CONFIG_HOME}/starship/starship.toml

# ============================================================================
# History file location
# HISTSIZE and SAVEHIST live in conf.d/03-history.zsh (interactive only)
# ============================================================================
export HISTFILE=${XDG_DATA_HOME}/zsh/zsh_history
[[ -d ${HISTFILE:h} ]] || mkdir -p ${HISTFILE:h}

# ============================================================================
# Node.js / NPM
# ============================================================================
export NPM_CONFIG_PREFIX=~/.npm-packages
export PATH=${NPM_CONFIG_PREFIX}/bin:${PATH}

# ============================================================================
# Rust (cargo)
# ============================================================================
[[ -d ${HOME}/.cargo/bin ]] && export PATH=${HOME}/.cargo/bin:${PATH}

# ============================================================================
# Go
# ============================================================================
if command -v go &>/dev/null; then
  export GOPATH=${HOME}/go
  export GOBIN=${GOPATH}/bin
  export PATH=${GOBIN}:${PATH}
fi

# ============================================================================
# Bun
# ============================================================================
if [[ -d ${HOME}/.bun ]]; then
  export BUN_INSTALL=${HOME}/.bun
  export PATH=${BUN_INSTALL}/bin:${PATH}
fi

# ============================================================================
# Deno
# ============================================================================
[[ -d ${HOME}/.deno ]] && export PATH=${HOME}/.deno/bin:${PATH}

