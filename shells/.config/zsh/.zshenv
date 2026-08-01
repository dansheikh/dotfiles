# .zshenv - Loaded for every Zsh instance (interactive, non-interactive, scripts)
# Location: ~/.zshenv (symlink to ~/.config/zsh/.zshenv)

# ============================================================================
# Array Uniqueness & Typeset Setup
# ============================================================================
# Bind $PATH and $path, keeping entries unique across subshells and scripts
typeset -U path PATH

# Set array lookup options for faster parameter expansions
emulate zsh -o EXTENDED_GLOB

# ============================================================================
# XDG Base Directory Specification
# ============================================================================
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"

# ============================================================================
# Core Zsh & System Variables
# ============================================================================
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
export EDITOR="nvim"
export VISUAL="nvim"

# History file path (HISTSIZE and SAVEHIST belong in interactive configs)
export HISTFILE="${XDG_DATA_HOME}/zsh/zsh_history"
[[ -d ${HISTFILE:h} ]] || mkdir -p "${HISTFILE:h}"

# Tool-specific XDG flags / configs
export ASDF_DATA_DIR="${XDG_DATA_HOME}/asdf"
export ASDF_CONFIG_FILE="${XDG_CONFIG_HOME}/asdf/asdfrc"
export ASDF_DEFAULT_TOOL_VERSIONS_FILENAME="${ASDF_CONFIG_FILE:h}/tool-versions"
export STARSHIP_CONFIG="${XDG_CONFIG_HOME}/starship/starship.toml"
export NPM_CONFIG_PREFIX="${HOME}/.npm-packages"
export GOPATH="${HOME}/go"
export GOBIN="${GOPATH}/bin"
export BUN_INSTALL="${HOME}/.bun"
export BAT_PAGER="less -KFR --mouse"

# ============================================================================
# Construct PATH Array
# ============================================================================
typeset -a target_paths=(
  "${ASDF_DATA_DIR}/shims"
  "${NPM_CONFIG_PREFIX}/bin"
  "${HOME}/.local/bin"
  "${HOME}/.cargo/bin"
  "${GOBIN}"
  "${BUN_INSTALL}/bin"
  "${HOME}/.deno/bin"
)

# Collect existing directories preserving priority order
typeset -a valid_paths=()
for dir in "${target_paths[@]}"; do
  [[ -d "${dir}" ]] && valid_paths+=("${dir}")
done

# Prepend all valid paths to $path in batch
path=($valid_paths $path)

export PATH
