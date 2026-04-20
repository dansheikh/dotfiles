# 10-integrations.zsh - External tool integrations
# (was 09-integrations.zsh; renumbered)
#
# Rust, Go, Bun, Deno PATH setup lives in .zshenv
# (available to non-interactive shells and editors).
# This file handles interactive-shell integrations only.

# ============================================================================
# Tmux auto-attach
# ============================================================================
if command -v tmux &>/dev/null && [[ -z $TMUX && -z $INSIDE_EMACS ]]; then
  if tmux has-session 2>/dev/null; then
    exec tmux attach
  else
    exec tmux new-session
  fi
fi

# ============================================================================
# mise — polyglot runtime manager
# Must run before direnv so managed runtimes are on PATH when .envrc evaluates
# ============================================================================
if command -v mise &>/dev/null; then
  eval "$(mise activate zsh)"
# Fall back to asdf only if mise is not present
elif [[ -f ${HOME}/.asdf/asdf.sh ]]; then
  source "${HOME}/.asdf/asdf.sh"
  fpath=(${ASDF_DIR}/completions $fpath)
fi

# ============================================================================
# direnv — per-directory environment variables
# Runs after mise so runtime binaries are available inside .envrc
# ============================================================================
if command -v direnv &>/dev/null; then
  eval "$(direnv hook zsh)"
fi

# ============================================================================
# Starship prompt
# ============================================================================
if command -v starship &>/dev/null; then
  eval "$(starship init zsh)"
fi

# ============================================================================
# zoxide — smarter cd
# ============================================================================
if command -v zoxide &>/dev/null; then
  eval "$(zoxide init zsh)"
  # Use `z` for zoxide; keep native `cd` available unchanged
fi

# ============================================================================
# Atuin — enhanced shell history
# ============================================================================
if command -v atuin &>/dev/null; then
  eval "$(atuin init zsh)"
fi

# ============================================================================
# pyenv — Python version management
# ============================================================================
if command -v pyenv &>/dev/null; then
  export PYENV_ROOT="${HOME}/.pyenv"
  path-add "${PYENV_ROOT}/bin"
  eval "$(pyenv init -)"
  command -v pyenv-virtualenv-init &>/dev/null && eval "$(pyenv virtualenv-init -)"
fi

# ============================================================================
# rbenv — Ruby version management
# ============================================================================
if command -v rbenv &>/dev/null; then
  eval "$(rbenv init - zsh)"
fi

# ============================================================================
# fnm — Fast Node Manager
# ============================================================================
if command -v fnm &>/dev/null; then
  eval "$(fnm env --use-on-cd)"
fi

# ============================================================================
# AWS CLI completion
# ============================================================================
if command -v aws &>/dev/null && [[ -f /usr/local/bin/aws_zsh_completer.sh ]]; then
  source /usr/local/bin/aws_zsh_completer.sh
fi

# ============================================================================
# Google Cloud SDK
# ============================================================================
if [[ -d ${HOME}/google-cloud-sdk ]]; then
  source "${HOME}/google-cloud-sdk/path.zsh.inc"
  source "${HOME}/google-cloud-sdk/completion.zsh.inc"
fi

# ============================================================================
# kubectl completion
# ============================================================================
if command -v kubectl &>/dev/null; then
  source <(kubectl completion zsh)
  complete -F __start_kubectl k
fi

# ============================================================================
# Terraform completion
# ============================================================================
if command -v terraform &>/dev/null; then
  autoload -U +X bashcompinit && bashcompinit
  complete -o nospace -C /usr/bin/terraform terraform
fi

# ============================================================================
# GitHub CLI completion
# ============================================================================
if command -v gh &>/dev/null; then
  eval "$(gh completion -s zsh)"
fi

# ============================================================================
# Nix
# ============================================================================
[[ -e ${HOME}/.nix-profile/etc/profile.d/nix.sh ]] \
  && source "${HOME}/.nix-profile/etc/profile.d/nix.sh"

# ============================================================================
# nerdfetch — system info on startup (outside tmux only)
# ============================================================================
if command -v nerdfetch &>/dev/null && [[ -z $TMUX && -z $NERDFETCH_SHOWN ]]; then
  nerdfetch
  export NERDFETCH_SHOWN=1
fi

# ============================================================================
# Utility functions
# ============================================================================

# check-tools — report which integrated tools are installed
check-tools() {
  local tools=(
    "tmux:Tmux"
    "starship:Starship"
    "direnv:Direnv"
    "mise:Mise"
    "zoxide:Zoxide"
    "atuin:Atuin"
    "kubectl:Kubernetes"
    "docker:Docker"
    "go:Go"
    "pyenv:Pyenv"
    "rbenv:Rbenv"
    "fnm:Fast Node Manager"
    "terraform:Terraform"
    "gh:GitHub CLI"
    "nerdfetch:Nerdfetch"
  )
  print "Integrated tools:"
  for entry in "${tools[@]}"; do
    local cmd="${entry%%:*}" name="${entry##*:}"
    if command -v "$cmd" &>/dev/null; then
      print -P "  %F{green}✓%f $name"
    else
      print -P "  %F{red}✗%f $name"
    fi
  done
}
