# ============================================================================
# Mise (runtime manager)
# ============================================================================
if (( $+commands[mise] )); then
  eval "$(mise activate zsh)"
fi

# ============================================================================
# Starship Prompt
# ============================================================================
if (( $+commands[starship] )); then
  eval "$(starship init zsh)"
fi

# ============================================================================
# Zoxide
# ============================================================================
if (( $+commands[zoxide] )); then
  eval "$(zoxide init zsh)"
fi

# ============================================================================
# Direnv
# ============================================================================
if (( $+commands[direnv] )); then
  eval "$(direnv hook zsh)"
fi

# ============================================================================
# Tmux Auto-Session Integration
# ============================================================================
# Launch or attach to a tmux session only in interactive top-level shells
if (( $+commands[tmux] )) && [[ -z "$TMUX" && -z "$INSIDE_EMACS" && -z "$VIM" && -z "$NVIM" && -o interactive ]]; then
  # Attach to existing default session "main" or create a new one
  exec tmux new-session -A -s main
fi
