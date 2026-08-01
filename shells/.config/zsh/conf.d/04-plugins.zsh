# 04-plugins.zsh - Plugin management via Antidote
#
# After this file runs, fpath will contain plugin completion directories.
# compinit is therefore deferred to 05-compinit.zsh.

# Queue compdef calls if compinit hasn't executed yet
if ! typeset -f compdef >/dev/null; then
  typeset -ga _deferred_compdefs
  compdef() {
    _deferred_compdefs+=("$*")
  }
fi

# ============================================================================
# zsh-abbr — point at our declarative source file before the plugin loads.
# zsh-abbr uses this as its live store; no import step required.
# ============================================================================
export ABBR_USER_ABBREVIATIONS_FILE="${ZDOTDIR:-~/.config/zsh}/abbreviations.zsh"

# ============================================================================
# Antidote
# ============================================================================
_antidote_path="${ZDOTDIR:-~/.config/zsh}/antidote"
_zsh_plugins="${ZDOTDIR:-~/.config/zsh}/.zsh_plugins"

if [[ -f ${_antidote_path}/antidote.zsh ]]; then
  source "${_antidote_path}/antidote.zsh"
  fpath=("${_antidote_path}/functions" ${fpath})
  autoload -Uz antidote

  [[ -f ${_zsh_plugins}.txt ]] || touch ${_zsh_plugins}.txt

  # Regenerate static bundle only when the plugin list changes
  if [[ ! ${_zsh_plugins}.zsh -nt ${_zsh_plugins}.txt ]]; then
    antidote bundle < ${_zsh_plugins}.txt >| ${_zsh_plugins}.zsh
  fi

  source ${_zsh_plugins}.zsh

  # Reassert HISTFILE after plugins — OMZ lib unconditionally sets it to
  # ~/.zsh_history, overwriting the XDG location set in .zshenv.
  export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/zsh_history"
  [[ -d ${HISTFILE:h} ]] || mkdir -p ${HISTFILE:h}
else
  print -P "%F{yellow}warn:%f antidote not found — plugins not loaded"
fi

unset _antidote_path _zsh_plugins

# ============================================================================
# zsh-autosuggestions
# ============================================================================
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
export ZSH_AUTOSUGGEST_MANUAL_REBIND=1
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#6c7086"

# Ctrl-Space to accept suggestion; Tab drives fzf-tab completion
bindkey '^ ' autosuggest-accept

# ============================================================================
# zsh-syntax-highlighting
# ============================================================================
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red,bold'
ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[alias]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[builtin]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[function]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[command]='fg=green'
ZSH_HIGHLIGHT_STYLES[precommand]='fg=green,underline'
ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=green'
ZSH_HIGHLIGHT_STYLES[path]='fg=blue,underline'
ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=cyan,underline'
ZSH_HIGHLIGHT_STYLES[globbing]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=blue'
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=blue'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[redirection]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[comment]='fg=gray'
