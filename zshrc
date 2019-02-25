HISTFILE=$HOME/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

if [[ $TERM =~ (eterm*|rxvt*|screen*|xterm*) ]]; then
  autoload -Uz compinit && compinit
  autoload -Uz colors && colors
  autoload -Uz vcs_info

  setopt AUTO_CD
  setopt AUTO_REMOVE_SLASH
  setopt BANG_HIST
  setopt EXTENDED_GLOB
  setopt HIST_EXPIRE_DUPS_FIRST
  setopt HIST_FIND_NO_DUPS
  setopt HIST_IGNORE_DUPS
  setopt HIST_IGNORE_ALL_DUPS
  setopt HIST_IGNORE_SPACE
  setopt HIST_REDUCE_BLANKS
  setopt HIST_SAVE_NO_DUPS
  setopt HIST_VERIFY
  setopt INC_APPEND_HISTORY
  setopt INTERACTIVE_COMMENTS
  setopt NO_BEEP
  setopt SHARE_HISTORY

  if [ -f $HOME/.zsh_theme ]; then
     . $HOME/.zsh_theme
  fi

  # Bindings.
  bindkey -v
  bindkey -M viins "^R" history-incremental-pattern-search-backward
  bindkey -M vicmd "/" history-incremental-pattern-search-backward
  bindkey -M viins "^F" history-incremental-pattern-search-forward
  bindkey -M vicmd "?" history-incremental-pattern-search-forward
  bindkey "^[[1~" beginning-of-line
  bindkey "^[[3~" delete-char
  bindkey "^[[4~" end-of-line
  bindkey "^[[5~" up-line-or-history
  bindkey "^[[6~" down-line-or-history
  bindkey "^[[7~" beginning-of-line
  bindkey "^[[8~" end-of-line
  bindkey "^[[H" beginning-of-line
  bindkey "^[[F" end-of-line
  bindkey "${terminfo[khome]}" beginning-of-line
  bindkey "${terminfo[kend]}" end-of-line

  # Stylings.
  zstyle ':completion:*' format %d
  zstyle ':completion:*' group-name ''
  zstyle ':completion:*' menu select=1
  zstyle ':completion:*' completer _expand _complete _correct
  zstyle ':completion:*' verbose yes
  zstyle ':completion:*' squeeze-slashes true
  zstyle ':completion:*' completer _expand _complete _approximate _correct
  zstyle ':completion:*:cd:*' ignore-parents parent pwd
  zstyle ':completion::complete:*' use-cache on
  zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'
fi

if [ -f $HOME/.aliases ]; then
  . $HOME/.aliases
fi

# Activate NVM.
[ -n $NVM_HOME ] && [ -d $NVM_HOME ] && [ -s $NVM_HOME/nvm.sh ] && . $NVM_HOME/nvm.sh

# Configure OPAM.
if command -v opam &> /dev/null; then
   eval $(opam config env)
fi

# Launch Tmux.
if command -v tmux &> /dev/null; then
  [[ -n $TERM ]] && [[ ! $TERM =~ dumb ]] && [[ ! $TERM =~ screen ]] && [[ -z $INSIDE_EMACS ]] && [[ -z $TMUX ]] && exec tmux new-session -A -s $HOST
fi

# Source sdkman.
[[ -s $HOME/.sdkman/bin/sdkman-init.sh ]] && source $HOME/.sdkman/bin/sdkman-init.sh
