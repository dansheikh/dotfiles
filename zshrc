HISTFILE=$HOME/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

if [[ $TERM =~ (eterm*|rxvt*|screen*|xterm*) ]]; then
  autoload -Uz compinit && compinit 
  autoload -Uz colors && colors
  autoload -Uz vcs_info

  setopt EXTENDED_GLOB
  setopt INTERACTIVE_COMMENTS
  setopt NO_BEEP

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
fi

if [ -f $HOME/.aliases ]; then
  . $HOME/.aliases
fi

if [ ! -d $HOME/.config/base16-shell ]; then
  git clone https://github.com/chriskempson/base16-shell.git $HOME/.config/base16-shell
else
  BASE16_HOME=$HOME/.config/base16-shell
  [ -n $PROMPT ] && [ -s $BASE16_HOME/profile_helper.sh ] && eval "$($BASE16_HOME/profile_helper.sh)"
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
