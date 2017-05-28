HISTFILE=$HOME/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

if [[ $TERM =~ (screen*|rxvt*|xterm*) ]]; then
  autoload -Uz compinit && compinit

  setopt INTERACTIVE_COMMENTS
  setopt PROMPT_SUBST
  setopt NO_BEEP

  PS1="%n@%m:%~%# "

  # Bindings.
  bindkey -e
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

if [ -f ~/.aliases ]; then
  source ~/.aliases
fi

eval $(opam config env)

# Launch Tmux.
if command -v tmux &> /dev/null; then
  [[ ! $TERM =~ dumb ]] && [[ ! $TERM =~ screen ]] && [[ -z $TMUX ]] && exec tmux -u
fi
