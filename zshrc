autoload -U compinit && compinit

setopt INTERACTIVE_COMMENTS
setopt PROMPT_SUBST
setopt NO_BEEP

HISTFILE=~/.shell_history
HISTSIZE=500
SAVEHIST=500

PS1="%n@%m:%~%# "

# Launch Tmux.
if command -v tmux &> /dev/null; then
  [[ ! $TERM =~ screen ]] && [ -z $TMUX ] && exec tmux -u
fi

# ZSH Line Editor Configuration.
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

# Styling.
zstyle ':completion:*' format %d
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=1
zstyle ':completion:*' completer _expand _complete _correct
zstyle ':completion:*' verbose yes

if [ -f ~/.aliases ]; then
   source ~/.aliases
fi

eval $(opam config env)

export CC=$(which clang)
export CXX=$(which clang++)
export ANACONDA3=/usr/local/share/anaconda/anaconda3
export GOLANG=/usr/local/go
export GOPATH=/home/$USER/Workspace/Go
export PATH=$HOME/bin:$GOLANG/bin:$SCALA_HOME/bin:$SBT_HOME/bin:$ANACONDA3/bin:$PATH

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/danish/.sdkman"
[[ -s "/home/danish/.sdkman/bin/sdkman-init.sh" ]] && source "/home/danish/.sdkman/bin/sdkman-init.sh"
