autoload -U compinit && compinit

setopt INTERACTIVE_COMMENTS
setopt PROMPT_SUBST
setopt NO_BEEP

HISTFILE=~/.shell_history
HISTSIZE=1000
SAVEHIST=1000

PS1="%n@%m:%~%# "

# Launch Tmux.
if command -v tmux &> /dev/null; then
  [[ ! $TERM =~ dumb ]] && [[ ! $TERM =~ screen ]] && [ -z $TMUX ] && exec tmux -u
fi

# ZSH Line Editor Configuration.
if [[ ! $TERM =~ dumb ]]; then
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

export HISTCONTROL=erasedups
export LESS="-CQaixr4"
export CC=$(which clang)
export CXX=$(which clang++)
export ANACONDA3=/usr/local/share/anaconda3
export LINUXBREW=$HOME/.linuxbrew
export MONO_GAC_PREFIX=$LINUXBREW
export XDG_DATA_DIRS="$HOME/.linuxbrew/share:$XDG_DATA_DIRS"
export GOLANG=/usr/local/go
export GOPATH=/home/$USER/Workspace/Go
export PATH=$HOME/.local:$HOME/bin:$ANACONDA3/bin:$LINUXBREW/bin:$GOLANG/bin:$PATH
export MANPATH=$HOME/.linuxbrew/share/man:$MANPATH
export INFOPATH=$HOME/.linuxbrew/share/info:$INFOPATH

eval $(opam config env)

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/danish/.sdkman"
[[ -s "/home/danish/.sdkman/bin/sdkman-init.sh" ]] && source "/home/danish/.sdkman/bin/sdkman-init.sh"
