HISTFILE=$HOME/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

fpath+=$HOME/.zfunc

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

if [ ! -d $HOME/.config ]; then
  mkdir -p $HOME/.config
fi

if [ -f $HOME/.aliases ]; then
  . $HOME/.aliases
fi

if [ -f $HOME/.helpers ]; then
  . $HOME/.helpers
fi

# Activate Brew.
$(grep -Eq "^Linux.*$" <<< "$OS") && [ -s "$BREW_BIN" ] && eval "$($BREW_BIN shellenv)"

# Activate Nix.
if [ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
   . $HOME/.nix-profile/etc/profile.d/nix.sh
fi

# Activate SDKMAN.
[[ -s $HOME/.sdkman/bin/sdkman-init.sh ]] && source $HOME/.sdkman/bin/sdkman-init.sh

# Activate ASDF.
[ -s "$BREW_BIN" ] && "$BREW_BIN" list asdf &> /dev/null && asdf_prefix=$(brew --prefix asdf) && [ -n $asdf_prefix ] && . "$asdf_prefix/libexec/asdf.sh"

# Configure OPAM.
if command -v opam &> /dev/null; then
   eval $(opam config env)
fi

# Launch Tmux.
if command -v tmux &> /dev/null; then
  SESSION_NAME=$(date +"%H%M%S")
  VALID_TERM=$(! grep -Eq "dumb|screen.*" <<< "$TERM"; echo $?)

  [ -n $TERM ] && [ $VALID_TERM -eq 0 ] && [ -z $TMUX ] && [ -z $INSIDE_EMACS ] && [ -z $INSIDE_INTELLIJ ] && exec tmux new-session -s $SESSION_NAME
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Activate Miniconda.
case "$OS" in
  Linux*)
    if [ -d "/opt/miniconda" ] && [ "$(find /opt/miniconda -maxdepth 1 -type f | wc -l)" -eq 0 ]; then
      latest_linux_miniconda3="https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh"
      miniconda3_script_path="$HOME/Downloads/miniconda3-latest-linux-x86_64.sh"
      curl -sSL "$latest_linux_miniconda3" -o "$miniconda3_script_path"
      zsh "$miniconda3_script_path" -b -u -p "/opt/miniconda"
      rm "$miniconda3_script_path"
    fi
    if [ -f "/opt/miniconda/bin/conda" ]; then
      conda_setup_cmd="$('/opt/miniconda/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
      if [ $? -eq 0 ]; then
        eval "$conda_setup_cmd"
      else
        if [ -f "/opt/miniconda/etc/profile.d/conda.sh" ]; then
          . "/opt/miniconda/etc/profile.d/conda.sh"
        else
          export PATH="/opt/miniconda/bin:$PATH"
        fi
      fi
    fi
  ;;
  Darwin*)
    if [ "$($BREW_BIN list miniconda &> /dev/null)" ]; then
      conda_setup_cmd="$($BREW_BIN --prefix conda)/base/bin/conda shell.zsh hook 2> /dev/null"
      if [ $? -eq 0 ]; then
        eval "$conda_setup_cmd"
      else
        if [ -f "$($BREW_BIN --prefix conda)/base/etc/profile.d/conda.sh" ]; then
          . "$($BREW_BIN --prefix conda)/base/etc/profile.d/conda.sh"
        else
          export PATH="$($BREW_BIN --prefix conda)/base/bin:$PATH"
        fi
      fi
      unset conda_setup_cmd
    fi
  ;;
esac

# Activate PyEnv.
if command -v pyenv &> /dev/null; then
  eval "$(pyenv init --path)"
fi
