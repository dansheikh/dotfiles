HISTFILE=$HOME/.bash_history
HISTSIZE=1000
SAVEHIST=1000

if [ -f $HOME/.bash_theme ]; then
    . $HOME/.bash_theme
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

# OS setup.
OS="$(/bin/uname -s)"
case "${OS}" in
    Linux*)
        export HOMEBREW_PREFIX=$(/home/linuxbrew/.linuxbrew/bin/brew --prefix)
        export BREW_BIN=/home/linuxbrew/.linuxbrew/bin/brew
    ;;
    Darwin*)
        export HOMEBREW_PREFIX=$(/usr/local/bin/brew --prefix)
        export BREW_BIN=/usr/local/bin/brew
    ;;
esac

# Activate Brew.
$(grep -Eq "^Linux.*$" <<< "$OS") && [ -s "$BREW_BIN" ] && eval "$($BREW_BIN shellenv)"

# Export environment variables.
if [ -f "$HOME/.env" ]; then
   set -a
   . "$HOME/.env"
   set +a

   PATH=$(awk -v RS=: -v ORS=: '!paths[$0]++' <<< "$PATH" | tr -s ':' | sed 's/:$//')
fi

# Activate bash completions.
[ -f "$($BREW_BIN --prefix)/etc/profile.d/bash_completion.sh" ] && . "$($BREW_BIN --prefix)/etc/profile.d/bash_completion.sh"

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

# Activate Miniconda.
if [ "$($BREW_BIN list miniconda &> /dev/null)" ]; then
    conda_setup_cmd="$($BREW_BIN --prefix conda)/base/bin/conda shell.bash hook 2> /dev/null"
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

# Activate PyEnv.
if command -v pyenv &> /dev/null; then
    eval "$(pyenv init --path)"
fi

[ -x "$(command -v neofetch)" ] && neofetch
