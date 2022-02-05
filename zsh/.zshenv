# OS setup.
OS="$(uname -s)"
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

# Exports.
export ANDROID_SDK_ROOT=/usr/local/share/android-sdk
export CARGO_HOME="$HOME/.cargo"
export CONDA_AUTO_ACTIVATE_BASE=false
"$BREW_BIN" list curl &> /dev/null && export CURL_BIN=$(${BREW_BIN} --prefix curl)/bin
export DOTFILES="$HOME/Dotfiles"
"$BREW_BIN" list flutter &> /dev/null && export FLUTTER_HOME=$(${BREW_BIN} --prefix flutter)
export FZF_DEFAULT_COMMAND="fd --type f"
export GOPATH="$HOME/go"
"$BREW_BIN" list go &> /dev/null && export GOROOT="$(${BREW_BIN} --prefix go)/libexec"
export HISTCONTROL=erasedups
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LESS="-CQaixr4"
export PIPENV_VENV_IN_PROJECT=1
"$BREW_BIN" list openssl &> /dev/null && export OPENSSL_BIN=$(${BREW_BIN} --prefix openssl)/bin
export PUB_CACHE="$HOME/.pub-cache"
export SDKMAN_DIR="$HOME/.sdkman"
export VIMCONFIG="$HOME/.vim"
export VIMDATA="$HOME/.vim"

# Paths.
export FPATH=$(${BREW_BIN} --prefix)/share/zsh/site-functions:$FPATH
export PATH="$HOME/.local/bin:$PUB_CACHE/bin:$HOME/local/bin:$HOME/bin:$OPENSSL_BIN:$CURL_BIN:/usr/local/bin:/usr/local/sbin:$VIMCONFIG/pack/bundle/start/fzf/bin:$GOROOT/bin:$GOPATH/bin:$ANDROID_SDK_ROOT/build-tools:$ANDROID_SDK_ROOT/emulator:$ANDROID_SDK_ROOT/tools:$ANDROID_SDK_ROOT/tools/bin:$ANDROID_SDK_ROOT/platform-tools:$FLUTTER_HOME/bin:$CARGO_HOME/bin:$PATH"
