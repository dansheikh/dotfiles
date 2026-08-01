# 09-functions.zsh - Custom utility functions
# (was 08-functions.zsh; renumbered)

# ============================================================================
# Directory & file operations
# ============================================================================

# mkcd — create directory and cd into it
mkcd() {
  (( $# == 1 )) || { echo "Usage: mkcd <directory>"; return 1 }
  mkdir -p "$1" && cd "$1"
}

# take — clone a git repo and cd into it; or mkdir+cd for a plain path
take() {
  (( $# == 1 )) || { echo "Usage: take <git-url|directory>"; return 1 }
  if [[ $1 == git://* || $1 == https://* || $1 == git@* ]]; then
    git clone "$1" && cd "$(basename "$1" .git)"
  else
    mkdir -p "$1" && cd "$1"
  fi
}

# backup — timestamped copy of a file or directory
backup() {
  (( $# == 1 )) || { echo "Usage: backup <file-or-directory>"; return 1 }
  local ts=$(date +%Y%m%d_%H%M%S)
  cp -r "$1" "${1}.backup.${ts}"
  echo "Backup created: ${1}.backup.${ts}"
}

# extract — universal archive extractor
extract() {
  (( $# == 1 )) || { echo "Usage: extract <archive>"; return 1 }
  [[ -f $1 ]] || { echo "Error: '$1' is not a file"; return 1 }
  case "$1" in
    *.tar.bz2)  tar xjf "$1"         ;;
    *.tar.gz)   tar xzf "$1"         ;;
    *.tar.xz)   tar xJf "$1"         ;;
    *.tar.zst)  tar --zstd -xf "$1"  ;;
    *.tar)      tar xf  "$1"         ;;
    *.tbz2)     tar xjf "$1"         ;;
    *.tgz)      tar xzf "$1"         ;;
    *.bz2)      bunzip2 "$1"         ;;
    *.gz)       gunzip  "$1"         ;;
    *.zip)      unzip   "$1"         ;;
    *.rar)      unrar x "$1"         ;;
    *.Z)        uncompress "$1"      ;;
    *.7z)       7z x    "$1"         ;;
    *.deb)      ar x    "$1"         ;;
    *.zst)      zstd -d "$1"         ;;
    *)          echo "Error: don't know how to extract '$1'"; return 1 ;;
  esac
}

# ============================================================================
# Search & navigation
# ============================================================================

# fcd — fuzzy cd
fcd() {
  local dir
  dir=$(fd --type d --hidden --follow --exclude .git 2>/dev/null \
    | fzf --preview 'eza --tree --level=1 --color=always --icons {} 2>/dev/null || tree -C -L 1 {} | head -100') \
    && cd "$dir"
}

# fopen — fuzzy find and open in $EDITOR
fopen() {
  local file
  file=$(fd --type f --hidden --follow --exclude .git 2>/dev/null \
    | fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}') \
    && ${EDITOR:-nvim} "$file"
}

# ============================================================================
# Git utilities
# ============================================================================

# git-clean-branches — delete local merged branches
git-clean-branches() {
  git branch --merged \
    | grep -v -E '^\*|^.*(main|master|develop)' \
    | xargs -n1 git branch -d
  echo "Cleaned up merged branches."
}

# git-undo-commit — undo last commit, keep changes staged
git-undo-commit() {
  git reset --soft HEAD~1
  echo "Last commit undone; changes kept in staging."
}

# ============================================================================
# Network & system
# ============================================================================

# ports — show listening TCP ports
ports() {
  if command -v lsof &>/dev/null; then
    lsof -iTCP -sTCP:LISTEN -n -P
  else
    ss -tulanp
  fi
}

# myip — show public and local IP addresses
myip() {
  echo "Public IP:  $(curl -s ifconfig.me)"
  echo "Local IP:   $(ipconfig getifaddr en0 2>/dev/null || hostname -I | awk '{print $1}')"
}

# ============================================================================
# Development utilities
# ============================================================================

# serve — start a local HTTP server
serve() {
  local port="${1:-8000}"
  echo "Serving on http://localhost:${port}"
  python3 -m http.server "$port"
}

# json — pretty-print JSON
json() {
  if (( $# == 0 )); then
    python3 -m json.tool
  else
    python3 -m json.tool "$1"
  fi
}

# encode64 / decode64
encode64() { (( $# )) && base64 < "$1" || base64 }
decode64() { (( $# )) && base64 --decode < "$1" || base64 --decode }

# ============================================================================
# File content
# ============================================================================

# findtext — search for text in files
findtext() {
  (( $# >= 1 )) || { echo "Usage: findtext <pattern> [directory]"; return 1 }
  local pattern="$1" dir="${2:-.}"
  if command -v rg &>/dev/null; then
    rg --color=always --line-number "$pattern" "$dir"
  else
    grep -rnw "$dir" -e "$pattern" --color=always
  fi
}

# replace — in-place text substitution
replace() {
  (( $# == 3 )) || { echo "Usage: replace <search> <replace> <file>"; return 1 }
  [[ -f $3 ]] || { echo "Error: '$3' not found"; return 1 }
  sed -i "s/${1}/${2}/g" "$3"
  echo "Replaced '$1' with '$2' in $3"
}

# ============================================================================
# Process management
# ============================================================================

# psgrep — search running processes
psgrep() {
  (( $# )) || { echo "Usage: psgrep <name>"; return 1 }
  ps aux | grep -v grep | grep -i "$@"
}

# killport — kill whatever is listening on a port
killport() {
  (( $# == 1 )) || { echo "Usage: killport <port>"; return 1 }
  local pid=$(lsof -ti tcp:"$1")
  if [[ -n $pid ]]; then
    kill -9 "$pid"
    echo "Killed PID $pid (port $1)"
  else
    echo "No process on port $1"
  fi
}

# ============================================================================
# Disk & system info
# ============================================================================

# duh — disk usage sorted by size
duh() {
  du -h --max-depth=1 "${1:-.}" | sort -hr
}

# largest — N largest files under current directory
largest() {
  du -ah . | sort -rh | head -n "${1:-10}"
}

# ============================================================================
# PATH management
# ============================================================================

# path-add — prepend a directory to PATH, no duplicates
path-add() {
  (( $# == 1 )) || { echo "Usage: path-add <directory>"; return 1 }
  [[ -d $1 ]] || { echo "Warning: '$1' is not a directory"; return 1 }
  [[ :$PATH: == *:$1:* ]] || export PATH="$1:$PATH"
}

# ============================================================================
# Quick helpers
# ============================================================================

# up — go up N directories
up() {
  local d='' limit="${1:-1}"
  for (( i=1; i<=limit; i++ )); do d="../$d"; done
  cd "$d"
}

# zreload — full config reload
zreload() {
  exec zsh
}

# zsh-functions — list custom functions defined in this file
zsh-functions() {
  grep -E '^[a-zA-Z_-]+\(\)' "${ZDOTDIR:-~/.config/zsh}/conf.d/09-functions.zsh" \
    | sed 's/() {.*//' \
    | sort
}
