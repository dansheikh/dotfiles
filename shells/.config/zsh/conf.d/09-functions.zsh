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
# Multimedia
# ============================================================================

# mpv-playlist — finds and plays video files (case-insensitive, excludes hidden dirs) in natural sort order using mpv
mpv-playlist() {
    emulate -L zsh
    setopt localoptions pipefail

    local help_text="Usage: mpv-playlist [options] [path_or_file]

Options:
  -i, --interactive      Select files interactively using fzf (multi-select)
  -s, --start N          Start playback at file index N (1-based)
  -S, --skip N           Skip the first N files
  -e, --end N            End playback at file index N
  -E, --extensions EXTS  File extensions to include
  -r, --reverse          Reverse the natural sort order
  -h, --help             Display this help message

Examples:
  mpv-playlist -i ~/Videos
  mpv-playlist --start 22 ~/Videos/Show
  mpv-playlist -r -S 2 ~/Videos/Movies
"

    local -a help=() interactive=() start_opt=() skip_opt=() end_opt=() ext_opt=() reverse=()

    zparseopts -D -F -K -- \
        h=help -help=help \
        i=interactive -interactive=interactive \
        s:=start_opt -start:=start_opt \
        S:=skip_opt -skip:=skip_opt \
        e:=end_opt -end:=end_opt \
        E:=ext_opt -extensions:=ext_opt \
        r=reverse -reverse=reverse || {
        echo "Run 'mpv-playlist --help' for usage." >&2
        return 1
    }

    (( ${#help} )) && { print -r -- "$help_text"; return 0; }

    # Dependencies check
    command -v mpv >/dev/null 2>&1 || { echo "Error: mpv is not installed." >&2; return 1; }
    (( ${#interactive} )) && { command -v fzf >/dev/null 2>&1 || { echo "Error: fzf is not installed." >&2; return 1; } }

    (( $# > 1 )) && { echo "Error: Too many positional arguments." >&2; return 1; }

    local target="${1:-.}"

    # Validate numeric options using native Zsh pattern matching (<1->, <0->)
    local start_value="${start_opt[2]:-1}"
    local skip_value="${skip_opt[2]:-0}"
    local end_value="${end_opt[2]:-0}"

    [[ "$start_value" == <1-> ]] || { echo "Error: --start requires a positive integer." >&2; return 1; }
    [[ "$skip_value"  == <0-> ]] || { echo "Error: --skip requires a non-negative integer." >&2; return 1; }
    [[ "$end_value"   == <0-> ]] || { echo "Error: --end requires a non-negative integer." >&2; return 1; }

    local start_index=$(( start_value ))
    local skip_count=$(( skip_value ))
    local end_index=$(( end_value ))

    # Direct file playback
    if [[ -f "$target" ]]; then
        mpv -- "$target"
        return $?
    fi

    [[ -d "$target" ]] || { echo "Error: '$target' is not a valid file or directory." >&2; return 1; }

    # Parse extensions
    local -a exts=()
    if (( ${#ext_opt} )); then
        exts=(${=ext_opt[2]//,/ })
    fi
    (( ${#exts} )) || exts=(mp4 mkv webm avi mov flv wmv)

    # Build find arguments safely
    local -a find_args=("$target" -type f '!' -path '*/.*' '!' -name '.*' '(')
    local first_ext=1

    for ext in "${exts[@]}"; do
        [[ -n "$ext" ]] || continue
        (( first_ext )) || find_args+=(-o)
        find_args+=(-iname "*.${ext#.}")
        first_ext=0
    done

    (( first_ext )) && { echo "Error: No valid extensions supplied." >&2; return 1; }
    find_args+=(')')

    # Sorting flag
    local sort_flag="-V"
    (( ${#reverse} )) && sort_flag="-Vr"

    # Fetch and sanitize file list
    local -a files=()
    files=("${(@f)$(find "${find_args[@]}" 2>/dev/null | sort "$sort_flag")}")
    files=(${files:#}) # Strip empty elements

    local total_files=${#files}
    (( total_files > 0 )) || { echo "No matching video files found in '$target'." >&2; return 1; }

    # Range & Selection handling
    if (( ${#interactive} )); then
        local selected
        selected="$(printf '%s\n' "${files[@]}" |
            fzf -m \
                --prompt="Select videos > " \
                --header="TAB to multi-select, ENTER to play")"

        [[ -n "$selected" ]] || return 0
        files=("${(@f)selected}")
    else
        # Apply skip
        if (( skip_count > 0 )); then
            (( skip_count >= total_files )) && { echo "Error: Skip count exceeds total files." >&2; return 1; }
            files=("${files[@]:skip_count}")
            total_files=${#files}
        fi

        # Apply range and strict bounds checking
        (( start_index > total_files )) && { echo "Error: Start index exceeds available files (total: $total_files)." >&2; return 1; }

        local end=$end_index
        (( end <= 0 || end > total_files )) && end=$total_files

        (( start_index > end )) && { echo "Error: Start index cannot exceed end index." >&2; return 1; }

        files=("${(@)files[start_index,end]}")
    fi

    files=(${files:#})
    (( ${#files} > 0 )) || { echo "No valid files selected for playback." >&2; return 1; }

    echo "Playing ${#files} file(s)..."
    mpv -- "${files[@]}"
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
