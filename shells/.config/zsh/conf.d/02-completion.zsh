# 02-completion.zsh - Completion zstyles
#
# NOTE: compinit is intentionally NOT called here. It runs in 05-compinit.zsh
# after plugins (04-plugins.zsh) have had a chance to extend fpath.
# Only zstyle configuration belongs in this file.

# ============================================================================
# Cache
# ============================================================================
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompcache"

# ============================================================================
# Matching and correction
# ============================================================================
zstyle ':completion:*' matcher-list \
  'm:{a-zA-Z}={A-Za-z}' \
  'r:|[._-]=* r:|=*' \
  'l:|=* r:|=*'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# ============================================================================
# Grouping and formatting
# ============================================================================
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes
zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
zstyle ':completion:*:messages'     format '%F{purple}-- %d --%f'
zstyle ':completion:*:warnings'     format '%F{red}-- no matches found --%f'
zstyle ':completion:*:corrections'  format '%F{green}-- %d (errors: %e) --%f'

# ============================================================================
# Menu
# ============================================================================
zstyle ':completion:*'          menu select
zstyle ':completion:*:*:*:*:*'  menu select
zstyle ':completion:*:matches'  group yes
zstyle ':completion:*:options'  description yes
zstyle ':completion:*:options'  auto-description '%d'

# ============================================================================
# Colors
# ============================================================================
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:kill:*:processes' \
  list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# ============================================================================
# Directories
# ============================================================================
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' squeeze-slashes true

# ============================================================================
# Processes
# ============================================================================
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*:*:kill:*'        menu yes select
zstyle ':completion:*:*:kill:*'        force-list always
zstyle ':completion:*:*:kill:*'        insert-ids single

# ============================================================================
# Git
# ============================================================================
zstyle ':completion:*:git:*' group-order 'main commands' 'alias commands' 'external commands'

# ============================================================================
# Rehash — pick up newly installed executables automatically
# ============================================================================
zstyle ':completion:*' rehash true

# ============================================================================
# fzf-tab general settings
# (plugin-specific previews follow after fzf-tab loads in 04-plugins.zsh)
# ============================================================================
zstyle ':fzf-tab:*' fzf-command fzf
zstyle ':fzf-tab:*' fzf-flags --height=80% --layout=reverse
zstyle ':fzf-tab:*' fzf-preview-window right:60%:wrap
zstyle ':fzf-tab:*' switch-group '<' '>'
zstyle ':completion:complete:*:options' sort false

# ============================================================================
# fzf-tab previews
# ============================================================================

# Directories
zstyle ':fzf-tab:complete:cd:*' fzf-preview \
  'eza -1 --color=always --icons $realpath 2>/dev/null || ls -1 --color=always $realpath'

# Environment variables
zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
  fzf-preview 'echo ${(P)word}'

# kill
zstyle ':fzf-tab:complete:kill:argument-rest' fzf-preview \
  'ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:kill:argument-rest' fzf-flags '--preview-window=down:3:wrap'

# git
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
  'git diff $word 2>/dev/null | delta 2>/dev/null || git diff $word --color=always'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
  'git log --color=always $word'
zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
  'git show --color=always $word | delta 2>/dev/null || git show --color=always $word'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
  'case "$group" in
    "modified file")          git diff $word --color=always | delta 2>/dev/null || git diff $word --color=always ;;
    "recent commit object name") git show --color=always $word | delta 2>/dev/null || git show --color=always $word ;;
    *)                        git log --color=always $word ;;
  esac'

# Commands (tldr → man → which fallback)
zstyle ':fzf-tab:complete:-command-:*' fzf-preview \
  '(out=$(tldr --color always "$word") 2>/dev/null && echo $out) ||
   (out=$(MANWIDTH=$FZF_PREVIEW_COLUMNS man "$word") 2>/dev/null && echo $out) ||
   (out=$(which "$word") && echo $out) ||
   echo "${(P)word}"'

# Systemd
zstyle ':fzf-tab:complete:systemctl-*:*' fzf-preview \
  'SYSTEMD_COLORS=1 systemctl status $word'

# Docker containers
zstyle ':fzf-tab:complete:docker-*:*' fzf-preview \
  'docker inspect $word 2>/dev/null | jq -C ".[0] | {Name,Status:.State.Status,Image,Ports:.NetworkSettings.Ports}" 2>/dev/null || echo $word'

# mise runtimes
zstyle ':fzf-tab:complete:mise:*' fzf-preview \
  'mise ls --current 2>/dev/null || echo $word'

