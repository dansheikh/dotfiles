# 07-aliases.zsh - Command aliases
# (was 06-aliases.zsh; renumbered)

# ============================================================================
# Safety
# ============================================================================
alias rm='rm -iv'
alias cp='cp -iv'
alias mv='mv -iv'
alias ln='ln -iv'

# ============================================================================
# Modern replacements — only when the tool is present
# ============================================================================

# ls → eza
if command -v eza &>/dev/null; then
  alias ls='eza --icons'
  alias la='eza -lAh --icons'
  alias ll='eza -lh --icons'
  alias lt='eza --tree --level=2 --icons'
  alias lta='eza --tree --level=2 --icons -a'
  alias l='eza -lah --icons'
else
  alias ls='ls --color=auto'
  alias la='ls -lAh --color=auto'
  alias ll='ls -lh --color=auto'
  alias l='ls -lah --color=auto'
fi

# diff
alias diff='diff --color=auto'
alias ip='ip --color=auto'

# ============================================================================
# Directory navigation
# ============================================================================
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias -- -='cd -'

# ============================================================================
# File operations
# ============================================================================
alias mkdir='mkdir -pv'
alias md='mkdir -pv'

# ============================================================================
# Editor
# ============================================================================
if command -v nvim &>/dev/null; then
  alias v='nvim'
  alias vi='nvim'
  alias vim='nvim'
  alias nv='nvim'
fi

# ============================================================================
# Disk usage
# ============================================================================
alias df='df -h'

# ============================================================================
# System information
# ============================================================================
alias listening='lsof -iTCP -sTCP:LISTEN -n -P'
alias weather='curl wttr.in'

# ============================================================================
# Process management
# ============================================================================
alias psg='ps aux | grep -v grep | grep -i -e VSZ -e'
alias htop='htop --sort-key=PERCENT_CPU'

# ============================================================================
# Git
# ============================================================================
alias g='git'
alias gdiff='git diff --color | diff-so-fancy'

# ============================================================================
# Tmux
# ============================================================================
alias t='tmux'
alias ta='tmux attach'
alias tl='tmux list-sessions'
alias tn='tmux new-session -s'

# ============================================================================
# Network
# ============================================================================
alias pingg='ping google.com'

# ============================================================================
# Docker
# ============================================================================
if command -v docker &>/dev/null; then
  alias d='docker'
  alias dc='docker compose'
  alias dps='docker ps'
  alias dpsa='docker ps -a'
  alias di='docker images'
  alias dex='docker exec -it'
  alias dlogs='docker logs -f'
  alias dprune='docker system prune -a'
  alias dstop='docker stop $(docker ps -q)'
fi

# ============================================================================
# Kubernetes
# ============================================================================
if command -v kubectl &>/dev/null; then
  alias k='kubectl'
  alias kg='kubectl get'
  alias kd='kubectl describe'
  alias kdel='kubectl delete'
  alias kl='kubectl logs'
  alias kx='kubectl exec -it'
fi

# ============================================================================
# Quick config edits
# ============================================================================
alias zshconfig='${EDITOR:-nvim} ${ZDOTDIR:-~/.config/zsh}/.zshrc'
alias zshenv='${EDITOR:-nvim} ~/.zshenv'
alias zshreload='source ${ZDOTDIR:-~/.config/zsh}/.zshrc'

# ============================================================================
# Misc
# ============================================================================
alias h='history'
alias hg='history | grep'
alias j='jobs -l'
alias path='echo $PATH | tr ":" "\n"'
alias c='clear'
alias :q='exit'
alias q='exit'

# ============================================================================
# Global aliases
# ============================================================================
alias -g L='| less'
alias -g G='| grep'
alias -g H='| head'
alias -g T='| tail'
alias -g W='| wc -l'
alias -g S='| sort'
alias -g NE='2>/dev/null'
alias -g NUL='>/dev/null 2>&1'

# ============================================================================
# Suffix aliases
# ============================================================================
if command -v nvim &>/dev/null; then
  alias -s txt=nvim
  alias -s md=nvim
  alias -s json=nvim
  alias -s yaml=nvim
  alias -s yml=nvim
fi
alias -s html=open
alias -s pdf=open
