# 06-fzf.zsh - FZF configuration, keybindings, and custom widgets
# (was 05-fzf.zsh; renumbered to load after compinit)

# ============================================================================
# Guard
# ============================================================================
if ! command -v fzf &>/dev/null; then
  print -P "%F{yellow}warn:%f fzf not found — skipping fzf configuration"
  return
fi

# ============================================================================
# Base options (shared across all fzf invocations)
# ============================================================================
export FZF_DEFAULT_OPTS="
  --cycle
  --info=inline
  --input-label=' Input '
  --layout=reverse
  --style=full
  --tmux center,80%,60%
  --color='border:#aaaaaa,label:#cccccc'
  --color='list-border:#669966,list-label:#99cc99'
  --color='header-border:#6699cc,header-label:#99ccff'
  --bind='result:transform-list-label:
    if [[ -z \$FZF_QUERY ]]; then
      echo \" \$FZF_MATCH_COUNT items \"
    else
      echo \" \$FZF_MATCH_COUNT matches for [\$FZF_QUERY] \"
    fi'
"

# ============================================================================
# Default file command
# ============================================================================
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

# ============================================================================
# Ctrl-T  — file picker
# ============================================================================
export FZF_CTRL_T_COMMAND="${FZF_DEFAULT_COMMAND}"
export FZF_CTRL_T_OPTS="
  --bind='focus:transform-preview-label:[[ -n {} ]] && printf \" Preview [%s] \" {}'
  --bind='focus:+transform-header:file --brief {} || echo \"No file selected\"'
  --header-label=' File Type '
  --preview='bat -n --color=always {} 2>/dev/null || cat {}'
"

# ============================================================================
# Alt-C  — directory picker
# ============================================================================
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'
export FZF_ALT_C_OPTS="
  --bind='focus:transform-preview-label:[[ -n {} ]] && printf \" Preview [%s] \" {}'
  --preview='eza --tree --level=2 --color=always --icons=always {} 2>/dev/null || tree -C {} | head -100'
  --walker-skip=.git,node_modules,target,venv,.venv,__pycache__
"

# ============================================================================
# Initialize fzf shell integration
# ============================================================================
source <(fzf --zsh)

# ============================================================================
# Custom ZLE widgets
# (all functions registered with `zle -N` and bound properly)
# ============================================================================

# fzf-git-branch — interactive branch switcher
fzf-git-branch() {
  local branch
  branch=$(git branch -a | grep -v HEAD \
    | fzf-tmux -p --ansi \
        --preview='git log --oneline --graph --color=always {1}' \
    | sed 's/.* //' | sed 's#remotes/[^/]*/##') || return
  git checkout "${branch}"
  zle reset-prompt
}
zle -N fzf-git-branch
bindkey '^Gb' fzf-git-branch

# fzf-git-log — interactive commit browser
fzf-git-log() {
  git log --graph --color=always \
    --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" \
  | fzf --ansi --no-sort --reverse --tiebreak=index \
      --preview='echo {} | grep -o "[a-f0-9]\{7\}" | head -1 | xargs git show --color=always' \
      --bind='enter:execute:echo {} | grep -o "[a-f0-9]\{7\}" | head -1 | xargs git show | less -R'
  zle reset-prompt
}
zle -N fzf-git-log
bindkey '^Gl' fzf-git-log

# fzf-kill — interactive process killer
fzf-kill() {
  local pid
  if [[ $UID -eq 0 ]]; then
    pid=$(ps -ef | sed 1d | fzf -m --preview 'echo {}' --preview-window=down:3:wrap | awk '{print $2}')
  else
    pid=$(ps -f -u $UID | sed 1d | fzf -m --preview 'echo {}' --preview-window=down:3:wrap | awk '{print $2}')
  fi
  [[ -n $pid ]] && echo $pid | xargs kill -${1:-9}
  zle reset-prompt
}
zle -N fzf-kill
bindkey '^K' fzf-kill

# fzf-env — browse environment variables
fzf-env() {
  local var
  var=$(env | fzf --preview 'echo {}' | cut -d= -f1) || return
  echo $var
  zle reset-prompt
}
zle -N fzf-env
bindkey '^Ge' fzf-env

# fzf-cd-with-preview — enhanced directory picker (cd on selection)
fzf-cd-with-preview() {
  local dir
  dir=$(fd --type d --hidden --follow --exclude .git 2>/dev/null \
    | fzf --preview 'eza --tree --level=1 --color=always --icons=always {} 2>/dev/null || tree -C -L 1 {} | head -100' \
          --preview-window=right:60%:wrap) || return
  cd "$dir"
  zle reset-prompt
}
zle -N fzf-cd-with-preview
bindkey '^G^D' fzf-cd-with-preview

# fzf-history-execute — pick from history and execute immediately
fzf-history-execute() {
  local selected
  selected=$(fc -rl 1 \
    | fzf --tac --no-sort --query="$LBUFFER" \
          --preview='echo {}' --preview-window=down:3:wrap \
    | sed 's/ *[0-9]* *//') || return
  BUFFER="$selected"
  zle accept-line
}
zle -N fzf-history-execute
bindkey '^X^R' fzf-history-execute

# ============================================================================
# Additional bindings
# ============================================================================
bindkey '^P'   fzf-file-widget   # Ctrl-P  — file search (mirrors Ctrl-T)
bindkey '^G^F' fzf-file-widget   # Ctrl-G Ctrl-F — file search with preview
