autoload -Uz compinit && compinit
autoload -Uz promptinit && promptinit

zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
zstyle ':completion:*:git:*' group-order 'main commands' 'alias commands' 'external commands'

# Environment Variables
export HISTFILE=${XDG_DATA_HOME:-~/.local/share}/zsh/zsh_history
export HISTSIZE=1000

# Antidote
antidote_path=$(nix eval --raw nixpkgs#antidote)
source ${antidote_path}/share/antidote/antidote.zsh
zsh_plugins=${ZDOTDIR:-~}/.zsh_plugins
[[ -f ${zsh_plugins}.txt ]] || touch ${zsh_plugins}.txt
fpath=("${antidote_path}/share/antidote/functions" ${fpath})
autoload -Uz antidote
if [[ ! ${zsh_plugins}.zsh -nt ${zsh_plugins}.txt ]]; then
  antidote bundle < ${zsh_plugins}.txt >| ${zsh_plugins}.zsh
fi
source ${zsh_plugins}.zsh

# FZF
source <(fzf --zsh)
export FZF_ALT_C_COMMAND=$(<<- EOF
fd --type d --hidden
EOF
)
export FZF_ALT_C_OPTS=$(<<- EOF
--bind 'focus:transform-preview-label:[[ -n {} ]] && printf " Preview [%s] " {}'
--bind 'ctrl-r:change-list-label( Reloading the list )+reload(sleep 2; git ls-files)'
--color 'border:#aaaaaa,label:#cccccc'
--color 'list-border:#669966,list-label:#99cc99'
--color 'header-border:#6699cc,header-label:#99ccff'
--cycle
--info=inline
--input-label ' Input '
--layout=reverse
--preview 'tree -C {}'
--style full
--tmux center,80%,60%
--walker-skip .git,node_modules,target
EOF
)
export FZF_CTRL_T_COMMAND=$(<<- EOF
fd --type f --hidden
EOF
)
export FZF_CTRL_T_OPTS=$(<<- EOF
--bind 'result:transform-list-label:
    if [[ -z $FZF_QUERY ]]; then
      echo " $FZF_MATCH_COUNT items "
    else
      echo " $FZF_MATCH_COUNT matches for [$FZF_QUERY] "
    fi'
--bind 'focus:transform-preview-label:[[ -n {} ]] && printf " Preview [%s] " {}'
--bind 'focus:+transform-header:file --brief {} || echo "No file selected"'
--bind 'ctrl-r:change-list-label( Reloading the list )+reload(sleep 2; git ls-files)'
--color 'border:#aaaaaa,label:#cccccc'
--color 'list-border:#669966,list-label:#99cc99'
--color 'header-border:#6699cc,header-label:#99ccff'
--cycle
--header-label ' File Type '
--info=inline
--input-label ' Input '
--layout=reverse
--preview 'bat -n --color=always {}'
--style full
--tmux center,80%,60%
EOF
)
export FZF_DEFAULT_COMMAND=$(<<- EOF
fd --type f --hidden
EOF
)
export FZF_DEFAULT_OPTS=$(<<- EOF
--bind 'result:transform-list-label:
    if [[ -z $FZF_QUERY ]]; then
      echo " $FZF_MATCH_COUNT items "
    else
      echo " $FZF_MATCH_COUNT matches for [$FZF_QUERY] "
    fi'
--bind 'ctrl-r:change-list-label( Reloading the list )+reload(sleep 2; git ls-files)'
--color 'border:#aaaaaa,label:#cccccc'
--color 'list-border:#669966,list-label:#99cc99'
--color 'header-border:#6699cc,header-label:#99ccff'
--cycle
--info=inline
--input-label ' Input '
--layout=reverse
--style full
--tmux center,80%,60%
EOF
)

# TMux
command -v tmux &> /dev/null && [ -z $TMUX ] && [ -z $INSIDE_EMACS ] && { tmux has &> /dev/null && tmux attach || exec tmux new && exit; }

# Starship
command -v starship &> /dev/null && eval "$(starship init zsh)"

# Nerdfetch
command -v nerdfetch &> /dev/null && nerdfetch

# Direnv
command -v direnv &> /dev/null && eval "$(direnv hook zsh)"
