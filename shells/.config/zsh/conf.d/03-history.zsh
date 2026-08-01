# 03-history.zsh - History configuration
# HISTFILE is set in .zshenv (available to all shells).
# Options here are interactive-only.

# ============================================================================
# Sizes
# ============================================================================
HISTSIZE=50000
SAVEHIST=50000

# ============================================================================
# History options
# ============================================================================
setopt EXTENDED_HISTORY       # record timestamp and duration
setopt HIST_EXPIRE_DUPS_FIRST # trim duplicates first when pruning
setopt HIST_IGNORE_DUPS       # don't record consecutive duplicates
setopt HIST_IGNORE_ALL_DUPS   # remove older duplicate before adding new
setopt HIST_IGNORE_SPACE      # don't record commands starting with space
setopt HIST_NO_STORE          # don't store `history` / `fc` themselves
setopt HIST_NO_FUNCTIONS      # don't store function definitions
setopt HIST_REDUCE_BLANKS     # strip extra blanks
setopt HIST_VERIFY            # show expansion before executing
setopt HIST_FIND_NO_DUPS      # skip duplicates in history search
setopt INC_APPEND_HISTORY     # write immediately, not on exit
setopt SHARE_HISTORY          # share history across sessions
setopt NO_HIST_BEEP           # no beep on missing history entry

# ============================================================================
# History substring search keybindings
# (zsh-history-substring-search plugin must be loaded first via 04-plugins)
# Bindings are set here; the plugin registers the widgets on load.
# ============================================================================
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# ============================================================================
# Edit command in $EDITOR
# ============================================================================
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# ============================================================================
# history-stats — top 20 commands by frequency
# ============================================================================
function history-stats() {
  fc -l 1 \
    | awk '{CMD[$2]++;count++;}END { for (a in CMD) print CMD[a] " " CMD[a]/count*100 "% " a; }' \
    | grep -v './' \
    | column -c3 -s ' ' -t \
    | sort -nr \
    | nl \
    | head -n20
}
