# 01-options.zsh - Shell behaviour and options

# ============================================================================
# Directory Navigation
# ============================================================================
setopt AUTO_CD              # cd by typing directory name
setopt AUTO_PUSHD           # push directories onto the stack
setopt PUSHD_IGNORE_DUPS    # no duplicate entries in the stack
setopt PUSHD_SILENT         # suppress stack output after pushd/popd
setopt PUSHD_MINUS          # exchange +/- meanings for pushd

# ============================================================================
# Globbing & Expansion
# ============================================================================
setopt EXTENDED_GLOB        # #, ~, ^ glob operators
setopt GLOB_DOTS            # include dotfiles in globs (not . and ..)
setopt NUMERIC_GLOB_SORT    # sort numeric filenames numerically
setopt NO_CASE_GLOB         # case-insensitive globbing
setopt MAGIC_EQUAL_SUBST    # filename completion after = in arguments

# ============================================================================
# Completion Behaviour
# ============================================================================
setopt COMPLETE_IN_WORD     # complete from both ends of a word
setopt ALWAYS_TO_END        # move cursor to end after completion
setopt AUTO_MENU            # show menu on successive Tab
setopt AUTO_LIST            # list choices on ambiguous completion
setopt AUTO_PARAM_SLASH     # add trailing slash for directory parameters

# ============================================================================
# Input / Output
# ============================================================================
setopt INTERACTIVE_COMMENTS # allow # comments in interactive shell
setopt RC_QUOTES            # allow '' inside single-quoted strings
setopt COMBINING_CHARS      # handle zero-width combining characters

# ============================================================================
# Terminal
# ============================================================================
setopt NO_BEEP              # never beep
setopt NO_FLOW_CONTROL      # disable Ctrl-S / Ctrl-Q flow control

# ============================================================================
# Job Control
# ============================================================================
setopt LONG_LIST_JOBS       # list jobs in long format
setopt AUTO_RESUME          # resume job on exact-match command
setopt NOTIFY               # report background job status immediately
setopt NO_HUP               # don't HUP background jobs on exit
setopt NO_CHECK_JOBS        # don't warn about running jobs on exit
