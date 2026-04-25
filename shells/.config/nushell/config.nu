# config.nu — Nushell configuration
# Location: ~/.config/nushell/config.nu
#
# ── Home-manager note ────────────────────────────────────────────────────────
# If Nushell is managed by home-manager, this file may be generated and
# marked read-only. Do not edit it directly. Instead use:
#
#   programs.nushell.extraConfig = builtins.readFile ./config.nu;
#
# ── nix develop / nix-shell ──────────────────────────────────────────────────
# Nix shell environments spawn bash by default. To use Nu inside them:
#
#   nix develop --command nu          # one-off
#   shellHook = "exec nu";            # in flake devShell (replaces bash)
#
# With direnv + nix-direnv and `use flake` in .envrc, the env_change.PWD hook below injects
# all flake env vars automatically on each prompt — no extra steps needed.
#
# ── Vendor files ─────────────────────────────────────────────────────────────
# zoxide and atuin define Nu commands and keybindings — they cannot be
# initialised inline and must be sourced as pre-generated files.
# Run once after installing each tool, then uncomment the source lines
# at the bottom of this file:
#
#   mkdir -p ~/.config/nushell/vendor
#   zoxide init nushell | save -f ~/.config/nushell/vendor/zoxide.nu
#   atuin init nu       | save -f ~/.config/nushell/vendor/atuin.nu
#
# ─────────────────────────────────────────────────────────────────────────────

# ============================================================================
# Settings
# ============================================================================
$env.config = ($env.config | merge deep {

    buffer_editor: "nvim"

    # History — SQLite enables structured queries: `history | where command =~ "git"`
    # Equivalent Zsh opts: HISTSIZE, SAVEHIST, EXTENDED_HISTORY, SHARE_HISTORY,
    # HIST_IGNORE_DUPS, HIST_IGNORE_SPACE, INC_APPEND_HISTORY
    history: {
        max_size:      50000
        sync_on_enter: true   # write immediately — INC_APPEND_HISTORY
        file_format:   "sqlite"
        isolation:     false  # share across sessions — SHARE_HISTORY
    }

    # Completions — external completer configured below
    # Equivalent Zsh opts: COMPLETE_IN_WORD, AUTO_LIST, case-insensitive matcher
    completions: {
        case_sensitive: false
        quick:          true
        partial:        true
        algorithm:      "fuzzy"
        use_ls_colors:  true
    }

    cursor_shape: {
        emacs:     "line"
        vi_insert: "line"
        vi_normal: "block"
    }

    edit_mode:                        "vi"
    show_banner:                      false
    use_ansi_coloring:                true
    render_right_prompt_on_last_line: false

    table: {
        mode:       "rounded"
        index_mode: "always"
        show_empty: true
        padding:    { left: 1, right: 1 }
        trim: {
            methodology:             "wrapping"
            wrapping_try_keep_words: true
        }
    }

    # filesize.unit replaced filesize.metric + filesize.format in Nu 0.102.
    # "binary" = auto-scale using KiB/MiB/GiB (equivalent to old metric: false).
    # "metric" = auto-scale using kB/MB/GB. Other options: kB, KiB, MB, MiB, etc.
    filesize: {
        unit:      "binary"
        precision: 1
    }

    error_style: "fancy"
    footer_mode: 25   # integer — show footer when row count exceeds this
})

# ============================================================================
# Completions — carapace external completer
# Replaces zsh-completions + fzf-tab for the tab-completion use case.
# Provides structured completions for 500+ commands.
# Install: brew install carapace  |  nix-env -iA nixpkgs.carapace
# ============================================================================
if (which carapace | is-not-empty) {
    $env.config = ($env.config | merge deep {
        completions: {
            external: {
                enable:      true
                max_results: 100
                completer: {|spans|
                    carapace $spans.0 nushell ...$spans
                    | from json
                    | if ($in | default [] | where value =~ "^-.*ERR" | is-empty) { $in } else { null }
                }
            }
        }
    })
} else {
    print $"(ansi yellow)warn(ansi reset) carapace not found — tab completions limited"
    print "  brew install carapace  |  nix-env -iA nixpkgs.carapace"
}

$env.config = ($env.config | merge deep {
    menus: [
        {
            name:                   "completion_menu"
            only_buffer_difference: false
            marker:                 "| "
            type: {
                layout:      "columnar"
                columns:     4
                col_width:   20
                col_padding: 2
            }
            style: {
                text:                "green"
                selected_text:       { attr: "r" }
                description_text:    "yellow"
                match_text:          { attr: "u" }
                selected_match_text: { attr: "ur" }
            }
        }
        {
            name:                   "history_menu"
            only_buffer_difference: true
            marker:                 "? "
            type: {
                layout:    "list"
                page_size: 20
            }
            style: {
                text:             "green"
                selected_text:    { attr: "r" }
                description_text: "yellow"
            }
        }
        {
            name:                   "help_menu"
            only_buffer_difference: true
            marker:                 "? "
            type: {
                layout:           "description"
                columns:          4
                col_width:        20
                col_padding:      2
                selection_rows:   4
                description_rows: 10
            }
            style: {
                text:             "green"
                selected_text:    { attr: "r" }
                description_text: "yellow"
            }
        }
    ]
})

# ============================================================================
# Keybindings
# ============================================================================
$env.config = ($env.config | merge deep {
    keybindings: [

        # Ctrl-F — accept full autosuggestion (zsh-autosuggestions right-arrow)
        {
            name:     "history_hint_complete"
            modifier: "control"
            keycode:  "char_f"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "historyHintComplete" }
        }

        # Ctrl-Space — accept autosuggestion
        # Uses historyHintComplete rather than historyHintWordComplete for
        # compatibility across Nu versions.
        {
            name:     "accept_autosuggestion"
            modifier: "control"
            keycode:  "space"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "historyHintComplete" }
        }

        # Up / Down — history prefix search
        {
            name:     "history_up"
            modifier: "none"
            keycode:  "up"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "up" }
        }
        {
            name:     "history_down"
            modifier: "none"
            keycode:  "down"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "down" }
        }

        # Tab / Shift-Tab — completion menu
        {
            name:     "completion_menu_open"
            modifier: "none"
            keycode:  "tab"
            mode:     ["vi_insert" "emacs"]
            event: {
                until: [
                    { send: "menu" name: "completion_menu" }
                    { send: "menuNext" }
                ]
            }
        }
        {
            name:     "completion_menu_prev"
            modifier: "shift"
            keycode:  "backtab"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "menuPrevious" }
        }

        # Ctrl-R — history menu (atuin overrides this if vendor/atuin.nu is sourced)
        {
            name:     "history_menu"
            modifier: "control"
            keycode:  "char_r"
            mode:     ["vi_insert" "emacs" "vi_normal"]
            event:    { send: "menu" name: "history_menu" }
        }

        # Ctrl-T — fzf file picker (mirrors FZF_CTRL_T)
        {
            name:     "fzf_file_picker"
            modifier: "control"
            keycode:  "char_t"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "executehostcommand" cmd: "fzf-file-pick" }
        }

        # Alt-C — fzf directory picker and cd (mirrors FZF_ALT_C)
        {
            name:     "fzf_dir_picker"
            modifier: "alt"
            keycode:  "char_c"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "executehostcommand" cmd: "fzf-dir-pick" }
        }

        # Ctrl-B — fzf git branch switcher (^Gb in Zsh; no chained bindings in Nu)
        {
            name:     "fzf_git_branch"
            modifier: "control"
            keycode:  "char_b"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "executehostcommand" cmd: "fzf-git-branch" }
        }

        # Ctrl-G — fzf git log browser (^Gl in Zsh; Ctrl-L freed for clear screen)
        {
            name:     "fzf_git_log"
            modifier: "control"
            keycode:  "char_g"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "executehostcommand" cmd: "fzf-git-log" }
        }

        # Ctrl-K — fzf process killer
        {
            name:     "fzf_kill"
            modifier: "control"
            keycode:  "char_k"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "executehostcommand" cmd: "fzf-kill" }
        }

        # Ctrl-L — clear screen (unambiguous now that git-log moved to Ctrl-G)
        {
            name:     "clear_screen"
            modifier: "control"
            keycode:  "char_l"
            mode:     ["vi_insert" "emacs"]
            event:    { send: "clearScreen" }
        }

        # Alt-E — edit current line in $EDITOR (Ctrl-X Ctrl-E in Zsh)
        {
            name:     "edit_in_editor"
            modifier: "alt"
            keycode:  "char_e"
            mode:     ["vi_insert" "emacs" "vi_normal"]
            event:    { send: "openEditor" }
        }

        # F1 — help menu
        {
            name:     "help_menu"
            modifier: "none"
            keycode:  "f1"
            mode:     ["vi_insert" "emacs" "vi_normal"]
            event:    { send: "menu" name: "help_menu" }
        }
    ]
})

# ============================================================================
# Aliases
# Merged from 07-aliases.zsh + abbreviations.zsh. Entries that were
# abbreviation-only are marked [abbr].
#
# Rules:
#   • `alias foo = bar` works for commands and a small set of keywords
#     (if, match, try, overlay, cd, clear, exit, exec). It does NOT work
#     for `source` — reload is therefore `exec nu` (replaces the process).
#   • Nu builtins (rm, cp, mv, mkdir, ls) are shadowed with `^` to invoke
#     the system binary where flag differences matter (e.g. rm -iv).
#   • Commands requiring parameters are `def` commands in the Functions
#     section below.
# ============================================================================

# Session
alias reload = exec nu   # restart the shell, re-reading all config

# Safety — shadow Nu builtins with `-iv` flags via system binaries
alias rm    = ^rm -iv
alias cp    = ^cp -iv
alias mv    = ^mv -iv
alias ln    = ln -iv
alias mkdir = ^mkdir -pv
alias md    = ^mkdir -pv

# eza — Nu's builtin `ls` is preserved for structured pipeline use.
# These aliases provide eza's human-readable output under distinct names.
alias l   = eza --icons
alias la  = eza -lAh --icons
alias le  = eza -lah --icons
alias ll  = eza -lh --icons
alias lt  = eza --tree --level=2 --icons
alias lta = eza --tree --level=2 --icons -a

# Diff / network
alias diff = diff --color=auto
alias ip   = ip --color=auto

# Directory navigation  [abbr: ... / ....]
alias ".."    = cd ..
alias "..."   = cd ../..
alias "...."  = cd ../../..
alias "....." = cd ../../../..

# Editor
alias v   = nvim
alias vi  = nvim
alias vim = nvim
alias nv  = nvim

# Disk / system
alias df        = df -h
alias listening = lsof -iTCP -sTCP:LISTEN -n -P
alias weather   = curl wttr.in    # [abbr]
alias htop      = htop --sort-key=PERCENT_CPU

# Git
alias g    = git
alias ga   = git add
alias gaa  = git add --all
alias gb   = git branch
alias gc   = git commit
alias gcm  = git commit -m
alias gca  = git commit --amend
alias gcan = git commit --amend --no-edit
alias gco  = git checkout
alias gcb  = git checkout -b
alias gd   = git diff
alias gds  = git diff --staged
alias gf   = git fetch
alias gl   = git log --oneline --graph
alias gp   = git push
alias gpf  = git push --force-with-lease
alias gpl  = git pull
alias gr   = git rebase
alias gs   = git status
alias gst  = git stash
alias gstp = git stash pop

# Tmux
alias t  = tmux
alias ta = tmux attach
alias tl = tmux list-sessions
alias tn = tmux new-session -s

# Network
# Docker
alias docker-ps    = docker ps
alias docker-psa   = docker ps -a
alias docker-imgs  = docker images
alias docker-exec  = docker exec -it
alias docker-logs  = docker logs -f
alias docker-prune = docker system prune -a
alias dco          = docker compose

# Kubernetes
alias k    = kubectl
alias kg   = kubectl get
alias kd   = kubectl describe
alias kdel = kubectl delete
alias kl   = kubectl logs
alias kx   = kubectl exec -it

# Config editing (open in $EDITOR)
alias nuconfig = nvim ($nu.config-path)
alias nuenv    = nvim ($nu.env-path)
alias dotfiles = cd ($env.HOME | path join ".config")    # [abbr]

# History
alias h = history

# Misc  [abbr: cls]
alias c   = clear
alias cls = clear    # [abbr]
alias q   = exit

# ============================================================================
# Functions
# ============================================================================

# ── Config ───────────────────────────────────────────────────────────────────

# show-path — display PATH entries as a table
def show-path [] {
    $env.PATH | wrap path
}

# path-add — prepend a directory to PATH (no-op if already present or missing)
def --env path-add [dir: string] {
    if not ($dir | path exists) {
        print $"warn: '($dir)' does not exist"
        return
    }
    if not ($env.PATH | any { |p| $p == $dir }) {
        $env.PATH = ($env.PATH | prepend $dir)
    }
}

# ── Directory & file operations ───────────────────────────────────────────────

# mkcd — create a directory and cd into it
def --env mkcd [dir: string] {
    mkdir $dir
    cd $dir
}

# take — clone a repo and cd into it; or mkdir+cd for a plain path
def --env take [target: string] {
    if ($target =~ "^(git://|https://|git@)") {
        git clone $target
        cd ($target | path basename | str replace --regex '\.git$' "")
    } else {
        mkdir $target
        cd $target
    }
}

# up — go up N directories
def --env up [n: int = 1] {
    cd (1..$n | reduce --fold "." { |_, acc| $acc | path join ".." })
}

# backup — timestamped copy of a file or directory
def backup [src: path] {
    let dest = $"($src).backup.(date now | format date '%Y%m%d_%H%M%S')"
    ^cp -r $src $dest
    print $"Backup: ($dest)"
}

# extract — universal archive extractor
def extract [archive: path] {
    if not ($archive | path exists) {
        error make { msg: $"'($archive)' does not exist" }
    }
    let name = ($archive | into string)
    let ext  = ($archive | path parse | get extension)
    match $ext {
        "gz"   => { if ($name | str ends-with ".tar.gz")  { tar xzf $name } else { gunzip $name } }
        "bz2"  => { if ($name | str ends-with ".tar.bz2") { tar xjf $name } else { bunzip2 $name } }
        "xz"   => { tar xJf $name }
        "zst"  => { tar --zstd -xf $name }
        "tar"  => { tar xf $name }
        "tbz2" => { tar xjf $name }
        "tgz"  => { tar xzf $name }
        "zip"  => { unzip $name }
        "rar"  => { unrar x $name }
        "7z"   => { 7z x $name }
        "Z"    => { uncompress $name }
        "deb"  => { ar x $name }
        _      => { error make { msg: $"Don't know how to extract '($archive)'" } }
    }
}

# ── fzf interactive pickers ───────────────────────────────────────────────────
# Also invoked via keybindings defined above.

# fcd — fuzzy cd
def --env fcd [] {
    let dir = if (which fd | is-not-empty) {
        fd --type d --hidden --follow --exclude .git
        | fzf --preview 'eza --tree --level=1 --color=always --icons {} 2>/dev/null' --preview-window 'right:60%:wrap'
        | str trim
    } else {
        ^find . -type d -not -path "*/.git/*"
        | fzf --preview-window 'right:60%:wrap'
        | str trim
    }
    if ($dir | is-not-empty) { cd $dir }
}

# fopen — fuzzy file picker; opens selection in $EDITOR
def fopen [] {
    let file = if (which fd | is-not-empty) {
        fd --type f --hidden --follow --exclude .git
        | fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}' --preview-window 'right:60%:wrap'
        | str trim
    } else {
        ^find . -type f -not -path "*/.git/*"
        | fzf --preview-window 'right:60%:wrap'
        | str trim
    }
    if ($file | is-not-empty) { run-external $env.EDITOR $file }
}

# fzf-file-pick — Ctrl-T: insert a file path at the prompt
def fzf-file-pick [] {
    if (which fd | is-not-empty) {
        fd --type f --hidden --follow --exclude .git
        | fzf --height '80%' --layout reverse --preview 'bat -n --color=always {} 2>/dev/null || cat {}' --preview-window 'right:60%:wrap'
        | str trim
    } else {
        ^find . -type f -not -path "*/.git/*"
        | fzf --height '80%' --layout reverse
        | str trim
    }
}

# fzf-dir-pick — Alt-C: cd to a picked directory
def --env fzf-dir-pick [] {
    let dir = if (which fd | is-not-empty) {
        fd --type d --hidden --follow --exclude .git
        | fzf --height '80%' --layout reverse --preview 'eza --tree --level=2 --color=always --icons {} 2>/dev/null' --preview-window 'right:60%:wrap'
        | str trim
    } else {
        ^find . -type d -not -path "*/.git/*"
        | fzf --height '80%' --layout reverse
        | str trim
    }
    if ($dir | is-not-empty) { cd $dir }
}

# fzf-git-branch — Ctrl-B: switch git branch interactively
def fzf-git-branch [] {
    let branch = (
        git branch -a
        | lines
        | where { |l| not ($l =~ "HEAD") }
        | fzf --ansi --preview 'git log --oneline --graph --color=always {1}' --height '80%' --layout reverse
        | str trim
        | str replace --regex '^[* ]+' ''
        | str replace --regex '^remotes/[^/]+/' ''
    )
    if ($branch | is-not-empty) { git checkout $branch }
}

# fzf-git-log — Ctrl-G: browse git log interactively
# Single quotes throughout: % in --format and --height would parse as modulo.
def fzf-git-log [] {
    git log --graph --color=always --format='%C(auto)%h%d %s %C(black)%C(bold)%cr'
    | fzf --ansi --no-sort --reverse --tiebreak index --preview 'echo {} | grep -o "[a-f0-9]\{7\}" | head -1 | xargs git show --color=always' --bind 'enter:execute:echo {} | grep -o "[a-f0-9]\{7\}" | head -1 | xargs git show | less -R' --height '80%' --layout reverse
}

# fzf-kill — Ctrl-K: kill selected processes
def fzf-kill [] {
    let pids = (
        ^ps aux
        | fzf -m --preview-window 'down:3:wrap' --height '80%' --layout reverse
        | lines
        | each { |l| $l | split row --regex '\s+' | get 1 }
    )
    if ($pids | is-not-empty) {
        $pids | each { |pid| kill -9 $pid }
        print $"Killed: ($pids | str join ', ')"
    }
}

# ── Git utilities ─────────────────────────────────────────────────────────────

# git-clean-branches — delete local merged branches
def git-clean-branches [] {
    git branch --merged
    | lines
    | where { |l| not ($l =~ '^\*|main|master|develop') }
    | each { |b| git branch -d ($b | str trim) }
    print "Cleaned up merged branches."
}

# git-undo-commit — undo last commit, keep changes staged
def git-undo-commit [] {
    git reset --soft HEAD~1
    print "Last commit undone; changes kept staged."
}

# ── Network & system ──────────────────────────────────────────────────────────

# ports — listening TCP ports as a structured table
def ports [] {
    if (which lsof | is-not-empty) {
        lsof -iTCP -sTCP:LISTEN -n -P
        | from ssv --minimum-spaces 1
        | select COMMAND PID USER NAME
    } else {
        ss -tulanp
    }
}

# myip — public and local IP addresses
def myip [] {
    let public = (curl -s ifconfig.me | str trim)
    let local  = if $nu.os-info.name == "macos" {
        ipconfig getifaddr en0 | str trim
    } else {
        hostname -I | split row " " | first
    }
    { public: $public, local: $local }
}

# serve — local HTTP server in the current directory
def serve [port: int = 8000] {
    print $"Serving on http://localhost:($port)"
    python3 -m http.server $port
}

# ── File & text utilities ──────────────────────────────────────────────────────

# encode64 / decode64 — base64 helpers, pipeline-aware
def encode64 [file?: path] {
    if $file != null { open --raw $file | encode base64 } else { $in | encode base64 }
}
def decode64 [file?: path] {
    if $file != null { open --raw $file | decode base64 } else { $in | decode base64 }
}

# findtext — search file contents; returns a structured table via rg
def findtext [pattern: string, dir: path = .] {
    if (which rg | is-not-empty) {
        rg --json $pattern ($dir | into string)
        | lines
        | each { |l| $l | from json }
        | where type == "match"
        | each { |m| {
            file: $m.data.path.text
            line: $m.data.line_number
            text: ($m.data.lines.text | str trim)
        }}
    } else {
        grep -rnw ($dir | into string) -e $pattern --color always
    }
}

# replace — in-place string substitution using Nu's native str replace + save
def replace [search: string, replacement: string, file: path] {
    if not ($file | path exists) {
        error make { msg: $"'($file)' not found" }
    }
    open $file
    | str replace --all $search $replacement
    | save --force $file
    print $"Replaced '($search)' → '($replacement)' in ($file)"
}

# ── Process management ────────────────────────────────────────────────────────

# psgrep — search running processes by name; returns a structured table
def psgrep [pattern: string] {
    ps | where name =~ $pattern
}

# hg — search command history by pattern
def hg [pattern: string] {
    history | where command =~ $pattern
}

# killport — kill whatever is listening on a TCP port
def killport [port: int] {
    let pids = (lsof -ti $"tcp:($port)" | lines | str trim | where { |p| $p != "" })
    if ($pids | is-not-empty) {
        $pids | each { |pid| kill -9 $pid }
        print $"Killed PID(s) ($pids | str join ', ') on port ($port)"
    } else {
        print $"No process on port ($port)"
    }
}

# ── Disk & filesystem ─────────────────────────────────────────────────────────

# duh — immediate children sorted by size (Nu-native, no external du)
def duh [dir: path = .] {
    ls $dir
    | sort-by size --reverse
    | select name size type
}

# largest — N largest files under a directory
# Uses glob to avoid the eza alias that shadows Nu's `ls`.
def largest [n: int = 10, dir: path = .] {
    glob ($"($dir | into string)/**/*")
    | where { |p| ($p | path type) == "file" }
    | each { |p| { name: $p, size: ($p | path expand | ls $in | get size | first) } }
    | sort-by size --reverse
    | first $n
}

# ── History & session ─────────────────────────────────────────────────────────

# history-stats — top 20 most-used commands
def history-stats [] {
    history
    | get command
    | each { |cmd| $cmd | split row " " | first }
    | wrap command
    | group-by command
    | transpose command rows
    | each { |r| { command: $r.command, count: ($r.rows | length) } }
    | sort-by count --reverse
    | first 20
}

def check-tools [] {
    [
        [cmd          name          ];
        ["tmux"       "Tmux"        ]
        ["starship"   "Starship"    ]
        ["direnv"     "Direnv"      ]
        ["mise"       "Mise"        ]
        ["zoxide"     "Zoxide"      ]
        ["atuin"      "Atuin"       ]
        ["kubectl"    "kubectl"     ]
        ["docker"     "Docker"      ]
        ["go"         "Go"          ]
        ["pyenv"      "Pyenv"       ]
        ["rbenv"      "Rbenv"       ]
        ["fnm"        "fnm"         ]
        ["terraform"  "Terraform"   ]
        ["gh"         "GitHub CLI"  ]
        ["carapace"   "Carapace"    ]
        ["fzf"        "fzf"         ]
        ["fd"         "fd"          ]
        ["bat"        "bat"         ]
        ["eza"        "eza"         ]
        ["rg"         "ripgrep"     ]
        ["delta"      "delta"       ]
        ["nerdfetch"  "nerdfetch"   ]
    ]
    | each { |t| { tool: $t.name, installed: (which $t.cmd | is-not-empty) } }
}

# ============================================================================
# Integrations
# ============================================================================

# ── Tmux auto-attach ──────────────────────────────────────────────────────────
# Mirrors the `exec tmux` block from the original Zsh config.
# Guards: tmux available, not already inside tmux, not inside Emacs.
# new-session without -s creates an unnamed session (matches Zsh original).
if ((which tmux | is-not-empty) and ("TMUX" not-in $env) and ("INSIDE_EMACS" not-in $env)) {
    if (tmux has-session | complete | get exit_code) == 0 {
        tmux attach
    } else {
        exec tmux new-session
    }
}

# ── nerdfetch ─────────────────────────────────────────────────────────────────
# Fires at the start of every interactive Nu session — including inside tmux
# windows and panes. The TMUX guard is intentionally absent: each new tmux
# window or pane spawns a fresh Nu process with no inherited NERDFETCH_SHOWN,
# so the NERDFETCH_SHOWN guard alone is sufficient to prevent double-firing.
# The exec tmux attach path above naturally suppresses nerdfetch for existing-
# session attaches because the Nu process is replaced before reaching here.
if ((which nerdfetch | is-not-empty) and ("NERDFETCH_SHOWN" not-in $env)) {
    nerdfetch
    $env.NERDFETCH_SHOWN = "1"
}

# ── fnm — Fast Node Manager ───────────────────────────────────────────────────
# Uses --json output for clean structured parsing; no bash intermediary or
# generated file required.
if (which fnm | is-not-empty) {
    let fnm_exports = (fnm env --json | from json)
    if "PATH" in $fnm_exports {
        let fnm_paths = ($fnm_exports.PATH | split row ":" | where { |p| $p | is-not-empty })
        $env.PATH = ($fnm_paths | append $env.PATH | uniq)
    }
    $fnm_exports | reject --optional PATH | load-env
}

# ── asdf — fallback if mise is absent ────────────────────────────────────────
if (which mise | is-empty) and (which asdf | is-not-empty) {
    $env.PATH = (
        [
            ($env.HOME | path join ".asdf" "shims")
            ($env.HOME | path join ".asdf" "bin")
        ]
        | where { |p| $p | path exists }
        | append $env.PATH
        | uniq
    )
}

# ── direnv ────────────────────────────────────────────────────────────────────
# Fires on every PWD change (cd, z, etc.) — the correct trigger for direnv.
#
# REQUIREMENTS:
#   1. Run `direnv allow` in any directory with a new or changed .envrc before
#      direnv will process it. This is a direnv security requirement.
#
#   2. `.envrc` files using `use flake` require nix-direnv. Without it,
#      direnv will error with "command not found: flake".
#      Install nix-direnv:
#        nix-env -iA nixpkgs.nix-direnv          # standalone Nix
#        # or via home-manager:
#        programs.direnv.enable = true;
#        programs.direnv.nix-direnv.enable = true;
#      Then add to ~/.config/direnv/direnvrc:
#        source $HOME/.nix-profile/share/nix-direnv/direnvrc
#
#   3. Do NOT use `source_env_if_exists .venv/bin/activate.nu` in .envrc.
#      direnv runs in bash and cannot parse Nu syntax — this causes direnv
#      to fail before exporting PATH, so no flake packages become available.
#      Instead, use `PATH_add .venv/bin` in .envrc to prepend the venv bin
#      to PATH within direnv's context. This hook then merges it into Nu's
#      PATH list correctly.
#
#   4. DIRENV_LOG_FORMAT controls direnv's output. Set to "" to silence the
#      "direnv: loading .envrc" messages, or leave unset for default output.
if (which direnv | is-not-empty) {
    $env.config = ($env.config | merge deep {
        hooks: {
            env_change: {
                PWD: [
                    {|before, after|
                        # ── direnv ──────────────────────────────────────────
                        let out = (direnv export json | complete)
                        if $out.exit_code != 0 {
                            let msg = ($out.stderr | str trim)
                            if ($msg | is-not-empty) {
                                print $"(ansi yellow)direnv:(ansi reset) ($msg)"
                            }
                        } else if ($out.stdout | str trim | is-not-empty) {
                            let exports = ($out.stdout | from json)
                            if "PATH" in $exports {
                                let direnv_paths = (
                                    $exports.PATH
                                    | split row ":"
                                    | where { |p| $p | is-not-empty }
                                )
                                $env.PATH = (
                                    $direnv_paths
                                    | append $env.PATH
                                    | uniq
                                )
                            }
                            $exports
                            | reject --optional PATH
                            | load-env
                        }

                        # ── venv activation ─────────────────────────────────
                        # `overlay use` requires a parse-time constant path and
                        # cannot be used with a runtime variable. Instead we
                        # replicate what activate.nu does directly: set
                        # VIRTUAL_ENV and prepend the venv bin to PATH.
                        # This is equivalent in effect and works correctly in
                        # a hook closure.
                        let venv_bin = ($after | path join ".venv" "bin")
                        let venv_python = ($venv_bin | path join "python")
                        if ($venv_python | path exists) {
                            $env.VIRTUAL_ENV = ($after | path join ".venv")
                            $env.PATH = ($env.PATH | prepend $venv_bin | uniq)
                        } else if "VIRTUAL_ENV" in $env {
                            # Left a venv directory — remove its bin from PATH
                            # and unset VIRTUAL_ENV.
                            let old_bin = ($env.VIRTUAL_ENV | path join "bin")
                            $env.PATH = ($env.PATH | where { |p| $p != $old_bin })
                            hide-env VIRTUAL_ENV
                        }
                    }
                ]
            }
        }
    })
}

# ============================================================================
# Vendor
# Run these commands once after installing zoxide / atuin, then uncomment
# the source lines below. Re-run after upgrading either tool.
#
#   mkdir -p ~/.config/nushell/vendor
#   zoxide init nushell | save -f ~/.config/nushell/vendor/zoxide.nu
#   atuin init nu       | save -f ~/.config/nushell/vendor/atuin.nu
#
# zoxide provides the `z` and `zi` commands (smarter cd).
# atuin replaces the Ctrl-R history menu with its own UI.
# ============================================================================
# source vendor/zoxide.nu
# source vendor/atuin.nu

# Belt-and-suspenders: ensure banner is suppressed even if vendor files re-enable it.
$env.config.show_banner = false
