# abbreviations.zsh - Declarative abbreviation definitions
#
# This file is NOT sourced directly by zsh. It is imported into the
# zsh-abbr persistent store via `abbr import file` in 08-abbr.zsh,
# and only re-imported when this file is newer than the store.
#
# Syntax: abbr <name>="<expansion>"
# To manually reimport: abbr import file ~/.config/zsh/abbreviations.zsh

# ============================================================================
# Navigation & listing
# ============================================================================
abbr l="eza -lah --icons=always"
abbr la="eza -lAh --icons=always"
abbr ll="eza -lh --icons=always"
abbr ls="eza --icons=always"
abbr lt="eza --tree --level=2 --icons=always"
abbr lta="eza --tree --level=2 --icons=always -a"
abbr -- -="cd -"
abbr ...="cd ../.."
abbr ....="cd ../../.."

# ============================================================================
# Git
# ============================================================================
abbr ga="git add"
abbr gaa="git add --all"
abbr gb="git branch"
abbr gc="git commit"
abbr gcm="git commit -m"
abbr gca="git commit --amend"
abbr gcan="git commit --amend --no-edit"
abbr gco="git checkout"
abbr gcb="git checkout -b"
abbr gd="git diff"
abbr gds="git diff --staged"
abbr gf="git fetch"
abbr gl="git log --oneline --graph"
abbr gp="git push"
abbr gpf="git push --force-with-lease"
abbr gpl="git pull"
abbr gr="git rebase"
abbr gs="git status"
abbr gst="git stash"
abbr gstp="git stash pop"

# ============================================================================
# Editor
# ============================================================================
abbr v="nvim"
abbr vim="nvim"

# ============================================================================
# Docker
# ============================================================================
abbr d="docker"
abbr dc="docker compose"
abbr dps="docker ps"
abbr dpsa="docker ps -a"
abbr dex="docker exec -it"
abbr dlogs="docker logs -f"
abbr dprune="docker system prune -a"

# ============================================================================
# Config shortcuts
# ============================================================================
abbr zshrc="nvim ${ZDOTDIR:-~/.config/zsh}/.zshrc"
abbr zshenv="nvim ~/.zshenv"

# ============================================================================
# Utilities
# ============================================================================
abbr mkdir="mkdir -p"
abbr weather="curl wttr.in"
abbr reload="source ${ZDOTDIR:-~/.config/zsh}/.zshrc"
abbr dotfiles="cd ~/.config"
abbr cls="clear"
