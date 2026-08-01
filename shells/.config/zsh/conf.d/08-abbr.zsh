# 08-abbr.zsh - zsh-abbr guard
#
# Abbreviations are declared in ~/.config/zsh/abbreviations.zsh.
# zsh-abbr reads that file directly as its persistent store
# (set via ABBR_USER_ABBREVIATIONS_FILE in 04-plugins.zsh).
#
# To edit abbreviations: update abbreviations.zsh and restart the shell.
# To list active abbreviations: abbr list

if ! command -v abbr &>/dev/null; then
  print -P "%F{yellow}warn:%f zsh-abbr not found — skipping abbreviations"
fi
