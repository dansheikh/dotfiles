# .zshrc - Main Zsh configuration file
# Location: ~/.config/zsh/.zshrc
#
# Orchestrates modular configuration from conf.d/.
# Modules load in numerical order; numbering encodes dependency:
#
#   01-options        shell behaviour
#   02-completion     zstyles (no compinit yet)
#   03-history        history opts and keybindings
#   04-plugins        antidote + plugin config (adds to fpath)
#   05-compinit       compinit — runs after plugins extend fpath
#   06-fzf            fzf opts, keybindings, custom widgets
#   07-aliases        command aliases
#   08-abbr           zsh-abbr abbreviations
#   09-functions      utility functions
#   10-integrations   external tools (starship, direnv, zoxide, …)

_zdir=${ZDOTDIR:-~/.config/zsh}

for _f in ${_zdir}/conf.d/*.zsh(N); do
  source ${_f}
done
unset _f _zdir

# Machine-local overrides (untracked)
[[ -f ${ZDOTDIR:-~/.config/zsh}/.zshrc.local ]] \
  && source ${ZDOTDIR:-~/.config/zsh}/.zshrc.local
