# .zshrc - Main Zsh configuration file
# Location: ~/.config/zsh/.zshrc

# Source asdf-vm shell integration (AUR package) before modules/compinit
if [[ -f /opt/asdf-vm/asdf.sh ]]; then
  . /opt/asdf-vm/asdf.sh
  [[ -d "${ASDF_DATA_DIR}/completions" ]] && fpath=("${ASDF_DATA_DIR}/completions" $fpath)
fi

# Orchestrates modular configuration from conf.d/
_zdir=${ZDOTDIR:-~/.config/zsh}

for _f in ${_zdir}/conf.d/*.zsh(N); do
  source "${_f}"
done
unset _f _zdir

# Machine-local overrides (untracked)
[[ -f ${ZDOTDIR:-~/.config/zsh}/.zshrc.local ]] \
  && source "${ZDOTDIR:-~/.config/zsh}/.zshrc.local"
