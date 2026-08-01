# 05-compinit.zsh - Initialize the completion system
#
# Runs AFTER 04-plugins.zsh so that all plugin fpath additions are visible.
# Uses a 24-hour cache check to avoid rebuilding the dump on every startup.

# Ensure the cache directory exists
_zcompdump="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompdump"
[[ -d ${_zcompdump:h} ]] || mkdir -p ${_zcompdump:h}

autoload -Uz compinit

# Rebuild dump only if it is older than 24 hours or missing
if [[ -n ${_zcompdump}(#qN.mh+24) ]]; then
  compinit -d "${_zcompdump}"
else
  # -C skips the security check and avoids regenerating the dump
  compinit -C -d "${_zcompdump}"
fi

unset _zcompdump

# Flush any compdef calls captured during plugin initialization
if (( ${#_deferred_compdefs} )); then
  for cd_args in "${_deferred_compdefs[@]}"; do
    eval "compdef $cd_args"
  done
  unset _deferred_compdefs
fi
