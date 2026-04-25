# plugins.xsh - Xontrib loading and per-xontrib configuration
# Location: ~/.config/xonsh/plugins.xsh
#
# Xontribs are installed via:
#   NixOS:  programs.xonsh.extraPackages (sets NIX_PYTHONPATH on the wrapper)
#   darwin: symlinkJoin + makeWrapper (sets PYTHONPATH on the wrapper)
#
# Both paths ensure the xontrib is on sys.path before this file runs,
# so the idiomatic `xontrib load` command works correctly.

import shutil

# ============================================================================
# xontrib-prompt-starship
# ============================================================================
if shutil.which('starship'):
    try:
        xontrib load prompt_starship
    except Exception as _e:
        print(f'\033[33mwarn:\033[0m xontrib-prompt-starship failed to load: {_e}')
        del _e

# ============================================================================
# fzf environment — used by keybindings.xsh widgets
# ============================================================================
if shutil.which('fzf'):
    $FZF_DEFAULT_OPTS = (
        '--cycle '
        '--info=inline '
        '--layout=reverse '
        '--height=80% '
        "--color='border:#aaaaaa,label:#cccccc' "
        "--color='list-border:#669966,list-label:#99cc99' "
        "--color='header-border:#6699cc,header-label:#99ccff' "
    )
    $FZF_DEFAULT_COMMAND = 'fd --type f --hidden --follow --exclude .git'
    $FZF_CTRL_T_COMMAND  = $FZF_DEFAULT_COMMAND
    $FZF_CTRL_T_OPTS = (
        "--preview='bat -n --color=always {} 2>/dev/null || cat {}' "
        '--preview-window=right:60%:wrap '
    )
    $FZF_ALT_C_COMMAND = 'fd --type d --hidden --follow --exclude .git'
    $FZF_ALT_C_OPTS = (
        "--preview='eza --tree --level=2 --color=always --icons {} 2>/dev/null || tree -C {} | head -100' "
        '--preview-window=right:60%:wrap '
        '--walker-skip=.git,node_modules,target,venv,.venv,__pycache__ '
    )

del shutil
