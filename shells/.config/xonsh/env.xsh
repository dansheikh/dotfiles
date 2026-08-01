# env.xsh - Environment variables and PATH
# Location: ~/.config/xonsh/env.xsh
#
# Responsible for everything that must be visible to all processes spawned
# from this shell. Platform-specific PATH augmentation lives in platform/.
#
# Loaded first by rc.xsh so all subsequent modules can rely on these values.
#
# IMPORTANT: This file must use only pure Python — no xonsh $() subprocess
# syntax and no $VAR interpolation inside Python expressions. The shell is
# not fully initialised when env.xsh runs. Use __xonsh__.env for all reads
# and writes to environment variables.

import os
import shutil

_env = __xonsh__.env

# ============================================================================
# XDG Base Directory Specification
# ============================================================================
_env['XDG_CONFIG_HOME'] = os.path.expanduser('~/.config')
_env['XDG_DATA_HOME']   = os.path.expanduser('~/.local/share')
_env['XDG_STATE_HOME']  = os.path.expanduser('~/.local/state')
_env['XDG_CACHE_HOME']  = os.path.expanduser('~/.cache')

# ============================================================================
# Editor
# ============================================================================
_env['EDITOR'] = 'nvim'
_env['VISUAL'] = 'nvim'

# ============================================================================
# Starship
# ============================================================================
_env['STARSHIP_CONFIG'] = os.path.join(
    _env['XDG_CONFIG_HOME'], 'starship', 'starship.toml'
)

# ============================================================================
# History file location
# History backend settings live in history.xsh.
# ============================================================================
_hist_file = os.path.join(_env['XDG_DATA_HOME'], 'xonsh', 'xonsh-history.sqlite')
_env['XONSH_HISTORY_FILE'] = _hist_file
os.makedirs(os.path.dirname(_hist_file), exist_ok=True)
del _hist_file

# ============================================================================
# Bat
# ============================================================================
_env['BAT_PAGER'] = 'less -KFR --mouse'

# ============================================================================
# NPM
# ============================================================================
_env['NPM_CONFIG_PREFIX'] = os.path.expanduser('~/.npm-packages')

# ============================================================================
# PATH — personal and tool candidate directories
#
# Read and write PATH via _env directly — avoids EnvPath type errors that
# occur when $PATH is used inside Python expression context during early init.
# ============================================================================
_path_candidates = [
    os.path.expanduser('~/.local/bin'),
    os.path.expanduser('~/.npm-packages/bin'),
    os.path.expanduser('~/.cargo/bin'),
    os.path.expanduser('~/.bun/bin'),
    os.path.expanduser('~/.deno/bin'),
    os.path.join(os.path.expanduser('~'), 'go', 'bin'),
    os.path.expanduser('~/.pyenv/shims'),
    os.path.expanduser('~/.pyenv/bin'),
    os.path.expanduser('~/.rbenv/shims'),
    os.path.join(os.path.expanduser('~'), 'google-cloud-sdk', 'bin'),
]

_current_path = list(_env.get('PATH', []))
for _p in _path_candidates:
    if os.path.isdir(_p) and _p not in _current_path:
        _current_path.insert(0, _p)

_env['PATH'] = _current_path
del _path_candidates, _current_path, _p

# ============================================================================
# Go
# ============================================================================
if shutil.which('go'):
    _env['GOPATH'] = os.path.join(os.path.expanduser('~'), 'go')
    _env["GOBIN"]  = os.path.join(os.path.expanduser("~"), "go", "bin")

# ============================================================================
# Bun
# ============================================================================
_bun_dir = os.path.expanduser('~/.bun')
if os.path.isdir(_bun_dir):
    _env['BUN_INSTALL'] = _bun_dir
del _bun_dir

# ============================================================================
# Pyenv
# ============================================================================
_pyenv_root = os.path.expanduser('~/.pyenv')
if os.path.isdir(_pyenv_root):
    _env['PYENV_ROOT'] = _pyenv_root
del _pyenv_root

