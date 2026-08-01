# rc.xsh - Xonsh configuration entry point
# Location: ~/.config/xonsh/rc.xsh
#
# Orchestrates modular configuration. Modules load in dependency order:
#
#   env.xsh          XDG, PATH, exports
#   platform/        platform-specific PATH and env fixups
#   opts.xsh         $XONSH_* shell settings (sets $XONSH_DEBUG)
#   history.xsh      SQLite history backend and settings
#   plugins.xsh      xontrib loads and per-xontrib config
#   completions.xsh  completer registration
#   keybindings.xsh  prompt_toolkit key bindings
#   aliases.xsh      string and callable aliases
#   functions.xsh    utility functions
#   integrations.xsh external tools: tmux, eat, direnv, zoxide, mise, atuin
#   health.xsh       registers xonsh-health alias (on-demand only)
#   local.xsh        machine-local overrides (untracked — add to .gitignore)
#
# Each module is executed via execx() in an isolated namespace so that
# imports and locals in one module cannot pollute or clobber another.
# The live shell state (__xonsh__, aliases, $ENV vars) is shared automatically
# through xonsh's builtins — no namespace threading needed for that.
#
# Startup timing: set XONSH_DEBUG=1 in local.xsh to print per-module timings.

import os as _os
import time as _time

_xdg       = _os.environ.get('XDG_CONFIG_HOME', _os.path.expanduser('~/.config'))
_xonsh_dir = _os.path.join(_xdg, 'xonsh')
_debug     = _os.environ.get('XONSH_DEBUG', '0') == '1'
_t0        = _time.monotonic()

def _load(name):
    p = _os.path.join(_xonsh_dir, name)
    if _os.path.isfile(p):
        if _debug:
            _ts = _time.monotonic()
        execx(open(p).read(), 'exec', {}, filename=p)
        if _debug:
            print(f'\033[2m[xonsh] {name}: {((_time.monotonic() - _ts) * 1000):.0f}ms\033[0m')

_load('env.xsh')

# Platform split — after env so XDG_CONFIG_HOME is set
if __xonsh__.env.get('NIX_PROFILES') or _os.path.isdir('/run/current-system'):
    _load('platform/nixos.xsh')
elif _os.uname().sysname == 'Darwin':
    _load('platform/darwin.xsh')

_load('opts.xsh')
_load('history.xsh')
_load('plugins.xsh')
_load('completions.xsh')
_load('keybindings.xsh')
_load('aliases.xsh')
_load('functions.xsh')
_load('integrations.xsh')
_load('health.xsh')
_load('local.xsh')

if _debug:
    print(f'\033[2m[xonsh] total startup: {((_time.monotonic() - _t0) * 1000):.0f}ms\033[0m')

del _os, _time, _xdg, _xonsh_dir, _debug, _t0, _load
