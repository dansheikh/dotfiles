# health.xsh - On-demand sanity checks
# Location: ~/.config/xonsh/health.xsh
#
# Registers the `xonsh-health` alias. Run it at any time to verify the shell
# environment is healthy. Never runs automatically at startup.
#
# Usage:
#   xonsh-health

import os
import shutil
import subprocess
import time

def _health_check():
    """Run all health checks and print a summary."""
    _ok   = '\033[32m✓\033[0m'
    _fail = '\033[31m✗\033[0m'
    _issues = []

    def _check(label, condition, fix=None):
        if condition:
            print(f'  {_ok} {label}')
        else:
            print(f'  {_fail} {label}' + (f'  →  {fix}' if fix else ''))
            _issues.append(label)

    print('\033[1mxonsh health check\033[0m')

    # Prompt
    _check('starship installed', shutil.which('starship') is not None, 'packages.nix')
    try:
        import xontrib.prompt_starship
        _check('xontrib-prompt-starship importable', True)
    except ImportError:
        _check('xontrib-prompt-starship importable', False,
               'check NIX_PYTHONPATH or PYTHONPATH in wrapper')

    # History
    _hist = __xonsh__.env.get('XONSH_HISTORY_FILE', '')
    _check(
        f'history file exists ({os.path.basename(_hist)})',
        bool(_hist) and os.path.isfile(_hist),
        'will be created on first command'
    )

    # Core tools
    for _tool, _fix in [
        ('tmux',    'packages.nix'),
        ('fzf',     'packages.nix'),
        ('fd',      'packages.nix'),
        ('bat',     'packages.nix'),
        ('eza',     'packages.nix'),
        ('rg',      'packages.nix'),
        ('direnv',  'packages.nix'),
        ('zoxide',  'packages.nix'),
        ('mise',    'packages.nix'),
        ('atuin',   'packages.nix'),
    ]:
        _check(f'{_tool} on PATH', shutil.which(_tool) is not None, _fix)

    # direnv functional
    if shutil.which('direnv'):
        _r = subprocess.run(['direnv', 'version'], capture_output=True, text=True)
        _check('direnv functional', _r.returncode == 0)

    # tmux-yank loaded
    if shutil.which('tmux'):
        _r = subprocess.run(['tmux', 'list-keys'], capture_output=True)
        _check(
            'tmux-yank loaded',
            b'yank' in _r.stdout,
            'prefix + I in tmux to install plugins'
        )

    # Traceback log location
    _logfile = __xonsh__.env.get('XONSH_TRACEBACK_LOGFILE', '')
    _check(
        f'traceback log configured ({_logfile})',
        bool(_logfile),
        'set $XONSH_TRACEBACK_LOGFILE in opts.xsh'
    )

    print()
    if _issues:
        print(f'  \033[33m{len(_issues)} issue(s) found\033[0m')
    else:
        print(f'  \033[32mAll checks passed\033[0m')

aliases['xonsh-health'] = lambda args, stdin=None: _health_check()

del os, shutil, subprocess, time
