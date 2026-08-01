# aliases.xsh - Aliases
# Location: ~/.config/xonsh/aliases.xsh
#
# String aliases expand inline at execution time. Python callable aliases
# are used where behaviour requires access to arguments, environment, or
# Python APIs — the xonsh idiom that replaces both zsh aliases and
# abbreviations.
#
# Callable alias signature: def name(args, stdin=None) -> str | None
# args is a list of strings (the full argv including the alias name).

import shutil
import os

# ============================================================================
# Safety
# ============================================================================
aliases['rm']    = 'rm -iv'
aliases['cp']    = 'cp -iv'
aliases['mv']    = 'mv -iv'
aliases['ln']    = 'ln -iv'
aliases['mkdir'] = 'mkdir -pv'
aliases['md']    = 'mkdir -pv'

# ============================================================================
# ls / eza
# ============================================================================
if shutil.which('eza'):
    aliases['ls']  = 'eza --icons auto'
    aliases['la']  = 'eza -lAh --icons auto'
    aliases['ll']  = 'eza -lh --icons auto'
    aliases['lt']  = 'eza --tree --level=2 --icons auto'
    aliases['lta'] = 'eza --tree --level=2 --icons auto -a'
    aliases['l']   = 'eza -lah --icons auto'
else:
    aliases['ls']  = 'ls --color=auto'
    aliases['la']  = 'ls -lAh --color=auto'
    aliases['ll']  = 'ls -lh --color=auto'
    aliases['l']   = 'ls -lah --color=auto'

# ============================================================================
# Diff / network
# ============================================================================
aliases['diff'] = 'diff --color=auto'
aliases['ip']   = 'ip --color=auto'

# ============================================================================
# Directory navigation
#
# Note: 'cd ..', 'cd ...', 'cd ....' etc. work natively in xonsh since
# the cd command runs in subprocess mode and passes the path string directly.
# No dot aliases needed — use cd .. / cd ... / cd .... directly.
# ============================================================================
aliases['-'] = 'cd -'

# ============================================================================
# Editor
# ============================================================================
if shutil.which('nvim'):
    aliases['v']   = 'nvim'
    aliases['vi']  = 'nvim'
    aliases['vim'] = 'nvim'
    aliases['nv']  = 'nvim'

# ============================================================================
# Disk / system
# ============================================================================
aliases['df']        = 'df -h'
aliases['listening'] = 'lsof -iTCP -sTCP:LISTEN -n -P'
aliases['weather']   = 'curl wttr.in'
aliases['htop']      = 'htop --sort-key=PERCENT_CPU'

# ============================================================================
# Process management
# ============================================================================
aliases['psg'] = 'ps aux | grep -v grep | grep -i -e VSZ -e'

# ============================================================================
# Git
# ============================================================================
aliases['g']     = 'git'
aliases['ga']    = 'git add'
aliases['gaa']   = 'git add --all'
aliases['gb']    = 'git branch'
aliases['gc']    = 'git commit'
aliases['gcm']   = 'git commit -m'
aliases['gca']   = 'git commit --amend'
aliases['gcan']  = 'git commit --amend --no-edit'
aliases['gco']   = 'git checkout'
aliases['gcb']   = 'git checkout -b'
aliases['gd']    = 'git diff'
aliases['gds']   = 'git diff --staged'
aliases['gf']    = 'git fetch'
aliases['gl']    = 'git log --oneline --graph'
aliases['gp']    = 'git push'
aliases['gpf']   = 'git push --force-with-lease'
aliases['gpl']   = 'git pull'
aliases['gr']    = 'git rebase'
aliases['gs']    = 'git status'
aliases['gst']   = 'git stash'
aliases['gstp']  = 'git stash pop'
aliases['gdiff'] = 'git diff --color'

# ============================================================================
# Tmux
# ============================================================================
aliases['t']  = 'tmux'
aliases['ta'] = 'tmux attach'
aliases['tl'] = 'tmux list-sessions'
aliases['tn'] = 'tmux new-session -s'

# ============================================================================
# Docker
# ============================================================================
if shutil.which('docker'):
    aliases['d']      = 'docker'
    aliases['dc']     = 'docker compose'
    aliases['dps']    = 'docker ps'
    aliases['dpsa']   = 'docker ps -a'
    aliases['di']     = 'docker images'
    aliases['dex']    = 'docker exec -it'
    aliases['dlogs']  = 'docker logs -f'
    aliases['dprune'] = 'docker system prune -a'

    def _dstop(args, stdin=None):
        """Stop all running containers."""
        running = $(docker ps -q).strip()
        if running:
            docker stop @(running.split())
        else:
            print('No running containers.')
    aliases['dstop'] = _dstop
    del _dstop

# ============================================================================
# Kubernetes
# ============================================================================
if shutil.which('kubectl'):
    aliases['k']    = 'kubectl'
    aliases['kg']   = 'kubectl get'
    aliases['kd']   = 'kubectl describe'
    aliases['kdel'] = 'kubectl delete'
    aliases['kl']   = 'kubectl logs'
    aliases['kx']   = 'kubectl exec -it'

# ============================================================================
# History
# ============================================================================
aliases['h']  = 'history'

def _histgrep(args, stdin=None):
    """Search history. Usage: histgrep <pattern>
    Renamed from hg to avoid shadowing the Mercurial hg command."""
    if len(args) < 2:
        print('Usage: histgrep <pattern>')
        return
    pattern = args[1]
    history | grep -i @(pattern)
aliases['histgrep'] = _histgrep
del _histgrep

# ============================================================================
# Misc
# ============================================================================
aliases['j']    = 'jobs'
aliases['c']    = 'clear'
aliases['cls']  = 'clear'
aliases[':q']   = 'exit'
aliases['q']    = 'exit'

def _path_alias(args, stdin=None):
    """Print PATH entries one per line."""
    for p in $PATH:
        print(p)
aliases['path'] = _path_alias
del _path_alias

aliases['dotfiles'] = f'cd {os.path.expanduser("~/.config")}'

# ============================================================================
# Config editing — callable so $EDITOR is evaluated at call time
# ============================================================================
def _xonshconfig(args, stdin=None):
    xdg = $XDG_CONFIG_HOME if 'XDG_CONFIG_HOME' in ${...} else os.path.expanduser('~/.config')
    rc = os.path.join(xdg, 'xonsh', 'rc.xsh')
    $EDITOR @(rc)
aliases['xonshconfig'] = _xonshconfig
del _xonshconfig

def _zreload(args, stdin=None):
    """Restart xonsh, re-reading all config."""
    import sys
    os.execv(sys.executable, [sys.executable] + sys.argv)
aliases['reload'] = _zreload
del _zreload

del shutil, os
