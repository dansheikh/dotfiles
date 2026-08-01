# functions.xsh - Utility functions
# Location: ~/.config/xonsh/functions.xsh
#
# All functions are plain Python. Functions that change the shell's working
# directory must use os.chdir() — xonsh tracks $PWD from os.getcwd() after
# each command, so os.chdir() is the correct mechanism for cd-equivalent
# functions defined in Python.

import os
import shutil
import subprocess

# ============================================================================
# Directory & file operations
# ============================================================================

def mkcd(args):
    """Create a directory and cd into it. Usage: mkcd <directory>"""
    if len(args) != 2:
        print('Usage: mkcd <directory>')
        return 1
    d = args[1]
    os.makedirs(d, exist_ok=True)
    os.chdir(d)

aliases['mkcd'] = mkcd
del mkcd


def take(args):
    """Clone a git repo and cd into it; or mkdir+cd for a plain path.
    Usage: take <git-url|directory>"""
    if len(args) != 2:
        print('Usage: take <git-url|directory>')
        return 1
    target = args[1]
    if any(target.startswith(p) for p in ('git://', 'https://', 'git@')):
        subprocess.run(['git', 'clone', target], check=True)
        name = os.path.basename(target)
        if name.endswith('.git'):
            name = name[:-4]
        os.chdir(name)
    else:
        os.makedirs(target, exist_ok=True)
        os.chdir(target)

aliases['take'] = take
del take


def backup(args):
    """Timestamped copy of a file or directory. Usage: backup <path>"""
    if len(args) != 2:
        print('Usage: backup <path>')
        return 1
    from datetime import datetime
    src = args[1]
    if not os.path.exists(src):
        print(f"Error: '{src}' does not exist")
        return 1
    ts = datetime.now().strftime('%Y%m%d_%H%M%S')
    dest = f'{src}.backup.{ts}'
    if os.path.isdir(src):
        shutil.copytree(src, dest)
    else:
        shutil.copy2(src, dest)
    print(f'Backup created: {dest}')

aliases['backup'] = backup
del backup


def extract(args):
    """Universal archive extractor. Usage: extract <archive>"""
    if len(args) != 2:
        print('Usage: extract <archive>')
        return 1
    f = args[1]
    if not os.path.isfile(f):
        print(f"Error: '{f}' is not a file")
        return 1
    ext_map = {
        '.tar.bz2':  ['tar', 'xjf'],
        '.tar.gz':   ['tar', 'xzf'],
        '.tar.xz':   ['tar', 'xJf'],
        '.tar.zst':  ['tar', '--zstd', '-xf'],
        '.tar':      ['tar', 'xf'],
        '.tbz2':     ['tar', 'xjf'],
        '.tgz':      ['tar', 'xzf'],
        '.bz2':      ['bunzip2'],
        '.gz':       ['gunzip'],
        '.zip':      ['unzip'],
        '.rar':      ['unrar', 'x'],
        '.7z':       ['7z', 'x'],
        '.Z':        ['uncompress'],
        '.deb':      ['ar', 'x'],
        '.zst':      ['zstd', '-d'],
    }
    for suffix, cmd in ext_map.items():
        if f.endswith(suffix):
            subprocess.run(cmd + [f], check=True)
            return
    print(f"Error: don't know how to extract '{f}'")
    return 1

aliases['extract'] = extract
del extract

# ============================================================================
# Search & navigation
# ============================================================================

def fcd(args):
    """Fuzzy cd. Usage: fcd"""
    if shutil.which('fd'):
        result = subprocess.run(
            ['bash', '-c',
             'fd --type d --hidden --follow --exclude .git '
             '| fzf --preview "eza --tree --level=1 --color=always --icons {} 2>/dev/null '
             '|| tree -C -L 1 {} | head -100" --preview-window right:60%:wrap'],
            capture_output=True, text=True
        )
    else:
        result = subprocess.run(
            ['bash', '-c',
             'find . -type d -not -path "*/.git/*" '
             '| fzf --preview-window right:60%:wrap'],
            capture_output=True, text=True
        )
    d = result.stdout.strip()
    if d:
        os.chdir(d)

aliases['fcd'] = fcd
del fcd


def fopen(args):
    """Fuzzy find and open in $EDITOR. Usage: fopen"""
    if shutil.which('fd'):
        result = subprocess.run(
            ['bash', '-c',
             'fd --type f --hidden --follow --exclude .git '
             '| fzf --preview "bat --color=always --style=numbers --line-range=:500 {}" '
             '--preview-window right:60%:wrap'],
            capture_output=True, text=True
        )
    else:
        result = subprocess.run(
            ['bash', '-c',
             'find . -type f -not -path "*/.git/*" | fzf --preview-window right:60%:wrap'],
            capture_output=True, text=True
        )
    f = result.stdout.strip()
    if f:
        editor = os.environ.get('EDITOR', 'nvim')
        subprocess.run([editor, f])

aliases['fopen'] = fopen
del fopen

# ============================================================================
# Git utilities
# ============================================================================

def git_clean_branches(args):
    """Delete local merged branches."""
    result = subprocess.run(
        ['git', 'branch', '--merged'], capture_output=True, text=True
    )
    branches = [
        b.strip() for b in result.stdout.splitlines()
        if b.strip() and not any(b.strip().startswith(p) for p in ('*', 'main', 'master', 'develop'))
    ]
    for b in branches:
        subprocess.run(['git', 'branch', '-d', b])
    print('Cleaned up merged branches.')

aliases['git-clean-branches'] = git_clean_branches
del git_clean_branches


def git_undo_commit(args):
    """Undo last commit, keep changes staged."""
    subprocess.run(['git', 'reset', '--soft', 'HEAD~1'])
    print('Last commit undone; changes kept in staging.')

aliases['git-undo-commit'] = git_undo_commit
del git_undo_commit

# ============================================================================
# Network & system
# ============================================================================

def ports(args):
    """Show listening TCP ports."""
    if shutil.which('lsof'):
        subprocess.run(['lsof', '-iTCP', '-sTCP:LISTEN', '-n', '-P'])
    else:
        subprocess.run(['ss', '-tulanp'])

aliases['ports'] = ports
del ports


def myip(args):
    """Show public and local IP addresses."""
    import platform
    public = subprocess.run(
        ['curl', '-s', 'ifconfig.me'], capture_output=True, text=True
    ).stdout.strip()
    if platform.system() == 'Darwin':
        local = subprocess.run(
            ['ipconfig', 'getifaddr', 'en0'], capture_output=True, text=True
        ).stdout.strip()
    else:
        local = subprocess.run(
            ['hostname', '-I'], capture_output=True, text=True
        ).stdout.strip().split()[0]
    print(f'Public IP:  {public}')
    print(f'Local IP:   {local}')

aliases['myip'] = myip
del myip

# ============================================================================
# Development utilities
# ============================================================================

def serve(args):
    """Start a local HTTP server. Usage: serve [port]"""
    port = int(args[1]) if len(args) > 1 else 8000
    print(f'Serving on http://localhost:{port}')
    subprocess.run(['python3', '-m', 'http.server', str(port)])

aliases['serve'] = serve
del serve


def encode64(args):
    """Base64 encode a file or stdin. Usage: encode64 [file]"""
    import base64
    if len(args) > 1:
        with open(args[1], 'rb') as fh:
            print(base64.b64encode(fh.read()).decode())
    else:
        import sys
        print(base64.b64encode(sys.stdin.buffer.read()).decode())

aliases['encode64'] = encode64
del encode64


def decode64(args):
    """Base64 decode a file or stdin. Usage: decode64 [file]"""
    import base64, sys
    if len(args) > 1:
        with open(args[1], 'rb') as fh:
            sys.stdout.buffer.write(base64.b64decode(fh.read()))
    else:
        sys.stdout.buffer.write(base64.b64decode(sys.stdin.buffer.read()))

aliases['decode64'] = decode64
del decode64

# ============================================================================
# File content
# ============================================================================

def findtext(args):
    """Search for text in files. Usage: findtext <pattern> [directory]"""
    if len(args) < 2:
        print('Usage: findtext <pattern> [directory]')
        return 1
    pattern = args[1]
    directory = args[2] if len(args) > 2 else '.'
    if shutil.which('rg'):
        subprocess.run(['rg', '--color=always', '--line-number', pattern, directory])
    else:
        subprocess.run(['grep', '-rnw', directory, '-e', pattern, '--color=always'])

aliases['findtext'] = findtext
del findtext


def replace(args):
    """In-place text substitution. Usage: replace <search> <replacement> <file>"""
    if len(args) != 4:
        print('Usage: replace <search> <replacement> <file>')
        return 1
    search, replacement, filepath = args[1], args[2], args[3]
    if not os.path.isfile(filepath):
        print(f"Error: '{filepath}' not found")
        return 1
    with open(filepath, 'r') as fh:
        content = fh.read()
    with open(filepath, 'w') as fh:
        fh.write(content.replace(search, replacement))
    print(f"Replaced '{search}' with '{replacement}' in {filepath}")

aliases['replace'] = replace
del replace

# ============================================================================
# Process management
# ============================================================================

def psgrep(args):
    """Search running processes. Usage: psgrep <name>"""
    if len(args) < 2:
        print('Usage: psgrep <name>')
        return 1
    pattern = ' '.join(args[1:])
    result = subprocess.run(
        ['ps', 'aux'], capture_output=True, text=True
    )
    lines = [l for l in result.stdout.splitlines()
             if pattern.lower() in l.lower() and 'grep' not in l]
    if lines:
        print(result.stdout.splitlines()[0])  # header
        print('\n'.join(lines))

aliases['psgrep'] = psgrep
del psgrep


def killport(args):
    """Kill whatever is listening on a port. Usage: killport <port>"""
    if len(args) != 2:
        print('Usage: killport <port>')
        return 1
    port = args[1]
    result = subprocess.run(
        ['lsof', f'-ti', f'tcp:{port}'], capture_output=True, text=True
    )
    pids = result.stdout.strip().split()
    if pids:
        subprocess.run(['kill', '-9'] + pids)
        print(f"Killed PID(s) {', '.join(pids)} on port {port}")
    else:
        print(f'No process on port {port}')

aliases['killport'] = killport
del killport

# ============================================================================
# Disk & system info
# ============================================================================

def duh(args):
    """Disk usage sorted by size. Usage: duh [directory]"""
    d = args[1] if len(args) > 1 else '.'
    subprocess.run(['du', '-h', '--max-depth=1', d],
                   env={**os.environ, 'LC_ALL': 'C'})
    # Note: sort not available on all platforms with -h; pipe manually
    result = subprocess.run(
        ['du', '-h', '--max-depth=1', d], capture_output=True, text=True
    )
    lines = sorted(result.stdout.splitlines(),
                   key=lambda l: l.split('\t')[0], reverse=True)
    print('\n'.join(lines))

aliases['duh'] = duh
del duh


def largest(args):
    """N largest files under current directory. Usage: largest [n]"""
    import glob
    n = int(args[1]) if len(args) > 1 else 10
    cwd = os.getcwd()
    files = []
    for f in glob.glob(os.path.join(cwd, '**', '*'), recursive=True):
        if os.path.isfile(f):
            try:
                files.append((os.path.getsize(f), f))
            except OSError:
                pass
    files.sort(reverse=True)
    for size, path in files[:n]:
        print(f'{size:>12,}  {path}')

aliases['largest'] = largest
del largest

# ============================================================================
# PATH management
# ============================================================================

def path_add(args):
    """Prepend a directory to PATH, no duplicates. Usage: path-add <directory>"""
    if len(args) != 2:
        print('Usage: path-add <directory>')
        return 1
    d = args[1]
    if not os.path.isdir(d):
        print(f"Warning: '{d}' is not a directory")
        return 1
    if d not in $PATH:
        $PATH.insert(0, d)

aliases['path-add'] = path_add
del path_add

# ============================================================================
# Quick helpers
# ============================================================================

def up(args):
    """Go up N directories. Usage: up [n]"""
    n = int(args[1]) if len(args) > 1 else 1
    target = os.path.normpath(os.path.join(os.getcwd(), *(['..'] * n)))
    os.chdir(target)

aliases['up'] = up
del up


def history_stats(args):
    """Top 20 commands by frequency."""
    from collections import Counter
    items = __xonsh__.history.all_items() if hasattr(__xonsh__, 'history') else []
    cmds = [item.get('inp', '').strip().split()[0]
            for item in items if item.get('inp', '').strip()]
    top = Counter(cmds).most_common(20)
    for rank, (cmd, count) in enumerate(top, 1):
        print(f'{rank:>3}  {count:>6}  {cmd}')

aliases['history-stats'] = history_stats
del history_stats


def check_tools(args):
    """Report which integrated tools are installed."""
    tools = [
        ('tmux',      'Tmux'),
        ('starship',  'Starship'),
        ('direnv',    'Direnv'),
        ('mise',      'Mise'),
        ('zoxide',    'Zoxide'),
        ('atuin',     'Atuin'),
        ('kubectl',   'Kubernetes'),
        ('docker',    'Docker'),
        ('go',        'Go'),
        ('pyenv',     'Pyenv'),
        ('rbenv',     'Rbenv'),
        ('fnm',       'fnm'),
        ('terraform', 'Terraform'),
        ('gh',        'GitHub CLI'),
        ('fzf',       'fzf'),
        ('fd',        'fd'),
        ('bat',       'bat'),
        ('eza',       'eza'),
        ('rg',        'ripgrep'),
        ('delta',     'delta'),
        ('nerdfetch', 'nerdfetch'),
    ]
    print('Integrated tools:')
    for cmd, name in tools:
        mark = '\033[32m✓\033[0m' if shutil.which(cmd) else '\033[31m✗\033[0m'
        print(f'  {mark} {name}')

aliases['check-tools'] = check_tools
del check_tools

del os, shutil, subprocess
