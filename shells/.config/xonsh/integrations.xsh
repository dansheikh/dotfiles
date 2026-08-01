# integrations.xsh - External tool integrations
# Location: ~/.config/xonsh/integrations.xsh
#
# Handles runtime integration of external tools:
#   - tmux auto-attach (with eat/vterm guard)
#   - eat terminal shell integration via event hooks
#   - direnv via @events.on_chdir (native Python, no xontrib)
#   - zoxide
#   - mise (with asdf fallback)
#   - atuin
#   - fnm
#   - pyenv / rbenv
#   - GitHub CLI completion
#   - nerdfetch

import os
import shutil
import subprocess

# ============================================================================
# Tmux auto-attach
#
# Guards:
#   - tmux must be installed
#   - not already inside tmux ($TMUX unset)
#   - not inside Emacs ($INSIDE_EMACS unset)
#   - not inside eat ($EAT_SHELL_INTEGRATION_DIR unset)
#   - not a dumb terminal
# ============================================================================
_in_tmux   = 'TMUX' in ${...}
_in_emacs  = 'INSIDE_EMACS' in ${...}
_in_eat    = 'EAT_SHELL_INTEGRATION_DIR' in ${...}
_dumb_term = ${...}.get('TERM', '') == 'dumb'

if (shutil.which('tmux')
        and not _in_tmux
        and not _in_emacs
        and not _in_eat
        and not _dumb_term):
    _has_session = subprocess.run(
        ['tmux', 'has-session'],
        capture_output=True
    ).returncode == 0
    if _has_session:
        os.execvp('tmux', ['tmux', 'attach'])
    else:
        os.execvp('tmux', ['tmux', 'new-session'])

del _in_tmux, _in_emacs, _in_eat, _dumb_term

# ============================================================================
# eat terminal shell integration
#
# eat supports shell integration via escape sequences that report:
#   - directory changes (OSC 7)
#   - command start/end with exit code (OSC 133)
#
# Xonsh doesn't have first-class eat support; we implement the protocol
# using xonsh event hooks. This enables eat's directory tracking,
# prompt annotations, and shell-command integration.
# ============================================================================
if 'EAT_SHELL_INTEGRATION_DIR' in ${...} or ${...}.get('TERM', '').startswith('eat'):

    @events.on_chdir
    def _eat_chdir(olddir, newdir, **kwargs):
        """Emit OSC 7 (directory changed) for eat."""
        import os, urllib.parse
        hostname = os.uname().nodename
        encoded = urllib.parse.quote(newdir, safe='/:@')
        print(f'\033]7;file://{hostname}{encoded}\033\\', end='', flush=True)

    @events.on_pre_prompt
    def _eat_pre_prompt(**kwargs):
        """Emit OSC 133 ;A (prompt start) for eat."""
        print('\033]133;A\033\\', end='', flush=True)

    @events.on_post_block
    def _eat_post_block(cmd, rtn, **kwargs):
        """Emit OSC 133 ;D;<exitcode> (command end) for eat."""
        print(f'\033]133;D;{rtn}\033\\', end='', flush=True)

# ============================================================================
# direnv — per-directory environment variables
#
# Native Python implementation via @events.on_chdir. Calls
# `direnv export json`, parses the result, and applies/removes variables.
# Handles PATH merging correctly — splits the direnv PATH string and
# prepends entries to $PATH rather than replacing it wholesale.
#
# Requirements:
#   - Run `direnv allow` in any directory with a new or changed .envrc.
#   - .envrc files using `use flake` require nix-direnv.
#   - DIRENV_LOG_FORMAT="" silences the "direnv: loading" messages.
# ============================================================================
if shutil.which('direnv'):

    # Track which keys direnv last set so we can unload them on leave
    _direnv_keys: set = set()

    @events.on_chdir
    def _direnv_chdir(olddir, newdir, **kwargs):
        import subprocess
        global _direnv_keys

        result = subprocess.run(
            ['direnv', 'export', 'json'],
            capture_output=True, text=True
        )

        if result.returncode != 0:
            msg = result.stderr.strip()
            if msg:
                print(f'\033[33mdirenv:\033[0m {msg}')
            return

        stdout = result.stdout.strip()
        if not stdout:
            # direnv returned nothing — unload any previously set vars
            for k in list(_direnv_keys):
                if k in ${...}:
                    del ${k}
            _direnv_keys = set()
            return

        try:
            import json
            exports = json.loads(stdout)
        except json.JSONDecodeError as e:
            print(f'\033[33mdirenv:\033[0m JSON parse error: {e}')
            return

        # Apply PATH separately — merge rather than replace
        if 'PATH' in exports:
            direnv_paths = [
                p for p in exports.pop('PATH').split(':') if p
            ]
            for p in reversed(direnv_paths):
                if p not in $PATH:
                    $PATH.insert(0, p)

        # Apply / clear remaining variables
        new_keys = set()
        for k, v in exports.items():
            if v is None:
                # direnv signals removal with null
                if k in ${...}:
                    del ${k}
            else:
                ${k} = v
                new_keys.add(k)

        _direnv_keys = new_keys

# ============================================================================
# zoxide — smarter cd
# ============================================================================
if shutil.which('zoxide'):
    # zoxide init xonsh emits xonsh-compatible source code
    _zoxide_init = subprocess.run(
        ['zoxide', 'init', 'xonsh'],
        capture_output=True, text=True
    ).stdout
    if _zoxide_init.strip():
        exec(compile(_zoxide_init, '<zoxide-init>', 'exec'))
    del _zoxide_init

# ============================================================================
# mise — polyglot runtime manager
# Must activate before direnv so managed runtimes are on PATH when .envrc runs.
# ============================================================================
if shutil.which('mise'):
    _mise_env = subprocess.run(
        ['mise', 'activate', 'xonsh'],
        capture_output=True, text=True
    ).stdout
    if _mise_env.strip():
        exec(compile(_mise_env, '<mise-activate>', 'exec'))
    del _mise_env
elif shutil.which('asdf'):
    # asdf fallback
    _asdf_dir = os.path.expanduser('~/.asdf')
    for _asdf_path in (
        os.path.join(_asdf_dir, 'shims'),
        os.path.join(_asdf_dir, 'bin'),
    ):
        if os.path.isdir(_asdf_path) and _asdf_path not in $PATH:
            $PATH.insert(0, _asdf_path)
    del _asdf_dir, _asdf_path

# ============================================================================
# atuin — enhanced shell history
# ============================================================================
if shutil.which('atuin'):
    _atuin_init = subprocess.run(
        ['atuin', 'init', 'xonsh'],
        capture_output=True, text=True
    ).stdout
    if _atuin_init.strip():
        exec(compile(_atuin_init, '<atuin-init>', 'exec'))
    del _atuin_init

# ============================================================================
# fnm — Fast Node Manager
# ============================================================================
if shutil.which('fnm'):
    try:
        import json as _json
        _fnm_raw = subprocess.run(
            ['fnm', 'env', '--json'],
            capture_output=True, text=True
        )
        if _fnm_raw.returncode == 0:
            _fnm_exports = _json.loads(_fnm_raw.stdout)
            if 'PATH' in _fnm_exports:
                _fnm_paths = [p for p in _fnm_exports.pop('PATH').split(':') if p]
                for _p in reversed(_fnm_paths):
                    if _p not in $PATH:
                        $PATH.insert(0, _p)
            for _k, _v in _fnm_exports.items():
                ${_k} = _v
        del _json, _fnm_raw, _fnm_exports
    except Exception as _e:
        print(f'\033[33mwarn:\033[0m fnm init failed: {_e}')
        del _e

# ============================================================================
# pyenv
# ============================================================================
if shutil.which('pyenv'):
    _pyenv_root = os.environ.get('PYENV_ROOT', os.path.expanduser('~/.pyenv'))
    $PYENV_ROOT = _pyenv_root
    for _p in (
        os.path.join(_pyenv_root, 'shims'),
        os.path.join(_pyenv_root, 'bin'),
    ):
        if os.path.isdir(_p) and _p not in $PATH:
            $PATH.insert(0, _p)
    del _pyenv_root, _p

# ============================================================================
# rbenv
# ============================================================================
if shutil.which('rbenv'):
    _rbenv_init = subprocess.run(
        ['rbenv', 'init', '-', 'bash'],
        capture_output=True, text=True
    ).stdout
    # rbenv init emits POSIX shell; extract the PATH prepend only
    for _line in _rbenv_init.splitlines():
        if 'export PATH=' in _line or 'PATH=' in _line:
            _rbenv_path = os.path.expanduser('~/.rbenv/shims')
            if os.path.isdir(_rbenv_path) and _rbenv_path not in $PATH:
                $PATH.insert(0, _rbenv_path)
            break
    del _rbenv_init, _line

# ============================================================================
# GitHub CLI completion
# ============================================================================
if shutil.which('gh'):
    _gh_comp = subprocess.run(
        ['gh', 'completion', '-s', 'bash'],
        capture_output=True, text=True
    ).stdout
    # gh bash completions are not directly usable in xonsh; the completer
    # registered in completions.xsh handles basic gh subcommands instead.
    del _gh_comp

# ============================================================================
# nerdfetch — system info on startup (outside tmux, shown once)
# ============================================================================
if (shutil.which('nerdfetch')
        and 'TMUX' not in ${...}
        and 'NERDFETCH_SHOWN' not in ${...}):
    subprocess.run(['nerdfetch'])
    $NERDFETCH_SHOWN = '1'

del os, shutil, subprocess
