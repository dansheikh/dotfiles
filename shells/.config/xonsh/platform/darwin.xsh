# platform/darwin.xsh - macOS / nix-darwin-specific environment setup
# Location: ~/.config/xonsh/platform/darwin.xsh
#
# Sourced by rc.xsh when running on macOS. Uses __xonsh__.env throughout —
# no $VAR interpolation in Python expressions during early init.

import os
import getpass
import subprocess
import shlex

_env = __xonsh__.env
_user = getpass.getuser()

# ============================================================================
# PATH — nix-darwin and home-manager profile directories
# ============================================================================
_darwin_paths = [
    f'/etc/profiles/per-user/{_user}/bin',
    os.path.expanduser('~/.nix-profile/bin'),
    '/nix/var/nix/profiles/default/bin',
    '/run/current-system/sw/bin',
]

_current_path = list(_env.get('PATH', []))
for _p in _darwin_paths:
    if os.path.isdir(_p) and _p not in _current_path:
        _current_path.insert(0, _p)
del _darwin_paths, _user

# ============================================================================
# Nix daemon environment
# Source nix-daemon.sh via bash and capture NIX_* exports if not already set.
# ============================================================================
_nix_daemon_sh = '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
if os.path.isfile(_nix_daemon_sh) and not _env.get('NIX_PROFILES'):
    try:
        _result = subprocess.run(
            ['bash', '-c', f'source {shlex.quote(_nix_daemon_sh)} && env'],
            capture_output=True, text=True
        )
        for _line in _result.stdout.splitlines():
            if '=' in _line:
                _k, _, _v = _line.partition('=')
                if _k in ('NIX_PROFILES', 'NIX_PATH', 'NIX_SSL_CERT_FILE',
                          'NIX_USER_PROFILE_DIR', 'NIX_LINK'):
                    _env[_k] = _v
    except Exception:
        pass
del _nix_daemon_sh

# ============================================================================
# Homebrew — only if present
# ============================================================================
for _brew_prefix in ('/opt/homebrew', '/usr/local'):
    if os.path.isfile(os.path.join(_brew_prefix, 'bin', 'brew')):
        for _d in (
            os.path.join(_brew_prefix, 'bin'),
            os.path.join(_brew_prefix, 'sbin'),
        ):
            if os.path.isdir(_d) and _d not in _current_path:
                _current_path.insert(0, _d)
        break

_env['PATH'] = _current_path
del _current_path, _brew_prefix

