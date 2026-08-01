# platform/nixos.xsh - NixOS-specific environment setup
# Location: ~/.config/xonsh/platform/nixos.xsh
#
# Sourced by rc.xsh when running on NixOS (NIX_PROFILES set or
# /run/current-system exists). Uses __xonsh__.env throughout — no $VAR
# interpolation in Python expressions during early init.

import os
import glob as _glob

_env = __xonsh__.env

# ============================================================================
# PATH — Phase 1: hardcoded Nix fallback candidates
# ============================================================================
_nix_fallback = [
    os.path.expanduser('~/.nix-profile/bin'),
    os.path.expanduser('~/.local/state/nix/profiles/profile/bin'),
    '/nix/var/nix/profiles/default/bin',
    '/run/current-system/sw/bin',
]

_current_path = list(_env.get('PATH', []))
for _p in _nix_fallback:
    if os.path.isdir(_p) and _p not in _current_path:
        _current_path.append(_p)
del _nix_fallback

# ============================================================================
# PATH — Phase 2: NIX_PROFILES (authoritative for active Nix profile stacks)
# ============================================================================
_nix_profiles_str = _env.get('NIX_PROFILES', '')
for _prof in (_nix_profiles_str.strip().split() if _nix_profiles_str else []):
    _bin = os.path.join(_prof.strip(), 'bin')
    if os.path.isdir(_bin) and _bin not in _current_path:
        _current_path.append(_bin)
del _nix_profiles_str

# ============================================================================
# /run/wrappers/bin — NixOS setuid wrappers (must be first)
# ============================================================================
if os.path.isdir('/run/wrappers/bin') and '/run/wrappers/bin' not in _current_path:
    _current_path.insert(0, '/run/wrappers/bin')

_env['PATH'] = _current_path
del _current_path

# ============================================================================
# LOCALE_ARCHIVE
# ============================================================================
if _env.get('NIX_PROFILES'):
    _archives = _glob.glob('/nix/store/*-glibc-locales-*/lib/locale/locale-archive')
    if _archives:
        _env['LOCALE_ARCHIVE'] = sorted(_archives)[-1]
    del _archives

