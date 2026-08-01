# opts.xsh - Xonsh shell settings
# Location: ~/.config/xonsh/opts.xsh

# ============================================================================
# Display
# ============================================================================
$XONSH_SHOW_BANNER = False
$XONSH_COLOR_STYLE = 'monokai'
$TERM_TITLE        = True

# ============================================================================
# Debug / tracebacks
# Log tracebacks to a file so errors are always recoverable without having
# to reproduce them with XONSH_SHOW_TRACEBACK=1 set upfront.
# ============================================================================
import os as _os
$XONSH_SHOW_TRACEBACK = False
# Read XDG_STATE_HOME from os.environ directly — $XDG_STATE_HOME may not
# be available in the xonsh env dict yet at opts.xsh load time.
_xonsh_log_dir = _os.path.join(
    _os.environ.get('XDG_STATE_HOME', _os.path.expanduser('~/.local/state')),
    'xonsh'
)
_os.makedirs(_xonsh_log_dir, exist_ok=True)
_xonsh_log_file = _os.path.join(_xonsh_log_dir, 'error.log')
# Touch the file so xonsh's path validation passes
if not _os.path.exists(_xonsh_log_file):
    open(_xonsh_log_file, 'a').close()
$XONSH_TRACEBACK_LOGFILE = _xonsh_log_file
del _os, _xonsh_log_dir, _xonsh_log_file

# ============================================================================
# Startup timing
# Set XONSH_DEBUG=1 in local.xsh to enable per-module timing output.
# ============================================================================
$XONSH_DEBUG = 0

# ============================================================================
# Input / editing
# ============================================================================
$VI_MODE        = True
$MOUSE_SUPPORT  = False
$INDENT         = '    '

# ============================================================================
# Completion
# COMPLETION_IN_THREAD can cause intermittent freezes in some xonsh versions
# when completions race with input. Disabled by default; enable in local.xsh
# if you find completion noticeably slow on your machine.
# ============================================================================
$COMPLETIONS_DISPLAY           = 'multi'
$COMPLETIONS_CONFIRM           = False
$COMPLETION_IN_THREAD          = False
$UPDATE_COMPLETIONS_ON_KEYPRESS = True
$COMPLETIONS_MENU_ROWS          = 5

# ============================================================================
# Suggestions
# AUTO_SUGGEST was renamed to XONSH_PROMPT_AUTO_SUGGEST in xonsh >= 0.18.
# ============================================================================
$XONSH_PROMPT_AUTO_SUGGEST = True

# ============================================================================
# Warnings and errors
# ============================================================================
$SUGGEST_COMMANDS = True
$SUGGEST_MAX_NUM  = 5

# ============================================================================
# Glob and expansion
# ============================================================================
$DOTGLOB     = True
$GLOB_SORTED = True

# ============================================================================
# Subprocess defaults
# RAISE_SUBPROC_ERROR was renamed to XONSH_SUBPROC_CMD_RAISE_ERROR.
# ============================================================================
$XONSH_SUBPROC_CMD_RAISE_ERROR = False

# ============================================================================
# Miscellaneous
# ============================================================================
$AUTO_CD = True

import shutil as _shutil
_xonsh_bin = _shutil.which('xonsh')
if _xonsh_bin:
    $SHELL = _xonsh_bin
del _shutil, _xonsh_bin
