# history.xsh - History configuration
# Location: ~/.config/xonsh/history.xsh
#
# $XONSH_HISTORY_FILE is set in env.xsh.

import os
import sqlite3

# ============================================================================
# Validate existing history file — remove if corrupt
# ============================================================================
_hist_file = __xonsh__.env.get('XONSH_HISTORY_FILE', '')
if _hist_file and os.path.isfile(_hist_file):
    try:
        _conn = sqlite3.connect(_hist_file)
        _conn.execute('PRAGMA journal_mode=WAL;')
        _conn.close()
    except sqlite3.DatabaseError:
        print(f'\033[33mwarn:\033[0m corrupt history file removed: {_hist_file}')
        os.remove(_hist_file)
del _hist_file, sqlite3

# ============================================================================
# Backend
# ============================================================================
$XONSH_HISTORY_BACKEND = 'sqlite'

# ============================================================================
# Size
# ============================================================================
$XONSH_HISTORY_SIZE = (50000, 'commands')
$XONSH_HISTORY_MATCH_ANYWHERE = False

# ============================================================================
# Deduplication
# ============================================================================
$HISTCONTROL = 'ignoredups'

# ============================================================================
# Load length
# Reduced from 10000 — loading 10k entries on every startup is measurably
# slow. 2500 covers typical interactive lookups; SQLite search covers the rest.
# ============================================================================
$XONSH_HISTORY_LOAD_LENGTH = 2500

del os
