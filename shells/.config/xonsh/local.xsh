# local.xsh - Machine-local overrides
# Location: ~/.config/xonsh/local.xsh
#
# Sourced last by rc.xsh. Use for overrides specific to this machine that
# must not be committed to the dotfiles repository.
#
# ── IMPORTANT: add to .gitignore ──────────────────────────────────────────
# Add this file to your global gitignore to prevent accidental commits:
#   echo '.config/xonsh/local.xsh' >> ~/.gitignore_global
#   git config --global core.excludesFile ~/.gitignore_global
#
# ── Available toggles ─────────────────────────────────────────────────────
# Startup timing (prints per-module ms on launch):
#   import os; os.environ['XONSH_DEBUG'] = '1'
#
# Verbose tracebacks in terminal (logfile always active via opts.xsh):
#   $XONSH_SHOW_TRACEBACK = True
#
# Enable threaded completions if you find completion slow:
#   $COMPLETION_IN_THREAD = True
#
# ── Machine-local config below ────────────────────────────────────────────
