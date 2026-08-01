# completions.xsh - Completer registration
# Location: ~/.config/xonsh/completions.xsh
#
# Xonsh's completer API allows Python functions to provide completions.
# Completers are registered with __xonsh__.completers and run in priority
# order. Built-in completers (commands, path, python) remain active unless
# explicitly removed.
#
# Runs AFTER plugins.xsh so xontrib completers are already registered.

import shutil
from xonsh.completers.tools import contextual_command_completer

# ============================================================================
# Completer settings
# ============================================================================

# Case-insensitive path and command completion
$CASE_SENSITIVE_COMPLETIONS = False

# ============================================================================
# Git completer
#
# Provides completions for git subcommands and common flags. Falls back
# gracefully if git is not installed.
# ============================================================================
if shutil.which('git'):
    @contextual_command_completer
    def _git_completer(ctx):
        """Basic git subcommand completions."""
        if not ctx.args or ctx.args[0].value != 'git':
            return None
        if len(ctx.args) == 1:
            subcmds = {
                'add', 'branch', 'checkout', 'cherry-pick', 'clone',
                'commit', 'diff', 'fetch', 'log', 'merge', 'pull',
                'push', 'rebase', 'remote', 'reset', 'restore', 'show',
                'stash', 'status', 'switch', 'tag',
            }
            return subcmds
        return None

    __xonsh__.completers['git'] = _git_completer
    del _git_completer

# ============================================================================
# Docker completer
# ============================================================================
if shutil.which('docker'):
    @contextual_command_completer
    def _docker_completer(ctx):
        """Basic docker subcommand completions."""
        if not ctx.args or ctx.args[0].value != 'docker':
            return None
        if len(ctx.args) == 1:
            subcmds = {
                'build', 'compose', 'exec', 'images', 'inspect',
                'logs', 'ps', 'pull', 'push', 'rm', 'rmi', 'run',
                'start', 'stop', 'system', 'tag', 'volume',
            }
            return subcmds
        return None

    __xonsh__.completers['docker'] = _docker_completer
    del _docker_completer

# ============================================================================
# kubectl completer
# ============================================================================
if shutil.which('kubectl'):
    @contextual_command_completer
    def _kubectl_completer(ctx):
        """Basic kubectl subcommand completions."""
        if not ctx.args or ctx.args[0].value != 'kubectl':
            return None
        if len(ctx.args) == 1:
            subcmds = {
                'apply', 'create', 'delete', 'describe', 'edit',
                'exec', 'get', 'logs', 'patch', 'port-forward',
                'rollout', 'scale', 'top',
            }
            return subcmds
        return None

    __xonsh__.completers['kubectl'] = _kubectl_completer
    del _kubectl_completer

del shutil, contextual_command_completer
