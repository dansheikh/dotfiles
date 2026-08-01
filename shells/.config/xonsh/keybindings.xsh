# keybindings.xsh - Key bindings
# Location: ~/.config/xonsh/keybindings.xsh
#
# Uses prompt_toolkit's key binding API.
# EmacsInputMode and ViInsertMode were removed from prompt_toolkit.filters
# in recent versions — use the filter functions vi_insert_mode / emacs_mode
# (lowercase) which are stable across versions.
#
# The entire file is wrapped in a guard block so non-interactive sessions
# skip it cleanly without raising any exception.

import shutil
from prompt_toolkit.filters import vi_insert_mode, emacs_mode

if hasattr(__xonsh__, 'shell') and hasattr(__xonsh__.shell, 'shell'):

    _kb = __xonsh__.shell.shell.key_bindings
    _insert_mode = vi_insert_mode | emacs_mode

    # ========================================================================
    # Ctrl-F — accept full autosuggestion
    # Ctrl-Space intentionally unbound — reserved for tmux prefix passthrough.
    # ========================================================================
    @_kb.add('c-f', filter=_insert_mode)
    def _accept_suggestion(event):
        buf = event.current_buffer
        if buf.suggestion:
            buf.insert_text(buf.suggestion.text)

    # ========================================================================
    # Alt-E — edit command line in $EDITOR
    # ========================================================================
    @_kb.add('escape', 'e', filter=_insert_mode)
    def _edit_in_editor(event):
        event.current_buffer.open_in_editor(event.cli)

    # ========================================================================
    # fzf widgets — only registered when fzf is available
    # ========================================================================
    if shutil.which('fzf'):

        # --------------------------------------------------------------------
        # Ctrl-G b — git branch switcher
        # --------------------------------------------------------------------
        @_kb.add('c-g', 'b', filter=_insert_mode)
        def _fzf_git_branch(event):
            import subprocess
            if not shutil.which('git'):
                return
            result = subprocess.run(
                ['bash', '-c',
                 'git branch -a 2>/dev/null | grep -v HEAD '
                 '| fzf --ansi --height=80% --layout=reverse '
                 "--preview='git log --oneline --graph --color=always {1}' "
                 '| sed "s/.* //" | sed "s#remotes/[^/]*/##"'],
                capture_output=True, text=True
            )
            branch = result.stdout.strip()
            if branch:
                event.current_buffer.insert_text(f'git checkout {branch}')

        # --------------------------------------------------------------------
        # Ctrl-G l — git log browser
        # --------------------------------------------------------------------
        @_kb.add('c-g', 'l', filter=_insert_mode)
        def _fzf_git_log(event):
            import subprocess
            if not shutil.which('git'):
                return
            subprocess.run(
                ['bash', '-c',
                 'git log --graph --color=always '
                 '--format="%C(auto)%h%d %s %C(black)%C(bold)%cr" '
                 '| fzf --ansi --no-sort --reverse --tiebreak=index '
                 "--preview='echo {} | grep -o \"[a-f0-9]\\{7\\}\" "
                 "| head -1 | xargs git show --color=always' "
                 "--bind='enter:execute:"
                 'echo {} | grep -o "[a-f0-9]\\{7\\}" '
                 "| head -1 | xargs git show | less -R'"]
            )
            event.app.renderer.reset()
            event.app.current_buffer.reset()

        # --------------------------------------------------------------------
        # Ctrl-K — fzf process killer
        # --------------------------------------------------------------------
        @_kb.add('c-k', filter=_insert_mode)
        def _fzf_kill(event):
            import subprocess, os
            cmd = (
                f'ps -f -u {os.getuid()} | sed 1d '
                '| fzf -m --height=80% --layout=reverse '
                "--preview='echo {}' --preview-window=down:3:wrap "
                "| awk '{print $2}' | xargs -r kill -9"
            )
            subprocess.run(['bash', '-c', cmd])
            event.app.renderer.reset()

        # --------------------------------------------------------------------
        # Ctrl-G e — environment variable browser
        # --------------------------------------------------------------------
        @_kb.add('c-g', 'e', filter=_insert_mode)
        def _fzf_env(event):
            import subprocess
            result = subprocess.run(
                ['bash', '-c',
                 'env | fzf --height=80% --layout=reverse | cut -d= -f1'],
                capture_output=True, text=True
            )
            var = result.stdout.strip()
            if var:
                event.current_buffer.insert_text(var)

        # --------------------------------------------------------------------
        # Ctrl-X r — fzf history search
        # --------------------------------------------------------------------
        @_kb.add('c-x', 'r', filter=_insert_mode)
        def _fzf_history_execute(event):
            import subprocess
            result = subprocess.run(
                ['bash', '-c',
                 'fc -rl 1 2>/dev/null | fzf --tac --no-sort '
                 "--preview='echo {}' --preview-window=down:3:wrap "
                 "| sed 's/ *[0-9]* *//'"],
                capture_output=True, text=True
            )
            cmd = result.stdout.strip()
            if cmd:
                event.current_buffer.text = cmd
                event.current_buffer.cursor_position = len(cmd)

    del _insert_mode

del vi_insert_mode, emacs_mode, shutil
