#!/usr/bin/env zsh
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

TARGET_PANE="${1:-}"

# Resolve target pane ID
POPUP_PANE="$(tmux display-message -p '#{pane_id}' 2>/dev/null)"
if [[ -z "$TARGET_PANE" || "$TARGET_PANE" == "#D" || "$TARGET_PANE" == "$POPUP_PANE" ]]; then
  TARGET_PANE=$(tmux display-message -p '#{client_last_session}:#{active_window_index}' 2>/dev/null | xargs -I{} tmux list-panes -t {} -F '#{pane_id} #{pane_active}' 2>/dev/null | awk '$2 == 1 {print $1; exit}')
fi
[[ -z "$TARGET_PANE" ]] && TARGET_PANE="$(tmux display-message -p '#{pane_id}')"

# Capture pane lines, prefix with relative distance from bottom (0 = lowest screen row), and feed into fzf hiding the index delimiter.
SELECTED=$(tmux capture-pane -ep -S -50000 -E - -t "$TARGET_PANE" 2>/dev/null \
  | sed -E 's/\x1B\[[0-9;]*[a-zA-Z]//g' \
  | tac \
  | awk 'NF {print NR-1 "\t" $0}' \
  | fzf --delimiter='\t' --with-nth=2.. --reverse --prompt="Jump to > " --height=100% 2>/dev/null)

if [[ -n "$SELECTED" ]]; then
  # Extract line offset from the bottom of the buffer
  OFFSET=$(print -rn -- "$SELECTED" | cut -f1)

  # 1. Put the target pane into copy-mode (if not already in copy-mode)
  tmux copy-mode -t "$TARGET_PANE" 2>/dev/null || true

  # 2. Reset cursor position to bottom of copy-mode buffer (G in vi mode)
  tmux send-keys -t "$TARGET_PANE" -X top-line
  tmux send-keys -t "$TARGET_PANE" -X history-bottom

  # 3. Scroll up to the matched line
  if [[ "$OFFSET" -gt 0 ]]; then
    tmux send-keys -t "$TARGET_PANE" -N "$OFFSET" -X cursor-up
  fi
fi
