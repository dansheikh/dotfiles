# VS Code + Vim Keybindings Quick Reference

## Leader Key
`<leader>` = `Space`

---

## File Operations
| Key | Action |
|-----|--------|
| `<leader>w` | Save file |
| `<leader>q` | Close editor |
| `<leader>ff` | Quick open (fuzzy file finder) |
| `<leader>fg` | Find in files |
| `<leader>fs` | Go to symbol in file |
| `<leader>fw` | Go to symbol in workspace |

---

## LSP Navigation (Vim Mode)
| Key | Action |
|-----|--------|
| `gd` | Go to definition |
| `gD` | Go to declaration |
| `gr` | Go to references |
| `gi` | Go to implementation |
| `gt` | Go to type definition |
| `K` | Show hover documentation |
| `[d` | Previous diagnostic (all files) |
| `]d` | Next diagnostic (all files) |
| `[e` | Previous error (current file) |
| `]e` | Next error (current file) |

---

## LSP Actions (Vim Mode)
| Key | Action |
|-----|--------|
| `<leader>rn` | Rename symbol |
| `<leader>ca` | Code actions / Quick fix |
| `<leader>cf` | Format document (or selection in visual) |

---

## LSP (Normal Keybindings)
| Key | Action |
|-----|--------|
| `Ctrl+K Ctrl+H` | Show hover |
| `Ctrl+K Ctrl+S` | Parameter hints |
| `Ctrl+.` | Quick fix |
| `Ctrl+Shift+F` | Format document |
| `Ctrl+R` | Go to symbol in file |
| `Ctrl+Shift+R` | Go to symbol in workspace |
| `Ctrl+K Ctrl+Enter` | Go to declaration |

---

## Git Integration
| Key | Action |
|-----|--------|
| `<leader>gb` | Show git blame (Vim mode) |
| `<leader>gg` | Open git graph (Vim mode) |
| `Ctrl+K Ctrl+G` | Git graph (normal) |

---

## Panels & Views
| Key | Action |
|-----|--------|
| `Ctrl+K Ctrl+E` | Explorer |
| `Ctrl+K Ctrl+G` | Source control |
| `Ctrl+K Ctrl+D` | Debug |
| `Ctrl+K Ctrl+X` | Extensions |
| `Ctrl+K Ctrl+B` | Toggle sidebar |
| `Ctrl+K Ctrl+P` | Problems panel |
| `Ctrl+E` | Focus editor |

---

## Terminal
| Key | Action |
|-----|--------|
| `Ctrl+T` | Toggle terminal |
| `Ctrl+X Ctrl+T` | Create terminal editor |
| `Ctrl+Shift+Enter` | Run selection in terminal |
| `Ctrl+Enter` | Run current line in terminal |
| `Ctrl+K` | Clear terminal (when focused) |
| `Ctrl+\`` | Next terminal |
| `Ctrl+Shift+\`` | Previous terminal |
| `Ctrl+PageDown` | Focus next terminal |
| `Ctrl+PageUp` | Focus previous terminal |

---

## Window Navigation
| Key | Action |
|-----|--------|
| `Ctrl+W H` | Navigate left |
| `Ctrl+W J` | Navigate down |
| `Ctrl+W K` | Navigate up |
| `Ctrl+W L` | Navigate right |

---

## Split Management
| Key | Action |
|-----|--------|
| `Alt+N` | Split editor (or terminal if focused) |
| `Alt+L` | Navigate right split |
| `Alt+H` | Navigate left split |
| `Alt+W` | Close split (or kill terminal) |
| `Alt+=` | Increase view size |
| `Alt+-` | Decrease view size |

---

## Terminal Split Navigation
| Key | Action |
|-----|--------|
| `Alt+N` | Split terminal |
| `Alt+L` | Next terminal pane |
| `Alt+H` | Previous terminal pane |
| `Alt+W` | Kill terminal |

---

## Code Editing
| Key | Action |
|-----|--------|
| `Ctrl+L` | Duplicate line down |
| `Ctrl+J` | Join lines |
| `Shift+Ctrl+[` | Fold |
| `Shift+Ctrl+]` | Unfold |

---

## Multi-Cursor
| Key | Action |
|-----|--------|
| `Ctrl+Backspace` | Remove previous selection |
| `Ctrl+K Ctrl+D` | Add next occurrence |
| `Ctrl+Right` | Cursor at end of each line |

---

## File Explorer (when focused)
| Key | Action |
|-----|--------|
| `Ctrl+N` | New file |
| `Shift+Ctrl+N` | New folder |
| `Ctrl+D` | Duplicate file |

---

## Emmet
| Key | Action |
|-----|--------|
| `Ctrl+M Ctrl+I` | Balance in |
| `Ctrl+M Ctrl+O` | Balance out |
| `Ctrl+M Ctrl+W` | Wrap with abbreviation |
| `Ctrl+M Ctrl+M` | Match tag |
| `Ctrl+M Ctrl+E` | Smart select expand |
| `Ctrl+M Ctrl+R` | Update tag |
| `Ctrl+M Ctrl+Backspace` | Remove tag |

---

## Utilities
| Key | Action |
|-----|--------|
| `Ctrl+K Ctrl+K` | Toggle font size |
| `Ctrl+;` | Switch window/project |
| `Alt+Ctrl+Right` | Next window tab |
| `Alt+Ctrl+Left` | Previous window tab |
| `Alt+Ctrl+I` | Toggle DevTools |
| `Escape` | Hide notifications |

---

## Language-Specific Notes

### Python (Ruff)
- Auto-format on save
- Auto-organize imports on save
- Auto-fix linting issues on save
- Inline diagnostics via Error Lens

### SQL (SQLFluff)
- Auto-format on save
- Configure dialect in `.sqlfluff` file
- Use SQLTools for database connections

### Rust
- Clippy runs on save
- Inlay hints for types and parameters
- Press `Alt` to temporarily hide hints

### Go
- Auto-format with gofmt on save
- Auto-organize imports on save
- Full gopls LSP support

### JavaScript/TypeScript
- ESLint auto-fix on save
- Prettier formatting on save

---

## Tips

1. **Error Lens**: Inline errors appear after 300ms delay
2. **Inlay Hints**: Press `Alt` to toggle visibility
3. **Hover**: 300ms delay before showing
4. **Terminal**: Sessions persist across VS Code restarts
5. **Git Blame**: Shows in status bar when line changes
6. **SQLTools**: Click SQLTools icon to manage connections
7. **Scrollbars**: Auto-hide when not in use
8. **Occurrences**: Highlights matching symbols in current file

---

## Customization

All settings in `~/.config/Code/User/settings.json`
All keybindings in `~/.config/Code/User/keybindings.json`

See `INSTALLATION_GUIDE.md` for detailed configuration options.