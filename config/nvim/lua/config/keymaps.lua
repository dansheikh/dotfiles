-- Global Keymaps
-- Clean, conflict-free keybinding setup
--
-- Note: localleader is set to ',' in init.lua for Conjure (Clojure REPL)
-- Conjure keybindings use comma prefix: ,ee (eval), ,eb (eval buffer), etc.
-- LSP keymaps are set buffer-locally in autocmds.lua LspAttach

local map = vim.keymap.set

-- Command shortcuts for accidental Shift key
vim.api.nvim_create_user_command('W', 'w', { desc = 'Write (save)' })
vim.api.nvim_create_user_command('Q', 'q', { desc = 'Quit' })
vim.api.nvim_create_user_command('Wq', 'wq', { desc = 'Write and quit' })
vim.api.nvim_create_user_command('WQ', 'wq', { desc = 'Write and quit' })
vim.api.nvim_create_user_command('Qa', 'qa', { desc = 'Quit all' })
vim.api.nvim_create_user_command('QA', 'qa', { desc = 'Quit all' })

-- Better line movement (respects word wrap)
map('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, desc = 'Move down (display line)' })
map('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, desc = 'Move up (display line)' })

-- Better indenting (stays in visual mode)
map('v', '<', '<gv', { desc = 'Indent left' })
map('v', '>', '>gv', { desc = 'Indent right' })

-- Move selected lines up/down
map('v', 'J', ":m '>+1<cr>gv=gv", { desc = 'Move selection down' })
map('v', 'K', ":m '<-2<cr>gv=gv", { desc = 'Move selection up' })

-- Keep cursor centered when scrolling
map('n', '<C-d>', '<C-d>zz', { desc = 'Scroll down (centered)' })
map('n', '<C-u>', '<C-u>zz', { desc = 'Scroll up (centered)' })
map('n', 'n', 'nzzzv', { desc = 'Next search (centered)' })
map('n', 'N', 'Nzzzv', { desc = 'Previous search (centered)' })

-- Better paste (don't lose register when pasting over selection)
map('x', 'p', '"_dP', { desc = 'Paste without yanking' })

-- Clear search highlighting
map('n', '<Esc>', '<cmd>nohlsearch<cr>', { desc = 'Clear search highlight' })

-- Better terminal escape
map('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Quick list navigation
map('n', '[q', '<cmd>cprevious<cr>', { desc = 'Previous quickfix' })
map('n', ']q', '<cmd>cnext<cr>', { desc = 'Next quickfix' })
map('n', '[l', '<cmd>lprevious<cr>', { desc = 'Previous location' })
map('n', ']l', '<cmd>lnext<cr>', { desc = 'Next location' })

-- Add blank lines without leaving normal mode
map('n', '[<Space>', ':<C-u>put! =repeat(nr2char(10), v:count1)<cr>\'[', { desc = 'Add line above' })
map('n', ']<Space>', ':<C-u>put =repeat(nr2char(10), v:count1)<cr>', { desc = 'Add line below' })

-- Window navigation (native Neovim + tmux via tmux.conf)
map('n', '<C-h>', '<C-w>h', { desc = 'Left window' })
map('n', '<C-j>', '<C-w>j', { desc = 'Down window' })
map('n', '<C-k>', '<C-w>k', { desc = 'Up window' })
map('n', '<C-l>', '<C-w>l', { desc = 'Right window' })

-- Window resizing (arrow keys)
map('n', '<C-Left>', '<C-w><', { desc = 'Decrease width' })
map('n', '<C-Right>', '<C-w>>', { desc = 'Increase width' })
map('n', '<C-Up>', '<C-w>+', { desc = 'Increase height' })
map('n', '<C-Down>', '<C-w>-', { desc = 'Decrease height' })

-- Window resizing (symbols)
map('n', '+', '<C-w>+', { desc = 'Increase height' })
map('n', '-', '<C-w>-', { desc = 'Decrease height' })
map('n', '_', '<C-w>_', { desc = 'Max height' })
map('n', '|', '<C-w>|', { desc = 'Max width' })
map('n', '=', '<C-w>=', { desc = 'Equalize' })

-- Buffer navigation
map('n', '<M-j>', '<cmd>bprevious<cr>', { desc = 'Previous buffer' })
map('n', '<M-k>', '<cmd>bnext<cr>', { desc = 'Next buffer' })
map('n', '<C-S-tab>', '<cmd>bprevious<cr>', { desc = 'Previous buffer' })
map('n', '<C-tab>', '<cmd>bnext<cr>', { desc = 'Next buffer' })

-- Buffer remove (preserves window layout via mini.bufremove)
map('n', '<leader>bd', function() MiniBufremove.delete() end, { desc = 'Delete buffer' })
map('n', '<leader>bD', function() MiniBufremove.delete(0, true) end, { desc = 'Delete buffer (force)' })

-- Diagnostic navigation
map('n', '[d', function() vim.diagnostic.jump({ count = -1 }) end, { desc = 'Previous diagnostic' })
map('n', ']d', function() vim.diagnostic.jump({ count =  1 }) end, { desc = 'Next diagnostic' })

-- Remove 0.12 built-in gr* LSP mappings globally: they conflict with
-- mini.operators (gr = replace-with-register). Our LSP bindings use the
-- quote-prefix namespace ('r, 'a, etc.) and FzfLua-backed gd/gi/gr/gt.
pcall(vim.keymap.del, 'n', 'grn')
pcall(vim.keymap.del, 'n', 'grr')
pcall(vim.keymap.del, 'n', 'gri')
pcall(vim.keymap.del, 'n', 'gra')
pcall(vim.keymap.del, 'n', 'grt')
pcall(vim.keymap.del, 'n', 'grx')
pcall(vim.keymap.del, 'n', 'gO')

-- File explorer bindings
map('n', '<leader>ef', function() MiniFiles.open(vim.api.nvim_buf_get_name(0)) end,
  { desc = 'File explorer (mini.files)' })

