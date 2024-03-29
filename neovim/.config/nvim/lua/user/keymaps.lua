local utility = require('lib.utility')

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

utility.map('i', '<C-c>', '<esc><C-w>c')
utility.map('i', '<C-h>', '<esc><C-w>h')
utility.map('i', '<C-j>', '<esc><C-w>j')
utility.map('i', '<C-k>', '<esc><C-w>k')
utility.map('i', '<C-l>', '<esc><C-w>l')
utility.map('i', '<C-o>', '<esc><C-w>o')
utility.map('n', '<C-c>', '<C-w>c')
utility.map('n', '<C-h>', '<C-w>h')
utility.map('n', '<C-j>', '<C-w>j')
utility.map('n', '<C-k>', '<C-w>k')
utility.map('n', '<C-l>', '<C-w>l')
utility.map('n', '<C-o>', '<C-w>o')
utility.map('n', '<C-s>', '<C-w>v')
utility.map('n', '<C-S>', '<C-w>s')
utility.map('n', '<C-S-tab>', ':bprevious<cr>')
utility.map('n', '<C-tab>', ':bnext<cr>')
utility.map('n', '<M-j>', ':bprevious<cr>')
utility.map('n', '<M-k>', ':bnext<cr>')

utility.map('n', '+', '<C-w>+')
utility.map('n', '-', '<C-w>-')
utility.map('n', '>', '<C-w>>')
utility.map('n', '<', '<C-w><')
utility.map('n', '_', '<C-w>_')
utility.map('n', '<bar>', '<C-w>|')

utility.map('n', ';b', ':HopChar2<cr>')
utility.map('n', ';c', ':HopChar1<cr>')
utility.map('n', ';e', ':Telescope file_browser<cr>')
utility.map('n', ';f', ':Telescope find_files<cr>')
utility.map('n', ';g', ':Telescope live_grep<cr>')
utility.map('n', ';l', ':HopLine<cr>')
utility.map('n', ';n', ':enew<cr>')
utility.map('n', ';p', ':HopPattern<cr>')
utility.map('n', ';s', ':HopLineStart<cr>')
utility.map('n', ';u', ':PackerSync<cr>')
utility.map('n', ';w', ':HopWord<cr>')
utility.map('n', ';q', ':quit<cr>')
