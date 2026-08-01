-- Clojure filetype settings
local opt = vim.opt_local
-- Clojure indentation (2 spaces)
opt.shiftwidth = 2
opt.softtabstop = 2
opt.tabstop = 2
opt.expandtab = true
-- Lisp-specific settings (fallback when treesitter indent is unavailable)
opt.lisp = true
-- Comments
opt.commentstring = ';; %s'
-- Style guide
opt.textwidth = 80
opt.colorcolumn = '81'
