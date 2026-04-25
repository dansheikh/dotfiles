-- Rust filetype settings
local opt = vim.opt_local

-- Rust indentation (4 spaces)
opt.shiftwidth = 4
opt.softtabstop = 4
opt.tabstop = 4
opt.expandtab = true

-- Text width
opt.textwidth = 100
opt.colorcolumn = '+1'

-- Comments
opt.commentstring = '// %s'
