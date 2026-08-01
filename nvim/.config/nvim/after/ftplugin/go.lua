-- Go filetype settings
local opt = vim.opt_local

-- Go uses tabs (not spaces)
opt.shiftwidth = 4
opt.softtabstop = 4
opt.tabstop = 4
opt.expandtab = false

-- Comments
opt.commentstring = '// %s'
