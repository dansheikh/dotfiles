-- JSON filetype settings
local opt = vim.opt_local

-- JSON indentation (2 spaces)
opt.shiftwidth = 2
opt.softtabstop = 2
opt.tabstop = 2
opt.expandtab = true

-- Disable spell checking in JSON
opt.spell = false

-- Conceal quotes
opt.conceallevel = 0
