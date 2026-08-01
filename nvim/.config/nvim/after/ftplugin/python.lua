-- Python filetype settings
local opt = vim.opt_local

-- PEP 8 indentation
opt.shiftwidth = 4
opt.softtabstop = 4
opt.tabstop = 4
opt.expandtab = true

-- Text width (Black compatibility)
opt.textwidth = 88
opt.colorcolumn = '+1'

-- Comments
opt.comments = 'b:#,fb:-'
opt.commentstring = '# %s'
