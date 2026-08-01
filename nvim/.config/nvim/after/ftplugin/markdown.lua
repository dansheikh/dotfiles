-- Markdown filetype settings
local opt = vim.opt_local

-- Markdown indentation (2 spaces)
opt.shiftwidth = 2
opt.softtabstop = 2
opt.tabstop = 2
opt.expandtab = true

-- Enable word wrap for markdown
opt.wrap = true
opt.linebreak = true

-- Text width
opt.textwidth = 80
opt.colorcolumn = '+1'

-- Comments
opt.commentstring = '<!-- %s -->'

-- Spell checking for prose
opt.spell = true
