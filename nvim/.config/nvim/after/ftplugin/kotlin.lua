-- Kotlin-specific settings
vim.opt_local.tabstop = 4
vim.opt_local.shiftwidth = 4
vim.opt_local.softtabstop = 4
vim.opt_local.expandtab = true

-- Comment string for Kotlin
vim.opt_local.commentstring = '// %s'

-- Kotlin uses $ in identifiers (for template strings)
vim.opt_local.iskeyword:append('$')

-- Format on save is handled by efm via autocmds.lua
-- LSP features (hover, goto-def, etc.) handled by kotlin-language-server
