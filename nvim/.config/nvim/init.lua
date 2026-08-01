-- Neovim Configuration Entry Point
-- Leader keys must be set before lazy.nvim loads
vim.g.mapleader = ' '
vim.g.maplocalleader = ','  -- For Conjure (Clojure REPL)

-- Load configuration modules
require('config.lazy')
require('config.options')
require('config.keymaps')
require('config.autocmds')
