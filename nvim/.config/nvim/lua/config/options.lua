vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Disable netrw for Yazi directory takeover
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

local opt = vim.opt

-- Appearance & Line Numbers
opt.number = true
opt.relativenumber = true
opt.signcolumn = "yes"
opt.cursorline = true
opt.termguicolors = true
opt.wrap = false
opt.scrolloff = 8
opt.sidescrolloff = 8

-- Indentation
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.softtabstop = 2
opt.smartindent = true

-- Search & Splits
opt.ignorecase = true
opt.smartcase = true
opt.splitbelow = true
opt.splitright = true

-- Persistence & Performance
opt.updatetime = 200
opt.timeoutlen = 300
opt.swapfile = false
opt.undofile = true
opt.undolevels = 10000

-- Completion
opt.completeopt = { "menu", "menuone", "noselect" }
