-- Editor Options (Neovim 0.12+)
local opt = vim.opt

-- Disable netrw (using oil.nvim)
vim.g.loaded_netrw       = 1
vim.g.loaded_netrwPlugin = 1

-- Indentation
opt.autoindent  = true
opt.smartindent = true
opt.smarttab    = true
opt.expandtab   = true
opt.shiftwidth  = 2
opt.tabstop     = 2
opt.softtabstop = 0
opt.shiftround  = true

-- UI
opt.number         = true
opt.relativenumber = true
opt.signcolumn     = 'yes:2'
opt.cursorline     = false
opt.showmode       = false
opt.termguicolors  = true
opt.title          = true
opt.fillchars      = 'eob: '
opt.listchars      = 'tab:▸ ,trail:·'
opt.list           = true

-- Cursor
opt.guicursor = 'n-v-c:block-nCursor-blinkwait300-blinkon300-blinkoff300,'
  .. 'i-ci-ve:ver20-iCursor-blinkwait300-blinkon150-blinkoff150,'
  .. 'r-cr-ve:ver20-Cursor-blinkwait300-blinkon150-blinkoff150'

-- Scrolling
opt.scrolloff     = 5
opt.sidescrolloff = 5

-- Search
opt.ignorecase = true
opt.smartcase  = true

-- Splits
opt.splitbelow = true
opt.splitright = true

-- Completion
-- 0.12: disable native autocomplete (we use nvim-cmp for the full UI)
opt.autocomplete = false
-- 0.12: popup border and width options
opt.pumborder   = 'rounded'
opt.pummaxwidth = 40
opt.pumheight   = 10
opt.wildmode    = 'longest:full,full'

-- Files
opt.backup    = false
opt.swapfile  = false
opt.undofile  = true

-- 0.12: shelltemp defaults to false; make it explicit
opt.shelltemp = false

-- Misc
opt.clipboard   = 'unnamedplus'
opt.confirm     = true
opt.mouse       = 'a'
opt.updatetime  = 300   -- CursorHold delay for diagnostic float
opt.redrawtime  = 10000
opt.spell       = false -- enabled per-filetype in after/ftplugin/
opt.wrap        = false
opt.timeoutlen  = 300   -- mini.clue popup
