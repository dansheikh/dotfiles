local fn = vim.fn
local get_config = require('lib.utility').get_config
local lazypath = fn.stdpath("data") .. "/lazy/lazy.nvim"
local loop = vim.loop
local opt = vim.opt

if not loop.fs_stat(lazypath) then
  fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end

opt.rtp:prepend(lazypath)

require('lazy').setup({ { import = 'plugins' } })
