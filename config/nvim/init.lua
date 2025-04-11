local global = vim.g

global.mapleader = ' '
global.maplocalleader = ' '

require('config.lazy')
require('config.options')
require('config.keymaps')
require('config.diagnostics')
require('config.autocmds')
