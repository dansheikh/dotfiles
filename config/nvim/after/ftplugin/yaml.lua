local opt = vim.opt_local
opt.shiftwidth = 2
opt.softtabstop = 2

dofile(os.getenv('XDG_CONFIG_HOME') .. '/nvim/lua/lib/efm_config.lua')
