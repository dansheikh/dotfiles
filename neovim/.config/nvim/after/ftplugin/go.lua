local capabilities = require('cmp_nvim_lsp').default_capabilities()
local opt = vim.opt_local
opt.shiftwidth = 2
opt.softtabstop = 2

dofile(os.getenv('XDG_CONFIG_HOME') .. '/nvim/lua/lib/efm_config.lua')

local config = {
  autostart = true,
  capabilities = capabilities,
  cmd = { 'gopls' },
  name = 'gopls',
  root_dir = vim.fs.root(0, { '.git', 'go.mod', 'go.sum' })
}

vim.lsp.start(config, {
  reuse_client = function(client, conf)
    return (
      client.name == conf.name and client.config.root_dir == conf.root_dir
    )
  end
})
