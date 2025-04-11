local capabilities = require('cmp_nvim_lsp').default_capabilities()
local opt = vim.opt_local
opt.shiftwidth = 4
opt.softtabstop = 4

dofile(os.getenv('XDG_CONFIG_HOME') .. '/nvim/lua/lib/efm_config.lua')

local config = {
  autostart = true,
  capabilities = capabilities,
  cmd = { 'pyright-langserver', '--stdio' },
  name = 'pyright',
  root_dir = vim.fs.root(0, { '.git', 'setup.cfg', 'setup.py', 'poetry.lock', 'pyproject.toml', 'requirements.txt' })
}

vim.lsp.start(config, {
  reuse_client = function(client, conf)
    return (
      client.name == conf.name and client.config.root_dir == conf.root_dir
    )
  end
})
