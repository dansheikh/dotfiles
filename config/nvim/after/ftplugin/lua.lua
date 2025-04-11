local capabilities = require('cmp_nvim_lsp').default_capabilities()
local opt = vim.opt_local
opt.shiftwidth = 2
opt.softtabstop = 2

local config = {
  autostart = true,
  capabilities = capabilities,
  cmd = { 'lua-language-server' },
  name = 'lua_ls',
  root_dir = vim.fs.root(0,
    { '.git', '.luarc.json', '.luarc.jsonc', '.luacheckrc', '.stylua.toml', 'selene.toml', 'selene.yml', 'stylua.toml' })
}

vim.lsp.start(config, {
  reuse_client = function(client, conf)
    return (
      client.name == conf.name and client.config.root_dir == conf.root_dir
    )
  end
})
