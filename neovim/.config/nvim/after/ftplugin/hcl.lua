local capabilities = require('cmp_nvim_lsp').default_capabilities()
local opt = vim.opt_local
opt.shiftwidth = 2
opt.softtabstop = 2

local config = {
  autostart = true,
  capabilities = capabilities,
  cmd = { 'terraform-ls', 'serve' },
  name = 'terraform_ls',
  root_dir = vim.fs.root(0, { '.git', '.terraform' })
}

vim.lsp.start(config, {
  reuse_client = function(client, conf)
    return (
      client.name == conf.name and client.config.root_dir == conf.root_dir
    )
  end
})
