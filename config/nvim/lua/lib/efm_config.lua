local capabilities = require('cmp_nvim_lsp').default_capabilities()
local config = {
  autostart = true,
  capabilities = capabilities,
  cmd = { 'efm-langserver', '-c', os.getenv('XDG_CONFIG_HOME') .. '/efm-langserver/config.yaml' },
  name = 'efm_ls',
  root_dir = vim.fs.root(0,
    { 'flake.lock',
      'flake.nix',
      '.git',
      'setup.cfg',
      'setup.py',
      'tsconfig.json',
      'package.json',
      'package-lock.json',
      'poetry.lock',
      'pyproject.toml',
      'requirements.txt',
      'yarn.lock'
    })
}

vim.lsp.start(config, {
  reuse_client = function(client, conf)
    return (
      client.name == conf.name and client.config.root_dir == conf.root_dir
    )
  end
})
