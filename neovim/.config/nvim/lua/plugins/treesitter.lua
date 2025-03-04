return {
  'nvim-treesitter/nvim-treesitter',
  dependencies = {
    { 'p00f/nvim-ts-rainbow' },
    { 'RRethy/nvim-treesitter-endwise' }
  },
  build = ':TSUpdate',
  config = function()
    require('nvim-treesitter.configs').setup({
      auto_install = true,
      autopairs = {
        enable = true
      },
      endwise = {
        enable = true
      },
      ensure_installed = {
        "bash",
        "clojure",
        "css",
        "dart",
        "dockerfile",
        "fish",
        "go",
        "graphql",
        "hcl",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "nix",
        "python",
        "proto",
        "r",
        "rnoweb",
        "sql",
        "toml",
        "typescript",
        "yaml"
      },
      highlight = {
        enable = true
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = '<cr>',
          scope_incremental = '<cr>',
          node_incremental = '<tab>',
          node_decremental = '<s-tab>'
        }
      },
      indent = {
        enable = true
      },
      rainbow = {
        enable = true,
        extended_mode = true,
        max_file_lines = nil
      },
      sync_install = false
    })
  end
}
