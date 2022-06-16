require('nvim-treesitter.configs').setup({
  autopairs = {
    enable = true
  },
  endwise = {
    enable = true
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
  }
})
