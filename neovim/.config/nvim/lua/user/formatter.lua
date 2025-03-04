return {
  'mhartington/formatter.nvim',
  config = function()
    require('formatter').setup({
      logging = true,
      log_level = vim.log.levels.WARN,
      filetype = {
        javascript = {
          require('formatter.filetype.javascript').prettier
        },
        json = {
          require('formatter.filetype.json').prettier
        },
        markdown = {
          require('formatter.filetype.markdown').prettier
        },
        nix = {
          require('formatter.filetype.nix').nixfmt
        },
        python = {
          require('formatter.filetype.python').black
        },
        terraform = {
          require('formatter.filetype.terraform').terraformfmt
        },
        typescript = {
          require('formatter.filetype.typescript').prettier
        }
      }
    })
  end
}
