return {
  'catppuccin/nvim',
  name = 'catppuccin',
  config = function()
    require('catppuccin').setup({
      options = {
        styles = {
          comments = 'italic',
          keywords = 'bold',
          types = 'bold,italic'
        }
      }
    })

    vim.cmd('colorscheme catppuccin-mocha')
  end
}
