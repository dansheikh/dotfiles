require('nightfox').setup({
  options = {
    styles = {
      comments = 'italic',
      keywords = 'bold',
      types = 'bold,italic'
    }
  }
})

vim.cmd('colorscheme nightfox')
