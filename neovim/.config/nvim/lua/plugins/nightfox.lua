return {
  'EdenEast/nightfox.nvim',
  lazy = true,
  config = function()
    require('nightfox').setup({
      options = {
        styles = {
          comments = 'italic',
          keywords = 'bold',
          types = 'bold,italic'
        }
      }
    })
  end
}
