return {
  'akinsho/bufferline.nvim',
  config = function()
    require('bufferline').setup({
      options = {
        mode = 'buffers',
        numbers = 'buffer_id'
      }
    })
  end
}
