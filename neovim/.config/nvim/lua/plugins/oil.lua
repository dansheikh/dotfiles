return {
  'stevearc/oil.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    require('oil').setup({
      columns = {
        'icon',
      },
      default_file_explorer = true,
      delete_to_trash = true,
      skip_confirm_for_simple_edits = true,
      view_options = {
        show_hidden = true
      },
      win_options = {
        wrap = true
      }
    })
  end
}
