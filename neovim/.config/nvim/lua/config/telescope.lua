local telescope = require('telescope')
local actions = require('telescope.actions')
local actions_layout = require('telescope.actions.layout')

telescope.setup({
  extensions = {
    file_browser = {
      hijack_netrw = true
    },
    fzy_native = {
      case_mode = 'smart_case',
      fuzzy = true,
      override_file_sorter = true,
      override_generic_sorter = true
    }
  },
  mappings = {
    i = {
      ['<esc>'] = actions.close
    }
  }
})

telescope.load_extension('file_browser')
telescope.load_extension('fzy_native')

