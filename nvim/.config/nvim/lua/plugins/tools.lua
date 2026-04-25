-- Tools: FzfLua, Oil, Flash, EditorConfig (native)
return {
  {
    'ibhagwan/fzf-lua',
    dependencies = { 'echasnovski/mini.nvim' },
    cmd = 'FzfLua',
    keys = {
      { '<leader><leader>', '<cmd>FzfLua files<cr>', desc = 'Find Files' },
      { '<leader>/', '<cmd>FzfLua live_grep<cr>', desc = 'Search' },
      { '<leader>,', '<cmd>FzfLua buffers<cr>', desc = 'Buffers' },
      { '<leader>.', '<cmd>FzfLua oldfiles<cr>', desc = 'Recent Files' },
    },
    opts = function()
      local actions = require('fzf-lua.actions')
      return {
        'default-title',
        winopts = {
          height = 0.85,
          width = 0.80,
          row = 0.35,
          col = 0.50,
          border = 'rounded',
          preview = {
            border = 'border',
            wrap = 'nowrap',
            hidden = 'nohidden',
            vertical = 'down:45%',
            horizontal = 'right:50%',
            layout = 'flex',
            flip_columns = 120,
          },
        },
        fzf_opts = {
          ['--layout'] = 'reverse',
          ['--info'] = 'inline',
        },
        files = {
          prompt = 'Files❯ ',
          actions = {
            ['ctrl-g'] = actions.toggle_ignore,
          },
        },
        grep = {
          prompt = 'Grep❯ ',
          actions = {
            ['ctrl-g'] = actions.toggle_ignore,
          },
        },
        lsp = {
          symbols = {
            symbol_style = 1,
          },
        },
      }
    end,
  },

  {
    'stevearc/oil.nvim',
    cmd = 'Oil',
    keys = {
      { '<leader>eo', '<cmd>Oil<cr>', desc = 'File Browser (oil)' },
    },
    opts = {
      default_file_explorer = true,
      columns = {
        'icon',
        'permissions',
        'size',
        'mtime',
      },
      view_options = {
        show_hidden = true,
        is_hidden_file = function(name, bufnr)
          return vim.startswith(name, '.')
        end,
      },
      float = {
        padding = 2,
        max_width = 90,
        max_height = 30,
        border = 'rounded',
      },
      keymaps = {
        ['g?'] = 'actions.show_help',
        ['<CR>'] = 'actions.select',
        ['<C-s>'] = 'actions.select_vsplit',
        ['<C-h>'] = 'actions.select_split',
        ['<C-t>'] = 'actions.select_tab',
        ['<C-p>'] = 'actions.preview',
        ['<C-c>'] = 'actions.close',
        ['<C-r>'] = 'actions.refresh',
        ['-'] = 'actions.parent',
        ['_'] = 'actions.open_cwd',
        ['`'] = 'actions.cd',
        ['~'] = 'actions.tcd',
        ['gs'] = 'actions.change_sort',
        ['gx'] = 'actions.open_external',
        ['g.'] = 'actions.toggle_hidden',
        ['g\\'] = 'actions.toggle_trash',
      },
    },
  },

  {
    'folke/flash.nvim',
    event = 'VeryLazy',
    opts = {
      labels = 'asdfghjklqwertyuiopzxcvbnm',
      search = {
        multi_window = true,
        forward = true,
        wrap = true,
        mode = 'exact',
      },
      jump = {
        jumplist = true,
        pos = 'start',
        history = false,
        register = false,
        nohlsearch = true,
      },
      label = {
        uppercase = true,
        rainbow = {
          enabled = false,
          shade = 5,
        },
      },
      modes = {
        search = {
          enabled = true,
        },
        char = {
          enabled = false,  -- Native f/F/t/T preserved
        },
      },
    },
    keys = {
      -- 's' is safe: mini.surround uses sa/sd/sr, not bare s
      { 's', mode = { 'n', 'x', 'o' }, function() require('flash').jump() end, desc = 'Flash Jump' },
      { 'S', mode = { 'n', 'x', 'o' }, function() require('flash').treesitter() end, desc = 'Flash Treesitter' },
      { 'r', mode = 'o', function() require('flash').remote() end, desc = 'Remote Flash' },
      { 'R', mode = { 'o', 'x' }, function() require('flash').treesitter_search() end, desc = 'Treesitter Search' },
      { '<c-s>', mode = { 'c' }, function() require('flash').toggle() end, desc = 'Toggle Flash Search' },
    },
  },
}
