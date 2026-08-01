-- Editor Enhancements: Treesitter and Mini.nvim modules
return {
  {
    -- nvim-treesitter main branch (0.12+ rewrite): parser + query installer only.
    -- Highlighting and indentation are handled by Neovim natively; see autocmds.lua.
    -- Requires tree-sitter-cli installed system-wide (nix/homebrew, not npm).
    --
    -- Bundled in Neovim 0.12 core (no install needed):
    --   bash, c, lua, markdown, markdown_inline, python, query, vim, vimdoc
    'nvim-treesitter/nvim-treesitter',
    branch = 'main',
    build = ':TSUpdate',
    lazy = false,
    init = function()
      local ts = require('nvim-treesitter')
      local cfg = require('nvim-treesitter.config')
      local installed = cfg.get_installed()
      local wanted = {
        'clojure', 'css', 'dockerfile', 'go', 'html',
        'javascript', 'json', 'kotlin', 'nix', 'regex', 'rust',
        'sql', 'typescript', 'yaml',
      }
      local missing = vim.iter(wanted)
        :filter(function(p) return not vim.tbl_contains(installed, p) end)
        :totable()
      if #missing > 0 then
        ts.install(missing)
      end
    end,
  },

  {
    'nvim-mini/mini.nvim',
    branch = 'stable',
    config = function()
      -- AI: Enhanced text objects
      require('mini.ai').setup({
        n_lines = 500,
        search_method = 'cover_or_next',
      })

      -- Align: Align text interactively (ga / gA)
      require('mini.align').setup()

      -- Bracketed: Navigate with ]b, [b, ]d, [d, etc.
      require('mini.bracketed').setup()

      -- Bufremove: Close buffers without destroying window layout
      require('mini.bufremove').setup()

      -- Clue: Key binding hints (replaces which-key.nvim)
      local clue = require('mini.clue')
      clue.setup({
        triggers = {
          -- Leader
          { mode = 'n', keys = '<leader>' },
          { mode = 'x', keys = '<leader>' },
          -- Localleader (Conjure)
          { mode = 'n', keys = '<localleader>' },
          { mode = 'x', keys = '<localleader>' },
          -- Quotes prefix (LSP actions)
          { mode = 'n', keys = "'" },
          { mode = 'x', keys = "'" },
          -- g prefix
          { mode = 'n', keys = 'g' },
          { mode = 'x', keys = 'g' },
          -- Bracket navigation
          { mode = 'n', keys = ']' },
          { mode = 'n', keys = '[' },
          -- Window management
          { mode = 'n', keys = '<C-w>' },
          -- Surround
          { mode = 'n', keys = 's' },
          { mode = 'x', keys = 's' },
          -- Built-in completions
          { mode = 'i', keys = '<C-x>' },
          -- Marks
          { mode = 'n', keys = "'" },
          { mode = 'n', keys = '`' },
          -- Registers
          { mode = 'n', keys = '"' },
          { mode = 'x', keys = '"' },
        },
        clues = {
          -- Leader groups
          { mode = 'n', keys = '<leader>b', desc = '+buffers' },
          { mode = 'n', keys = '<leader>e', desc = '+explore' },
          { mode = 'n', keys = '<leader>g', desc = '+git' },
          -- Built-in clue sets
          clue.gen_clues.builtin_completion(),
          clue.gen_clues.g(),
          clue.gen_clues.marks(),
          clue.gen_clues.registers(),
          clue.gen_clues.windows(),
          clue.gen_clues.z(),
        },
        window = {
          config = {
            border = 'rounded',
          },
          delay = 300,
        },
      })

      -- Comment: Toggle comments
      require('mini.comment').setup()

      -- Cursorword: Autohighlight word under cursor
      require('mini.cursorword').setup()

      -- Diff: Git diff signs in sign column
      require('mini.diff').setup({
        view = {
          style = vim.go.number and 'number' or 'sign',
          signs = {
            add = '▎',
            change = '▎',
            delete = '➤',
          },
          priority = 199,
        },
      })

      -- Extra: Extra pickers and utilities for mini.pick / mini.ai
      require('mini.extra').setup()

      -- Git: Git integration commands
      require('mini.git').setup()

      -- Hipatterns: Highlight hex colors, TODO/FIXME/HACK/NOTE
      local hipatterns = require('mini.hipatterns')
      hipatterns.setup({
        highlighters = {
          fixme = { pattern = '%f[%w]()FIXME()%f[%W]', group = 'MiniHipatternsFixme' },
          hack  = { pattern = '%f[%w]()HACK()%f[%W]',  group = 'MiniHipatternsHack'  },
          todo  = { pattern = '%f[%w]()TODO()%f[%W]',  group = 'MiniHipatternsTodo'  },
          note  = { pattern = '%f[%w]()NOTE()%f[%W]',  group = 'MiniHipatternsNote'  },
          hex_color = hipatterns.gen_highlighter.hex_color(),
        },
      })

      -- Icons: Icon provider
      require('mini.icons').setup()

      -- Indentscope: Visualize indent scope
      require('mini.indentscope').setup({
        symbol = '┊',
        options = { try_as_border = true },
        draw = {
          delay = 0,
          animation = require('mini.indentscope').gen_animation.none(),
        },
      })

      -- Notify: Non-blocking notifications via vim.notify
      local notify = require('mini.notify')
      notify.setup({
        window = {
          config = {
            border = 'rounded',
          },
        },
      })
      vim.notify = notify.make_notify()

      -- Operators: Text edit operators (gr = replace with register, gm = duplicate, gx = exchange, g= = evaluate)
      require('mini.operators').setup()

      -- Pairs: Auto-pair brackets
      require('mini.pairs').setup()

      -- Files: File explorer
      require('mini.files').setup({
        content = {
          filter = nil,
          prefix = nil,
          sort = nil,
        },
        mappings = {
          close = 'q',
          go_in = 'l',
          go_in_plus = '<CR>',
          go_out = 'h',
          go_out_plus = 'H',
          reset = '<BS>',
          reveal_cwd = '@',
          show_help = 'g?',
          synchronize = '=',
          trim_left = '<',
          trim_right = '>',
        },
        options = {
          permanent_delete = true,
          use_as_default_explorer = false,
        },
        windows = {
          max_number = 3,
          preview = true,
          width_focus = 30,
          width_nofocus = 15,
          width_preview = 50,
        },
      })

      -- Pick: Fuzzy picker
      require('mini.pick').setup({
        window = {
          config = function()
            local height = math.floor(0.618 * vim.o.lines)
            local width = math.floor(0.618 * vim.o.columns)
            return {
              anchor = 'NW',
              height = height,
              width = width,
              row = math.floor(0.5 * (vim.o.lines - height)),
              col = math.floor(0.5 * (vim.o.columns - width)),
              border = 'rounded',
            }
          end,
        },
      })

      -- Splitjoin: Split and join arguments (gS = split, gJ = join)
      require('mini.splitjoin').setup()

      -- Statusline: Status line
      require('mini.statusline').setup({
        use_icons = true,
        set_vim_settings = false,
        content = {
          active = function()
            local mode, mode_hl = MiniStatusline.section_mode({ trunc_width = 120 })
            local git = MiniStatusline.section_git({ trunc_width = 75 })
            -- 0.12: vim.diagnostic.status() and vim.lsp.status() replace manual counting
            local diagnostics = vim.diagnostic.status() or ''
            local lsp_status  = vim.lsp.status() or ''
            local filename = MiniStatusline.section_filename({ trunc_width = 140 })
            local fileinfo = MiniStatusline.section_fileinfo({ trunc_width = 120 })
            local location = MiniStatusline.section_location({ trunc_width = 75 })

            return MiniStatusline.combine_groups({
              { hl = mode_hl, strings = { mode } },
              { hl = 'MiniStatuslineDevinfo', strings = { git, diagnostics, lsp_status } },
              '%<',
              { hl = 'MiniStatuslineFilename', strings = { filename } },
              '%=',
              { hl = 'MiniStatuslineFileinfo', strings = { fileinfo } },
              { hl = mode_hl, strings = { location } },
            })
          end,
        },
      })

      -- Surround: Surround text objects
      require('mini.surround').setup({
        mappings = {
          add = 'sa',
          delete = 'sd',
          find = 'sf',
          find_left = 'sF',
          highlight = 'sh',
          replace = 'sr',
          update_n_lines = 'sn',
        },
      })

      -- Tabline: Buffer/tab line
      require('mini.tabline').setup()

      -- Trailspace: Highlight and trim trailing whitespace
      require('mini.trailspace').setup()
    end,
  },
}
