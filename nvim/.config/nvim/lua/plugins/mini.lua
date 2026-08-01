return {
  {
    "echasnovski/mini.nvim",
    version = false,
    lazy = false,
    priority = 900,
    config = function()
      -- 1. Icons
      require("mini.icons").setup()
      require("mini.icons").mock_nvim_web_devicons()

      -- 2. Autopairs
      require("mini.pairs").setup()

      -- 3. Extended textobjects
      require("mini.ai").setup({ n_lines = 500 })

      -- 4. Surround (tpope standard: ys, ds, cs)
      require("mini.surround").setup({
        mappings = {
          add = "ys",
          delete = "ds",
          replace = "cs",
          find = "",
          find_left = "",
          highlight = "",
          update_n_lines = "",
        },
      })

      -- Visual mode 'S' mapping to wrap selections
      vim.keymap.set(
        "x",
        "S",
        [[:<C-u>lua MiniSurround.add('visual')<CR>]],
        { silent = true, desc = "Add surrounding" }
      )

      -- 5. Split-safe buffer delete
      require("mini.bufremove").setup()

      -- 6. Fuzzy Picker
      require("mini.pick").setup({
        window = {
          config = { border = "rounded" },
        },
      })

      -- 7. Keymap clue popup
      local miniclue = require("mini.clue")
      miniclue.setup({
        triggers = {
          { mode = "n", keys = "<Leader>" },
          { mode = "x", keys = "<Leader>" },
          { mode = "n", keys = "[" },
          { mode = "n", keys = "]" },
          { mode = "n", keys = "g" },
          { mode = "x", keys = "g" },
          { mode = "n", keys = "'" },
          { mode = "n", keys = "`" },
          { mode = "n", keys = '"' },
          { mode = "x", keys = '"' },
          { mode = "i", keys = "<C-r>" },
          { mode = "c", keys = "<C-r>" },
          { mode = "n", keys = "<C-w>" },
        },
        clues = {
          miniclue.gen_clues.builtin_completion(),
          miniclue.gen_clues.g(),
          miniclue.gen_clues.marks(),
          miniclue.gen_clues.registers(),
          miniclue.gen_clues.windows(),
          miniclue.gen_clues.z(),

          -- Custom leader groupings
          { mode = "n", keys = "<Leader><tab>", desc = "+Tabs" },
          { mode = "n", keys = "<Leader>b", desc = "+Buffer" },
          { mode = "n", keys = "<Leader>c", desc = "+Code / LSP" },
          { mode = "n", keys = "<Leader>f", desc = "+Find / Pick" },
          { mode = "n", keys = "<Leader>g", desc = "+Git" },
          { mode = "n", keys = "<Leader>s", desc = "+Search" },

          -- Document surround commands for the popup
          { mode = "n", keys = "ys", desc = "+Surround (Add)" },
          { mode = "n", keys = "ds", desc = "+Surround (Delete)" },
          { mode = "n", keys = "cs", desc = "+Surround (Replace)" },
        },
        window = {
          delay = 200,
          config = {
            border = "rounded",
          },
        },
      })
    end,
  },
}
