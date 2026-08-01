return {
  -- Yazi File Manager Integration
  {
    "mikavilpas/yazi.nvim",
    cmd = { "Yazi" },
    keys = {
      {
        "<leader>-",
        function()
          require("yazi").yazi()
        end,
        desc = "Open Yazi at current file",
      },
      {
        "<leader>cw",
        function()
          require("yazi").yazi(nil, vim.fn.getcwd())
        end,
        desc = "Open Yazi in CWD",
      },
    },
    opts = {
      open_for_directories = true,
      keymaps = {
        show_help = "<f1>",
        open_file_in_vertical_split = "<c-v>",
        open_file_in_horizontal_split = "<c-x>",
        open_file_in_tab = "<c-t>",
        grep_in_directory = "<c-s>",
        replace_in_directory = "<c-g>",
        cycle_open_buffers = "<tab>",
      },
      floating_window_scaling_factor = 0.85,
      yazi_floating_window_border = "rounded",
    },
  },

  -- Clean line/block commenting
  {
    "folke/ts-comments.nvim",
    event = "VeryLazy",
    opts = {},
  },

  -- Fast bidirectional motions (Flash)
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {},
    keys = {
      -- Keep 's' active in all modes for standard jumping
      {
        "s",
        mode = { "n", "x", "o" },
        function()
          require("flash").jump()
        end,
        desc = "Flash Jump",
      },
      -- Restrict 'S' to Normal and Operator-pending so Visual 'S' is free for surround
      {
        "S",
        mode = { "n", "o" },
        function()
          require("flash").treesitter()
        end,
        desc = "Flash Treesitter",
      },
    },
  },
}
