-- Clojure Development Tools
return {
  {
    "Olical/conjure",
    ft = { "clojure", "fennel", "scheme", "racket" },
    dependencies = {},
    init = function()
      -- Set mappings prefix (default is localleader)
      vim.g["conjure#mapping#prefix"] = "<localleader>"
      
      -- Log configuration - use HUD (floating window) by default
      vim.g["conjure#log#hud#enabled"] = true            -- Enable floating HUD
      vim.g["conjure#log#hud#width"] = 0.42              -- HUD width (42% of screen)
      vim.g["conjure#log#hud#anchor"] = "SE"             -- Anchor to bottom-right
      vim.g["conjure#log#wrap"] = true
      vim.g["conjure#log#fold#enabled"] = false
      
      -- Disable diagnostics in REPL buffer
      vim.g["conjure#client#clojure#nrepl#eval#auto_require"] = false
      
      -- Better REPL experience
      vim.g["conjure#client#clojure#nrepl#connection#auto_repl#enabled"] = false
      vim.g["conjure#client#clojure#nrepl#test#runner"] = "kaocha"
    end,
    config = function()
      -- Simple: just use Conjure's built-in commands for opening log
      -- ,lq - close log
      -- ,ls - horizontal split
      -- ,lv - vertical split  
      -- ,lt - tab
      -- All work out of the box with HUD enabled
    end,
  },

  {
    "clojure-vim/vim-jack-in",
    ft = { "clojure" },
  },

  {
    "julienvincent/nvim-paredit",
    ft = { "clojure", "fennel", "scheme", "racket", "lisp" },
    config = function()
      require("nvim-paredit").setup({
        use_default_keys = true,
        filetypes = { "clojure", "fennel", "scheme", "racket", "lisp" },
      })
    end,
  },
}
