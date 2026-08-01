local M = {}

function M.setup()
  local blink = require("blink.cmp")
  local capabilities = blink.get_lsp_capabilities()

  -- 1. Use the native LspAttach event instead of deprecated on_attach hooks
  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("UserLspConfig", { clear = true }),
    callback = function(event)
      local client = vim.lsp.get_client_by_id(event.data.client_id)
      local bufnr = event.buf

      if client then
        -- Formatting delegated exclusively to conform.nvim
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentRangeFormattingProvider = false
      end

      local map = function(keys, func, desc)
        vim.keymap.set("n", keys, func, { buffer = bufnr, desc = "LSP: " .. desc })
      end

      map("gd", vim.lsp.buf.definition, "Goto Definition")
      map("gr", vim.lsp.buf.references, "Goto References")
      map("gI", vim.lsp.buf.implementation, "Goto Implementation")
      map("K", vim.lsp.buf.hover, "Hover Documentation")
      map("<leader>cr", vim.lsp.buf.rename, "Rename Symbol")
      map("<leader>ca", vim.lsp.buf.code_action, "Code Action")
    end,
  })

  -- 2. Define server configurations
  local servers = {
    efm = {
      filetypes = {
        "go",
        "javascript",
        "typescript",
        "javascriptreact",
        "typescriptreact",
        "json",
        "python",
        "sql",
        "sh",
        "bash",
        "zsh",
        "yaml",
        "markdown",
        "dockerfile",
        "toml",
        "vue",
        "svelte",
        "kotlin",
      },
      init_options = {
        documentFormatting = false,
        documentRangeFormatting = false,
      },
    },
    ts_ls = {},
    pyright = {},
    gopls = {},
    rust_analyzer = {},
    lua_ls = {
      settings = {
        Lua = {
          diagnostics = { globals = { "vim" } },
          workspace = { checkThirdParty = false },
        },
      },
    },
  }

  -- 3. Apply configurations via native vim.lsp.config (Neovim 0.11+)
  for server, config in pairs(servers) do
    config.capabilities = capabilities

    -- Ensure nvim-lspconfig populates the default commands and settings
    -- into the vim.lsp.config registry before we enable it
    pcall(require, "lspconfig.configs." .. server)

    vim.lsp.config(server, config)
    vim.lsp.enable(server)
  end
end

return M
