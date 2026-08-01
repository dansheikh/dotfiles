local M = {}

function M.setup()
  local blink = require("blink.cmp")
  local capabilities = blink.get_lsp_capabilities()

  -- 1. Native LspAttach autocommand for Neovim 0.12+
  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("UserLspConfig", { clear = true }),
    callback = function(event)
      local client = vim.lsp.get_client_by_id(event.data.client_id)
      local bufnr = event.buf

      if client then
        -- Strict delegation: formatting is handled entirely by conform.nvim
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

  -- 2. Server configurations
  local servers = {
    -- EFM Langserver: Reads configuration directly from ~/.config/efm-langserver/config.yaml
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

    -- TypeScript language server
    ts_ls = {
      filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
      init_options = {
        preferences = {
          disableSuggestions = false,
          quotePreference = "auto",
          includeCompletionsForModuleExports = true,
          includeCompletionsForImportStatements = true,
          importModuleSpecifierPreference = "non-relative",
          allowIncompleteCompletions = true,
        },
      },
      settings = {
        typescript = {
          inlayHints = {
            includeInlayParameterNameHints = "all",
            includeInlayParameterNameHintsWhenArgumentMatchesName = false,
            includeInlayFunctionParameterTypeHints = true,
            includeInlayVariableTypeHints = true,
            includeInlayPropertyDeclarationTypeHints = true,
            includeInlayFunctionLikeReturnTypeHints = true,
            includeInlayEnumMemberValueHints = true,
          },
        },
        javascript = {
          inlayHints = {
            includeInlayParameterNameHints = "all",
            includeInlayFunctionParameterTypeHints = true,
            includeInlayVariableTypeHints = true,
          },
        },
      },
    },

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

  -- 3. Native Neovim 0.12+ registration via vim.lsp.config & vim.lsp.enable
  for server, config in pairs(servers) do
    config.capabilities = capabilities

    pcall(require, "lspconfig.configs." .. server)

    vim.lsp.config(server, config)
    vim.lsp.enable(server)
  end
end

return M
