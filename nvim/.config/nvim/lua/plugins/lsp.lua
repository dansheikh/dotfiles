-- LSP and Completion
-- Neovim 0.12+: servers configured via vim.lsp.config / vim.lsp.enable()
-- nvim-cmp for completion UI, LuaSnip for snippets

return {
  -- Snippet engine
  {
    'L3MON4D3/LuaSnip',
    version = '2.*',
    dependencies = { 'rafamadriz/friendly-snippets' },
    config = function()
      require('luasnip.loaders.from_vscode').lazy_load()
    end,
  },

  -- Completion UI
  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'saadparwaiz1/cmp_luasnip',
      'L3MON4D3/LuaSnip',
    },
    config = function()
      local cmp = require('cmp')
      local luasnip = require('luasnip')

      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        window = {
          completion    = cmp.config.window.bordered({ border = 'rounded' }),
          documentation = cmp.config.window.bordered({ border = 'rounded' }),
        },
        mapping = cmp.mapping.preset.insert({
          ['<Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_locally_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { 'i', 's' }),
          ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.locally_jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { 'i', 's' }),
          ['<C-b>']     = cmp.mapping.scroll_docs(-4),
          ['<C-f>']     = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>']     = cmp.mapping.abort(),
          ['<CR>']      = cmp.mapping.confirm({ select = false }),
        }),
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'luasnip' },
          { name = 'path' },
        }, {
          { name = 'buffer', keyword_length = 3 },
        }),
        completion = {
          completeopt = 'menu,menuone,noinsert',
        },
        experimental = {
          ghost_text = false,
        },
      })
    end,
  },

  {
    'b0o/schemastore.nvim',
    lazy = true,
  },

  -- Wire up LSP servers via the 0.12 vim.lsp.config / vim.lsp.enable() API.
  -- Capabilities are extended with cmp-nvim-lsp so completion works.
  -- Diagnostics config is applied once here after capability setup.
  {
    name = 'lsp-config',
    dir = vim.fn.stdpath('config') .. '/lua/lsp',
    dependencies = { 'hrsh7th/cmp-nvim-lsp' },
    config = function()
      local servers = require('lsp.servers')
      local capabilities = require('cmp_nvim_lsp').default_capabilities()

      for name, cfg in pairs(servers) do
        -- Build the vim.lsp.config table from our server spec
        local lsp_cfg = {
          cmd          = cfg.cmd,
          filetypes    = cfg.filetypes,
          capabilities = capabilities,
        }

        -- root_markers takes precedence; custom root_dir function is a fallback
        if cfg.root_markers then
          lsp_cfg.root_markers = cfg.root_markers
        elseif cfg.root_dir then
          lsp_cfg.root_dir = cfg.root_dir
        end

        if cfg.settings and next(cfg.settings) ~= nil then
          lsp_cfg.settings = cfg.settings
        end

        if cfg.init_options and next(cfg.init_options) ~= nil then
          lsp_cfg.init_options = cfg.init_options
        end

        vim.lsp.config(name, lsp_cfg)
        vim.lsp.enable(name)
      end

      -- Suppress formatting on attach for servers that aren't formatters.
      -- efm, rust_analyzer, clojure_lsp handle formatting; everyone else does not.
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('LspDisableFormatting', { clear = true }),
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if not client then return end
          local formatters = { efm = true, rust_analyzer = true, clojure_lsp = true }
          if not formatters[client.name] then
            client.server_capabilities.documentFormattingProvider = false
            client.server_capabilities.documentRangeFormattingProvider = false
          end
        end,
        desc = 'Disable formatting on non-formatter LSP clients',
      })

      -- Skip conjure log buffers
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('LspSkipConjure', { clear = true }),
        callback = function(args)
          local name = vim.api.nvim_buf_get_name(args.buf)
          if name:match('conjure%-log') then
            vim.lsp.buf_detach_client(args.buf, args.data.client_id)
          end
        end,
        desc = 'Detach LSP from Conjure log buffers',
      })

      require('config.diagnostics')
    end,
  },
}
