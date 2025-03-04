return {
  'hrsh7th/nvim-cmp',
  dependencies = {
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-nvim-lsp-signature-help',
    'hrsh7th/cmp-nvim-lua',
    'hrsh7th/cmp-path',
    'onsails/lspkind.nvim',
    'ray-x/cmp-treesitter',
    'R-nvim/cmp-r',
    'saadparwaiz1/cmp_luasnip'
  },
  config = function()
    local cmp = require('cmp')
    local luasnip = require('luasnip')
    local lspkind = require('lspkind')

    cmp.setup({
      formatting = {
        format = lspkind.cmp_format({
          mode = 'text',
          preset = 'default',
          before = function(entry, item)
            item.menu = ({
              buffer = '',
              luasnip = '',
              nvim_lsp = '',
              path = '',
              treesitter = ''
            })[entry.source.name]

            return item
          end
        })
      },
      mapping = cmp.mapping.preset.insert({
        ['<C-a>'] = cmp.mapping.abort(),
        ['<C-c>'] = cmp.mapping.complete(),
        ['<cr>'] = cmp.mapping.confirm({ select = true }),
        ['<C-d>'] = cmp.mapping.scroll_docs(-5),
        ['<C-u>'] = cmp.mapping.scroll_docs(5),
        ['<down>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
        ['<up>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select })
      }),
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end
      },
      sources = cmp.config.sources(
        {
          { name = 'nvim_lsp' },
          { name = 'treesitter' },
          { name = 'cmp-nvim-lsp-signature-help' },
          { name = 'luasnip' },
          { name = 'nvim_lua' },
          { name = 'cmp_r' }
        },
        {
          { name = 'buffer' },
          { name = 'path' }
        })
    })
  end
}
