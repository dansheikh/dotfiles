local cmp = require('cmp')
local luasnip = require('luasnip')
local lspkind = require('lspkind')

cmp.setup({
  formatting = {
    format = function(entry, item)
      item.kind = string.format('%s %s', lspkind.presets.default[item.kind], item.kind)
      item.menu = ({
        buffer = '',
        luasnip = '',
        nvim_lsp = '',
        path = '',
        treesitter = ''
      })[entry.source.name]

      return item
    end
  },
  mapping = cmp.mapping.preset.insert({
    ['<c-e>'] = cmp.mapping.abort(),
    ['<c-c>'] = cmp.mapping.complete(),
    ['<cr>'] = cmp.mapping.confirm({ select = true }),
    ['<c-d>'] = cmp.mapping.scroll_docs(-5),
    ['<c-u>'] = cmp.mapping.scroll_docs(5),
    ['<down>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
    ['<up>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select })
  }),
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end
  },
  sources = cmp.config.sources({
    { name = 'buffer' },
    { name = 'luasnip' },
    { name = 'nvim_lsp' },
    { name = 'nvim_lua' },
    { name = 'path' },
    { name = 'treesitter' }
  })
})
