local cmp = require('cmp')

cmp.setup({
  sources = cmp.config.sources({
    { name = 'buffer' },
    { name = 'nvim_lsp' },
    { name = 'nvim_lua' },
    { name = 'path' }
  })
})
