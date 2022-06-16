local group = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

local on_save = group('OnSave', { clear = true })

autocmd('BufWritePre', {
  callback = function ()
    vim.schedule(function()
      vim.lsp.buf.formatting_sync(nil, 1000)
    end)
  end,
  group = 'OnSave'
})
