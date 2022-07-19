local autocmd = vim.api.nvim_create_autocmd
local execute = vim.api.nvim_command
local group = vim.api.nvim_create_augroup

local on_save = group('OnSave', { clear = true })
local check = group('Check', { clear = true })

autocmd({ 'BufEnter', 'FocusGained' }, {
  callback = function ()
    vim.schedule(function ()
      execute('checktime')
    end)
  end,
  group = check
})

autocmd('BufWritePre', {
  callback = function ()
    vim.schedule(function ()
      vim.lsp.buf.formatting_sync(nil, 1000)
    end)
  end,
  group = on_save
})
