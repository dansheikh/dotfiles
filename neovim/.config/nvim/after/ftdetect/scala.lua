local group = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
local scala = group('Scala', { clear = true })

autocmd({ 'BufNew', 'BufRead' }, {
  pattern = { '*.scala', '*.sbt', '*.sc' },
  callback = function()
    vim.schedule(function()
      vim.bo.filetype = 'scala'
    end)
  end,
  group = scala
})
