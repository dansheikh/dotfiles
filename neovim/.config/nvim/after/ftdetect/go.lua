local group = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
local hcl = group('Go', { clear = true })

autocmd({ 'BufNew', 'BufRead' }, {
  pattern = { '*.go', '*.gomod', '*.gowork', '*.gotmpl' },
  callback = function()
    vim.schedule(function()
      vim.bo.filetype = 'go'
    end)
  end,
  group = hcl
})
