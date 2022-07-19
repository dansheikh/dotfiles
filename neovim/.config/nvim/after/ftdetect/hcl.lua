local group = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

local hcl = group('HCL', { clear = true })

autocmd({ 'BufNew', 'BufRead' }, {
  pattern = { '*.hcl', '*.tf', '*.tfvars' },
  callback = function ()
    vim.schedule(function ()
      vim.bo.filetype = 'hcl'
    end)
  end,
  group = hcl
})
