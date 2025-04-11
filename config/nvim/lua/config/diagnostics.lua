local config = vim.diagnostic.config
local signs = {
  [vim.diagnostic.severity.ERROR] = '',
  [vim.diagnostic.severity.HINT] = '',
  [vim.diagnostic.severity.INFO] = '',
  [vim.diagnostic.severity.WARN] = ''
}

config({
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = signs[vim.diagnostic.severity.ERROR],
      [vim.diagnostic.severity.HINT] = signs[vim.diagnostic.severity.HINT],
      [vim.diagnostic.severity.INFO] = signs[vim.diagnostic.severity.INFO],
      [vim.diagnostic.severity.WARN] = signs[vim.diagnostic.severity.WARN]
    },
    linehl = {
      [vim.diagnostic.severity.ERROR] = 'ErrorMsg'
    },
    numhl = {
      [vim.diagnostic.severity.WARN] = 'WarningMsg'
    }
  },
  virtual_text = true,
})

-- for type, icon in pairs(signs) do
--   local hl = "DiagnosticSign" .. type
--   vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
-- end
