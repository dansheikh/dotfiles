local config = vim.diagnostic.config
local signs = { Error = '', Warn = '', Hint = '', Info = '' }

config({
  virtual_text = false
})

for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
