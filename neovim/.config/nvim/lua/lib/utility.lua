local function map(mode, lhs, rhs, opts)
  local config = { noremap = true, silent = true }

  if opts then
    config = vim.tbl_extend('force', config, opts)
  end

  vim.api.nvim_set_keymap(mode, lhs, rhs, config)
end

local function get_config(name)
  return string.format("require('config/%s')", name)
end

return {
  map = map,
  get_config = get_config
}
