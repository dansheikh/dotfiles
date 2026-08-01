-- Diagnostic Configuration (Neovim 0.12+)
-- Signs must be configured via vim.diagnostic.config() — :sign-define removed.

local sev = vim.diagnostic.severity

local signs = {
  [sev.ERROR] = '󰈸',
  [sev.WARN]  = '',
  [sev.INFO]  = '',
  [sev.HINT]  = '',
}

vim.diagnostic.config({
  severity_sort    = true,
  update_in_insert = false,
  underline        = true,
  virtual_text     = false, -- shown in float instead
  signs = {
    text = {
      [sev.ERROR] = signs[sev.ERROR],
      [sev.WARN]  = signs[sev.WARN],
      [sev.INFO]  = signs[sev.INFO],
      [sev.HINT]  = signs[sev.HINT],
    },
    linehl = {
      [sev.ERROR] = 'DiagnosticLineError',
      [sev.WARN]  = 'DiagnosticLineWarn',
      [sev.INFO]  = 'DiagnosticLineInfo',
      [sev.HINT]  = 'DiagnosticLineHint',
    },
    numhl = {
      [sev.ERROR] = 'DiagnosticSignError',
      [sev.WARN]  = 'DiagnosticSignWarn',
      [sev.INFO]  = 'DiagnosticSignInfo',
      [sev.HINT]  = 'DiagnosticSignHint',
    },
  },
  float = {
    focusable = false,
    style     = 'minimal',
    border    = 'rounded',
    source    = true,  -- 0.12: boolean (replaces deprecated string 'always')
    header    = '',
    prefix    = function(diagnostic, i, _total)
      local icon = signs[diagnostic.severity] or ''
      local hl   = ({
        [sev.ERROR] = 'DiagnosticError',
        [sev.WARN]  = 'DiagnosticWarn',
        [sev.INFO]  = 'DiagnosticInfo',
        [sev.HINT]  = 'DiagnosticHint',
      })[diagnostic.severity] or 'Normal'
      return string.format('%d. %s ', i, icon), hl
    end,
  },
})

-- Diagnostic line background highlights (catppuccin mocha palette)
local hl = function(name, opts) vim.api.nvim_set_hl(0, name, opts) end
hl('DiagnosticLineError', { bg = '#3d2626' })
hl('DiagnosticLineWarn',  { bg = '#3d3526' })
hl('DiagnosticLineInfo',  { bg = '#263d3d' })
hl('DiagnosticLineHint',  { bg = '#2d2d2d' })

-- Sign column text colors
hl('DiagnosticSignError', { fg = '#f38ba8', bg = 'NONE' })
hl('DiagnosticSignWarn',  { fg = '#f9e2af', bg = 'NONE' })
hl('DiagnosticSignInfo',  { fg = '#89dceb', bg = 'NONE' })
hl('DiagnosticSignHint',  { fg = '#a6adc8', bg = 'NONE' })

return signs
