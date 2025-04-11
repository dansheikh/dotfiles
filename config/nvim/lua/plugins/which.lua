return {
  'folke/which-key.nvim',
  config = function()
    local which = require('which-key')

    which.setup()

    which.add(
      {
        { '<leader><leader>d',  group = 'Diagnostics' },
        { '<leader><leader>dn', vim.diagnostic.goto_next,                                                                  desc = 'Next' },
        { '<leader><leader>dp', vim.diagnostic.goto_prev,                                                                  desc = 'Previous' },
        { '<leader><leader>f',  group = 'Find' },
        { '<leader><leader>fb', '<cmd>FzfLua buffers<cr>',                                                                 desc = 'Buffers' },
        { '<leader><leader>ff', '<cmd>FzfLua files<cr>',                                                                   desc = 'Files' },
        { '<leader><leader>fg', '<cmd>FzfLua live_grep<cr>',                                                               desc = 'Grep' },
        { '<leader><leader>fl', '<cmd>FzfLua treesitter<cr>',                                                              desc = 'Treesitter' },
        { '<leader><leader>l',  group = 'Lsp' },
        { '<leader><leader>ld', '<cmd>FzfLua lsp_definitions ignore_current_line=true jump_to_single_result=true<cr>',     desc = 'Definitions' },
        { '<leader><leader>lh', vim.lsp.buf.hover,                                                                         desc = 'Hover' },
        { '<leader><leader>li', '<cmd>FzfLua lsp_implementations ignore_current_line=true jump_to_single_result=true<cr>', desc = 'Implementations' },
        { '<leader><leader>ln', vim.lsp.buf.rename,                                                                        desc = 'Rename' },
        { '<leader><leader>lr', '<cmd>FzfLua lsp_references ignore_current_line=true jump_to_single_result=true<cr>',      desc = 'References' },
        { '<leader><leader>lt', '<cmd>FzfLua lsp_typedefs ignore_current_line=true jump_to_single_result=true<cr>',        desc = 'Type Definitions' },
        { '<leader><leader>m',  group = 'Menu' },
        { '<leader><leader>ml', '<cmd>FloatermNew lazygit<cr>',                                                            desc = 'Lazy Git' },
        { '<leader><leader>mr', '<cmd>FloatermNew ranger<cr>',                                                             desc = 'Ranger' },
        { '<leader><leader>mt', '<cmd>FloatermToggle<cr>',                                                                 desc = 'Pop-up Toggle' },
        { '<leader><leader>s',  '<cmd>lua vim.lsp.buf.format()<cr>',                                                       desc = 'Format' },
        { '<leader><leader>n',  group = 'Noice' },
        { '<leader><leader>nd', '<cmd>Noice dismiss<cr>',                                                                  desc = 'Dismiss' },
        { '<leader><leader>ne', '<cmd>Noice errors<cr>',                                                                   desc = 'Errors' },
        { '<leader><leader>nh', '<cmd>Noice history<cr>',                                                                  desc = 'History' },
        { '<leader><leader>nl', '<cmd>Noice last<cr>',                                                                     desc = 'Last Message' },
        { '<leader><leader>x',  group = 'Trouble' },
        { '<leader><leader>xb', '<cmd>Trouble diagnostics toggle filter.buf=0<cr>',                                        desc = 'Buffer Diagnostics' },
        { '<leader><leader>xd', '<cmd>Trouble diagnostics toggle<cr>',                                                     desc = 'Diagnostics' },
        { '<leader><leader>xl', '<cmd>Trouble loclist toggle<cr>',                                                         desc = 'Location List' },
        { '<leader><leader>xq', '<cmd>Trouble qflist toggle<cr>',                                                          desc = 'Quick Fix' },
        { '<leader><leader>xr', '<cmd>Trouble lsp toggle focus=false win.position=right<cr>',                              desc = 'LSP References' }
      }
    )
  end
}
