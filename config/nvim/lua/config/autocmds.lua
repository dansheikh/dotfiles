local autocmd = vim.api.nvim_create_autocmd
local execute = vim.api.nvim_command
local group = vim.api.nvim_create_augroup
local utility = require('lib.utility')

local check = group('Check', { clear = true })
local lsp_config_group = group('LspConfig', { clear = true })

autocmd({ 'BufEnter', 'FocusGained' }, {
  group = check,
  callback = function()
    vim.schedule(function()
      execute('checktime')
    end)
  end
})

autocmd({ 'LspAttach' }, {
  group = lsp_config_group,
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)

    -- if client:supports_method('textDocument/completion') then
    --   vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
    -- end

    if client:supports_method('textDocument/formatting') and client.name ~= 'ts_ls' then
      autocmd({ 'BufWritePre' }, {
        group = lsp_config_group,
        buffer = args.buf,
        callback = function()
          vim.lsp.buf.format({ async = false, bufnr = args.buf, id = client.id })
        end
      })
    end

    if client:supports_method('textDocument/inlayHint') then
      vim.lsp.inlay_hint.enable(true, { bufnr = args.buf })
    end

    autocmd({ 'CursorHold' }, {
      group = lsp_config_group,
      buffer = args.buf,
      callback = function(args)
        local opts = {
          focusable = false,
          close_events = { 'BufLeave', 'CursorMoved', 'InsertEnter', 'FocusLost' },
          border = 'rounded',
          source = 'always',
          prefix = ' ',
          scope = 'cursor',
        }
        vim.diagnostic.open_float(nil, opts)
        utility.map('i', '<C-s>', vim.lsp.buf.signature_help, { buffer = args.buf })
        utility.map('n', '\'h', vim.lsp.buf.hover, { buffer = args.buf })
        utility.map('n', '\'r', vim.lsp.buf.rename, { buffer = args.buf })
        utility.map('n', '[d', vim.diagnostic.goto_prev, { buffer = args.buf })
        utility.map('n', ']d', vim.diagnostic.goto_next, { buffer = args.buf })
        -- utility.map('n', 'gd', vim.lsp.buf.definition, { buffer = args.buf })
        -- utility.map('n', 'gt', vim.lsp.buf.type_definition, { buffer = args.buf })
        -- utility.map('n', 'gi', vim.lsp.buf.implementation, { buffer = args.buf })
        utility.map('n', 'gd', '<cmd>FzfLua lsp_definitions ignore_current_line=true jump_to_single_result=true<cr>',
          { buffer = args.buf })
        utility.map('n', 'gi', '<cmd>FzfLua lsp_implementations ignore_current_line=true jump_to_single_result=true<cr>',
          { buffer = args.buf })
        utility.map('n', 'gr', '<cmd>FzfLua lsp_references ignore_current_line=true jump_to_single_result=true<cr>',
          { buffer = args.buf })
        utility.map('n', 'gt', '<cmd>FzfLua lsp_typedefs ignore_current_line=true jump_to_single_result=true<cr>',
          { buffer = args.buf })
      end
    })
  end
})
