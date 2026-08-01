-- Autocommands
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- Check if file changed on disk
autocmd({ 'BufEnter', 'FocusGained' }, {
  group = augroup('CheckFile', { clear = true }),
  callback = function()
    vim.schedule(function() vim.cmd('checktime') end)
  end,
  desc = 'Check if buffer changed on disk',
})

-- Highlight yanked text
autocmd('TextYankPost', {
  group = augroup('HighlightYank', { clear = true }),
  callback = function()
    vim.highlight.on_yank({ higroup = 'Visual', timeout = 200 })
  end,
  desc = 'Highlight yanked text',
})

-- Return to last edit position
autocmd('BufReadPost', {
  group = augroup('LastPosition', { clear = true }),
  callback = function()
    local mark   = vim.api.nvim_buf_get_mark(0, '"')
    local lcount = vim.api.nvim_buf_line_count(0)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
  desc = 'Return to last edit position',
})

-- Close certain filetypes with q
autocmd('FileType', {
  group = augroup('CloseWithQ', { clear = true }),
  pattern = { 'help', 'man', 'qf', 'query', 'lspinfo', 'checkhealth', 'startuptime' },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set('n', 'q', '<cmd>close<cr>', { buffer = event.buf, desc = 'Close window' })
  end,
  desc = 'Close certain filetypes with q',
})

-- Auto-create parent directories when saving
autocmd('BufWritePre', {
  group = augroup('AutoCreateDir', { clear = true }),
  callback = function(event)
    if event.match:match('^%w%w+://') then return end
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ':p:h'), 'p')
  end,
  desc = 'Auto-create parent directories',
})


-- =============================================================================
-- Treesitter
-- =============================================================================

-- Enable native treesitter highlighting and indentation for every filetype.
-- Neovim 0.12: highlighting is on by default for bundled parsers; this autocmd
-- extends that to all installed parsers uniformly.
autocmd('FileType', {
  group = augroup('TreesitterStart', { clear = true }),
  callback = function()
    local ok = pcall(vim.treesitter.start)
    if ok then
      -- treesitter-based indentation (replaces indent = { enable = true })
      vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
  end,
  desc = 'Enable treesitter highlighting and indentation',
})

-- =============================================================================
-- LSP
-- =============================================================================

local lsp_group = augroup('LspConfig', { clear = true })

-- Safe format: only runs if a formatting-capable client is attached
local function safe_format(bufnr, show_message)
  bufnr = bufnr or 0
  show_message = (show_message == nil) and true or show_message

  if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then return end

  local clients    = vim.lsp.get_clients({ bufnr = bufnr })
  local formatters = vim.iter(clients)
    :filter(function(c) return c:supports_method('textDocument/formatting') end)
    :totable()

  if #formatters > 0 then
    vim.lsp.buf.format({ async = false, bufnr = bufnr, timeout_ms = 2000 })
  elseif show_message then
    local ft = vim.bo[bufnr].filetype
    vim.notify(
      string.format('No formatter available for %s', ft ~= '' and ft or 'this filetype'),
      vim.log.levels.WARN
    )
  end
end

vim.api.nvim_create_user_command('FormatToggle', function()
  vim.g.disable_autoformat = not vim.g.disable_autoformat
  vim.notify(
    'Format on save ' .. (vim.g.disable_autoformat and 'disabled' or 'enabled'),
    vim.log.levels.INFO
  )
end, { desc = 'Toggle format on save' })

autocmd('LspAttach', {
  group = lsp_group,
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if not client then return end

    local bufnr = args.buf

    -- Inlay hints (0.10+)
    if client:supports_method('textDocument/inlayHint') then
      vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
    end

    -- Format on save for designated formatter clients only
    local formatters = { efm = true, rust_analyzer = true, clojure_lsp = true }
    if formatters[client.name] and client:supports_method('textDocument/formatting') then
      autocmd('BufWritePre', {
        group  = lsp_group,
        buffer = bufnr,
        callback = function() safe_format(bufnr, false) end,
        desc   = string.format('Format on save (%s)', client.name),
      })
    end

    -- Keymaps: set once per buffer via buffer var (survives config reloads)
    if vim.b[bufnr].lsp_keymaps_set then return end
    vim.b[bufnr].lsp_keymaps_set = true

    local map  = vim.keymap.set
    local opts = { buffer = bufnr }
    local ext  = vim.tbl_extend

    -- LSP actions (quote-prefix namespace, distinct from 0.12 built-in gr*/gO)
    map('n', "'h", vim.lsp.buf.hover,       ext('force', opts, { desc = 'Hover' }))
    map('n', "'r", vim.lsp.buf.rename,      ext('force', opts, { desc = 'Rename' }))
    map('n', "'a", vim.lsp.buf.code_action, ext('force', opts, { desc = 'Code action' }))
    map('v', "'a", vim.lsp.buf.code_action, ext('force', opts, { desc = 'Code action' }))
    map('n', "'f", function() safe_format(bufnr, true) end, ext('force', opts, { desc = 'Format' }))
    map('i', '<C-s>', vim.lsp.buf.signature_help, ext('force', opts, { desc = 'Signature help' }))

    map('n', "'d", function()
      vim.diagnostic.open_float(nil, {
        focusable    = true,
        close_events = { 'BufLeave', 'CursorMoved', 'InsertEnter', 'FocusLost' },
        border       = 'rounded',
        source       = true,   -- 0.12: boolean (was string 'always')
        prefix       = ' ',
        scope        = 'cursor',
      })
    end, ext('force', opts, { desc = 'Show diagnostic' }))

    -- LSP navigation via FzfLua.
    map('n', 'gd', '<cmd>FzfLua lsp_definitions    jump1=true<cr>', ext('force', opts, { desc = 'Go to definition' }))
    map('n', 'gi', '<cmd>FzfLua lsp_implementations jump1=true<cr>', ext('force', opts, { desc = 'Go to implementation' }))
    map('n', 'gr', '<cmd>FzfLua lsp_references      jump1=true<cr>', ext('force', opts, { desc = 'Find references' }))
    map('n', 'gt', '<cmd>FzfLua lsp_typedefs        jump1=true<cr>', ext('force', opts, { desc = 'Go to type definition' }))
  end,
  desc = 'Configure LSP on attach',
})

-- Auto-show diagnostic float on cursor hold
autocmd('CursorHold', {
  group = lsp_group,
  callback = function()
    vim.diagnostic.open_float(nil, {
      focusable    = false,
      close_events = { 'BufLeave', 'CursorMoved', 'InsertEnter', 'FocusLost' },
      border       = 'rounded',
      source       = true,   -- 0.12: boolean
      scope        = 'cursor',
    })
  end,
  desc = 'Show diagnostic float on cursor hold',
})
