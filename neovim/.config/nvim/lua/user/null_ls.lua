return {
  'jose-elias-alvarez/null-ls.nvim',
  config = function()
    local clear_autocmds = vim.api.nvim_clear_autocmds
    local create_autocmd = vim.api.nvim_create_autocmd
    local create_augroup = vim.api.nvim_create_augroup
    local null_ls = require('null-ls')
    local diagnostics = null_ls.builtins.diagnostics
    local formatting = null_ls.builtins.formatting

    local augroup = create_augroup("LspFormat", { clear = true })

    null_ls.setup({
      debug = false,
      on_attach = function(client, bufnr)
        if client.supports_method("textDocument/formatting") then
          clear_autocmds({ group = augroup, buffer = bufnr })
          create_autocmd("BufWritePre", {
            group = augroup,
            buffer = bufnr,
            callback = function()
              vim.lsp.buf.format({
                bufnr = bufnr,
                filter = function(client)
                  return client.name == "null-ls"
                end
              })
            end
          })
        end
      end,
      sources = {
        diagnostics.ruff,
        diagnostics.mypy,
        formatting.black,
        formatting.eslint_d,
        formatting.nixfmt,
        formatting.prettier,
        formatting.stylua,
        formatting.terraform_fmt,
        formatting.yamlfmt
      }
    })
  end
}
