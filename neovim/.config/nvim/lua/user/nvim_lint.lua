return {
  'mfussenegger/nvim-lint',
  config = function ()
    require('lint').linters_by_ft = {
      javascript = {'eslint_d'},
      markdown = {'vale'},
      nix = {'nix'},
      python = {'ruff'},
      typescript = {'eslint_d'}
    }
  end
}
