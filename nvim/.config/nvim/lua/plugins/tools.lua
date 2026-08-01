return {
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        python = { "ruff_format" },
        javascript = { "oxfmt", "prettierd", "prettier", stop_after_first = true },
        typescript = { "oxfmt", "prettierd", "prettier", stop_after_first = true },
        javascriptreact = { "oxfmt", "prettierd", "prettier", stop_after_first = true },
        typescriptreact = { "oxfmt", "prettierd", "prettier", stop_after_first = true },
        json = { "oxfmt", "prettierd", stop_after_first = true },
        jsonc = { "oxfmt", "prettierd", stop_after_first = true },
        yaml = { "prettierd", "prettier", stop_after_first = true },
        markdown = { "prettierd", "prettier", stop_after_first = true },
        sh = { "shfmt" },
        bash = { "shfmt" },
        zsh = { "shfmt" },
        nix = { "nixfmt" },
        terraform = { "terraform_fmt" },
        toml = { "taplo" },
      },
      format_on_save = {
        timeout_ms = 1000,
        lsp_format = "fallback",
      },
      formatters = {
        oxfmt = {
          command = "bunx",
          args = { "--bun", "--no-install", "oxfmt", "--stdin-filepath", "$FILENAME", "-" },
          stdin = true,
        },
      },
    },
  },
}
