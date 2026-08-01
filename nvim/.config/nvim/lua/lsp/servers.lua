-- LSP Server Configurations (Neovim 0.12+)
-- Consumed by lua/plugins/lsp.lua via vim.lsp.config / vim.lsp.enable()
--
-- EFM language server handles formatting/linting for most filetypes via
-- ~/.config/efm-langserver/config.yaml (biome, prettier, ruff, shfmt, etc.)

local function get_schemastore_schemas()
  local ok, schemastore = pcall(require, 'schemastore')
  if ok then return schemastore.json.schemas() end
  return {}
end

return {
  -- EFM: linting + formatting via external tools
  efm = {
    cmd = { 'efm-langserver' },
    filetypes = {
      'bash', 'css', 'dockerfile', 'go', 'graphql', 'html',
      'javascript', 'javascriptreact', 'json', 'jsonc', 'kotlin',
      'less', 'lua', 'markdown', 'nix', 'python', 'rust',
      'scss', 'sh', 'sql', 'svelte', 'terraform', 'toml',
      'typescript', 'typescriptreact', 'vue', 'yaml', 'zsh',
    },
    root_markers = { '.git' },
  },

  -- TypeScript / JavaScript
  ts_ls = {
    cmd = { 'typescript-language-server', '--stdio' },
    filetypes = { 'javascript', 'javascriptreact', 'typescript', 'typescriptreact' },
    root_markers = { 'package.json', 'tsconfig.json', 'jsconfig.json', '.git' },
  },

  -- Markdown
  marksman = {
    cmd = { 'marksman', 'server' },
    filetypes = { 'markdown' },
    root_markers = { '.git', '.marksman.toml' },
  },

  -- Clojure — custom root_dir: marker-priority detection for mono-repos
  clojure_lsp = {
    cmd = { 'clojure-lsp' },
    filetypes = { 'clojure', 'edn' },
    root_dir = function(fname)
      local path = (type(fname) == 'string' and fname ~= '')
        and fname
        or vim.fn.getcwd()
      local markers = {
        'project.clj', 'deps.edn', 'build.boot',
        'shadow-cljs.edn', 'bb.edn', '.git',
      }
      local found = vim.fs.find(markers, { upward = true, path = path })
      return (found and #found > 0) and vim.fs.dirname(found[1]) or vim.fn.getcwd()
    end,
  },

  -- JSON
  jsonls = {
    cmd = { 'vscode-json-language-server', '--stdio' },
    filetypes = { 'json', 'jsonc' },
    root_markers = { '.git', 'package.json' },
    init_options = { provideFormatter = true },
    settings = {
      json = {
        schemas  = get_schemastore_schemas(),
        validate = { enable = true },
      },
    },
  },

  -- YAML
  yamlls = {
    cmd = { 'yaml-language-server', '--stdio' },
    filetypes = { 'yaml', 'yaml.docker-compose' },
    root_markers = { '.git' },
    settings = {
      yaml = {
        schemas = {
          ['https://json.schemastore.org/github-workflow.json'] =
            '/.github/workflows/*',
          ['https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json'] =
            'docker-compose*.yml',
        },
      },
    },
  },

  -- Go
  gopls = {
    cmd = { 'gopls' },
    filetypes = { 'go', 'gomod', 'gowork', 'gotmpl' },
    root_markers = { 'go.work', 'go.mod', '.git' },
    settings = {
      gopls = {
        analyses     = { unusedparams = true },
        staticcheck  = true,
      },
    },
  },

  -- Lua
  lua_ls = {
    cmd = { 'lua-language-server' },
    filetypes = { 'lua' },
    root_markers = {
      '.luarc.json', '.luarc.jsonc', '.luacheckrc',
      '.stylua.toml', 'stylua.toml', 'selene.toml', 'selene.yml', '.git',
    },
    settings = {
      Lua = {
        runtime     = { version = 'LuaJIT' },
        diagnostics = { globals = { 'vim' } },
        workspace   = {
          library        = vim.api.nvim_get_runtime_file('', true),
          checkThirdParty = false,
        },
        telemetry   = { enable = false },
      },
    },
  },

  -- Python
  pyright = {
    cmd = { 'pyright-langserver', '--stdio' },
    filetypes = { 'python' },
    root_markers = {
      'pyproject.toml', 'setup.py', 'setup.cfg',
      'requirements.txt', 'Pipfile', '.git',
    },
    settings = {
      python = {
        analysis = {
          autoSearchPaths    = true,
          diagnosticMode     = 'workspace',
          useLibraryCodeForTypes = true,
        },
      },
    },
  },

  -- Rust
  rust_analyzer = {
    cmd = { 'rust-analyzer' },
    filetypes = { 'rust' },
    root_markers = { 'Cargo.toml', 'rust-project.json', '.git' },
    settings = {
      ['rust-analyzer'] = {
        cargo      = { allFeatures = true },
        checkOnSave = { command = 'clippy' },
      },
    },
  },

  -- Kotlin
  kotlin_language_server = {
    cmd = { 'kotlin-language-server' },
    filetypes = { 'kotlin' },
    root_markers = {
      'settings.gradle.kts', 'settings.gradle',
      'build.gradle.kts', 'build.gradle',
      'gradlew', 'pom.xml', '.git',
    },
    settings = {
      kotlin = {
        compiler   = { jvm = { target = '21' } },
        indexing   = { enabled = true },
        inlayHints = {
          typeHints      = { enabled = true },
          parameterHints = { enabled = true },
          chainedHints   = { enabled = true },
        },
      },
    },
  },

  -- Nix
  nil_ls = {
    cmd = { 'nil' },
    filetypes = { 'nix' },
    root_markers = { 'flake.nix', 'default.nix', 'shell.nix', '.git' },
    -- formatting handled by EFM (nixpkgs-fmt)
  },
}
