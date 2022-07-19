local get_config = require('lib.utility').get_config
local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'http://github.com/wbthomason/packer.nvim', install_path})
  print('Installing plugin manager: Packer...')
  execute('packadd packer.nvim')
end

local packer = require('packer')

packer.init({
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'rounded' })
    end
  },
  max_jobs = 10,
  profile = {
    enable = true,
    threshold = 0
  }
})

packer.startup(function(use)
  use('wbthomason/packer.nvim')

  use('editorconfig/editorconfig-vim')

  use('tpope/vim-fugitive')

  use('tpope/vim-surround')

  use('kyazdani42/nvim-web-devicons')

  use({
        'akinsho/bufferline.nvim',
        config = get_config('bufferline')
  })

  use({
        'nvim-lualine/lualine.nvim',
        config = get_config('lualine')
  })

  use({
        'kyazdani42/nvim-tree.lua',
        config = get_config('tree')
  })

  use({
        'neovim/nvim-lspconfig',
        config = get_config('lspconfig')
  })

  use({
        'L3MON4D3/LuaSnip'
  })

  use({
        'hrsh7th/nvim-cmp',
        requires = {{'hrsh7th/cmp-buffer'},
                    {'hrsh7th/cmp-nvim-lsp'},
                    {'hrsh7th/cmp-nvim-lua'},
                    {'hrsh7th/cmp-path'},
                    {'onsails/lspkind.nvim'},
                    {'saadparwaiz1/cmp_luasnip'},
                    {'ray-x/cmp-treesitter'}},
        config = get_config('cmp')
  })

  use({
        'nvim-treesitter/nvim-treesitter',
        requires = {{'p00f/nvim-ts-rainbow'},
                    {'RRethy/nvim-treesitter-endwise'}},
        run = ':TSUpdate',
        config = get_config('treesitter')
  })

  use({
        'dense-analysis/ale',
        cmd = 'ALEEnable',
        config = 'vim.cmd([[ALEEnable]])'
  })

  use({
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/plenary.nvim'},
                    {'nvim-lua/popup.nvim'},
                    {'nvim-telescope/telescope-file-browser.nvim'},
                    {'nvim-telescope/telescope-fzy-native.nvim'}},
        config = get_config('telescope')
  })

  use({
        'phaazon/hop.nvim',
        config = get_config('hop')
  })

  use({
        'folke/which-key.nvim',
        config = get_config('which')
  })

  use({
        'goolord/alpha-nvim',
        config = get_config('alpha')
  })

  use({
        'EdenEast/nightfox.nvim',
        config = get_config('nightfox')
  })

  if packer_bootstrap then
    packer.sync()
  end
end)
