-- sourced from: https://github.com/ordazgustavo/dotfiles/

local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  execute 'packadd packer.nvim'
end

-- Compile this file every time it chages
vim.api.nvim_exec([[
  augroup Packer
    autocmd!
    autocmd BufWritePost plugins.lua PackerCompile
  augroup end
]], false)

require('packer').startup(function (use)
  use 'wbthomason/packer.nvim'

  -- LSP
  use {
    'neovim/nvim-lspconfig',
    requires = {'kabouzeid/nvim-lspinstall'},
    config = [[require'config.lsp']]
  }
  use 'nvim-lua/completion-nvim'
  use 'nvim-lua/lsp_extensions.nvim'

  -- TreeSitter
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = [[require'config.treesitter']]
  }

  -- Telescope
  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    setup = [[require'config.telescope_setup']],
    config = [[require'config.telescope']],
    cmd = 'Telescope'
  }

  -- Languages
  use {'leafgarland/typescript-vim', ft = {'typescript', 'typescriptreact'}}

  -- Git
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' }}
  use { 'TimUntersberger/neogit', requires = { 'nvim-lua/plenary.nvim' }}

  -- Editing
  use 'mg979/vim-visual-multi'
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use {
    'lukas-reineke/format.nvim',
    config = [[require'config.formatters']]
  }
  use {
    'phaazon/hop.nvim',
    as = 'hop',
    config = function()
      -- you can configure Hop the way you like here; see :h hop-config
      -- require'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
    end
  }

  -- UI
  use {
    'lewis6991/gitsigns.nvim',
    event = 'BufEnter',
    config = [[require'config.gitsigns']]
  }

  use {
    'kyazdani42/nvim-tree.lua',
    config = [[require'config/nvimtree']]
  }

  use {
    'folke/lsp-trouble.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require('trouble').setup() end,
    cmd = 'LspTroubleToggle'
  }

  use {
    'hoob3rt/lualine.nvim',
    config = [[require'config/lualine']]
  }

  use 'kyazdani42/nvim-web-devicons'

  use 'metalelf0/jellybeans-nvim'
  use 'mhartington/oceanic-next'
  use 'sainnhe/edge'
end)
