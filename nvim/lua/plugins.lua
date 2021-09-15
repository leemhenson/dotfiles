-- sourced from: https://github.com/ordazgustavo/dotfiles/

local execute = vim.api.nvim_command
local fn = vim.fn

-- local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'
local install_path = '~/.local/share/nvim/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
  execute 'packadd packer.nvim'
end

vim.cmd('packadd packer.nvim')

local packer = require'packer'
local util = require'packer.util'

packer.init({
  package_root = util.join_paths(vim.fn.stdpath('data'), 'site', 'pack')
})

packer.startup(function(use)
  use { 'wbthomason/packer.nvim', opt = true }

  -- LSP
  use 'neovim/nvim-lspconfig'
  use 'nvim-lua/completion-nvim'
  use 'nvim-lua/lsp_extensions.nvim'

  -- Treesitter
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = [[require'nvim-treesitter.configs']]
  }

  -- Languages
  use {'leafgarland/typescript-vim', ft = {'typescript', 'typescriptreact'}}

  -- Telescope
  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
  }

  -- Git
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' }}
  use { 'TimUntersberger/neogit', requires = { 'nvim-lua/plenary.nvim' }}

  -- Tools
  use { 'kyazdani42/nvim-tree.lua', requires = { 'kyazdani42/nvim-web-devicons' }}
  use 'mg979/vim-visual-multi'
  use 'tpope/vim-surround'

  use {
    'terrortylor/nvim-comment',
    config = function()
      require('nvim_comment').setup()
    end
  }

  use {
    'lukas-reineke/format.nvim',
    config = function()
      require('format').setup()
    end
  }

  use {
    'phaazon/hop.nvim',
    as = 'hop',
    config = function()
      require('hop').setup(
        -- { keys = 'etovxqpdygfblzhckisuran' }
      )
    end
  }

  use {
    'cappyzawa/trim.nvim',
    config = function()
      require('trim').setup({
        disable = {"markdown"}
      })
    end
  }

  use {
    'folke/lsp-trouble.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      require('trouble').setup()
    end,
    cmd = 'LspTroubleToggle'
  }

  -- Themes
  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true},
    config = function()
      require('lualine').setup{
        options = {
          theme = 'onedark',
          section_separators = {'', ''},
          component_separators = {'', ''},
          icons_enabled = true,
        },
        sections = {
          lualine_a = { {'mode', upper = true} },
          lualine_b = { {'branch', icon = ''} },
          lualine_c = { {'filename', file_status = true}, {'diagnostics', sources = { 'nvim_lsp' }, color_warn ='#ffcc00' } },
          lualine_x = { 'encoding', 'filetype' },
          lualine_y = { 'progress' },
          lualine_z = { 'location' },
        },
        inactive_sections = {
          lualine_a = {  },
          lualine_b = {  },
          lualine_c = { 'filename' },
          lualine_x = { 'location' },
          lualine_y = {  },
          lualine_z = {   }
        }
      }
    end
  }

  use { 'metalelf0/jellybeans-nvim', requires = 'rktjmp/lush.nvim' }
  use 'mhartington/oceanic-next'
  use 'sainnhe/edge'
end)
