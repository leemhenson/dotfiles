local cmd = vim.cmd                     -- Execute Vim commands
local exec = vim.api.nvim_exec          -- Execute Vimscript
local fn = vim.fn                       -- Call Vim functions
local g = vim.g                         -- Global variables
local opt = vim.opt                     -- Global/buffer/windows-scoped options

g.mapleader = ' '                       -- Change leader

opt.backup = false                      -- Disable overwrite-backup files
opt.clipboard = 'unnamedplus'           -- Allow copy/paste from anywhere
opt.cmdheight = 2                       -- Screen lines to use for the command-line
opt.colorcolumn = '100'                 -- Highlight 100th column
opt.completeopt = 'menuone,noselect,noinsert' -- Completion options
opt.cursorline = true                   -- Highlight current line
opt.errorbells = false                  -- Don't beep on errors
opt.expandtab = false                   -- Dont' use spaces instead of tabs
opt.hidden = true                       -- Enable hidden buffers (navigate away from buffer with unsaved changes)
opt.hlsearch = false                    -- Don't highlight all search matches
opt.incsearch = true                    -- Makes search act like search in modern browsers
opt.mouse = 'a'                         -- Enable mouse in all modes
opt.number = true                       -- Show line numbers
opt.scrolloff = 30                      -- Number of lines to show around cursor
opt.shada = ''                          -- Disable shada
opt.shadafile = 'NONE'                  -- Disable shada
opt.shell = '~/.nix-profile/bin/zsh'    -- Use my zsh as default in terminal
opt.shiftwidth = 2                      -- Number of spaces to use for each step of (auto)indent
opt.shortmess = 'c'                     -- Don't show completion messages
opt.showmode = false                    -- Diasble current-mode message
opt.signcolumn = 'yes'                  -- Always render a sign column
opt.smartindent = true                  -- Smart autoindenting when starting a new line
opt.splitbelow = true                   -- Open new splits below
opt.splitright = true                   -- Open new splits to the right
opt.swapfile = false                    -- Disable swapfile for buffers
opt.syntax = 'on'                       -- Enable syntax highlighting
opt.tabstop = 2                         -- Number of spaces that a <Tab> in the file counts for
opt.termguicolors = true                -- Enable 24-bit colors in terminal vim
opt.wrap = false                        -- Don't wrap lines
opt.writebackup = false                 -- Disable overwrite-backup files

-- Reload file when focusing, if it has changed on disk
cmd[[autocmd FocusGained * silent! checktime]]

-- Theme
cmd [[colorscheme jellybeans-nvim]]
