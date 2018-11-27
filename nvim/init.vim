scriptencoding utf-8
let g:mapleader = "\<Space>"

call plug#begin('~/.local/share/nvim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
Plug 'haya14busa/incsearch.vim'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'dir': '~/.local/share/nvim/fzf', 'do': './install --bin' }
Plug 'junegunn/vim-peekaboo'
Plug 'justinmk/vim-dirvish'
Plug 'LnL7/vim-nix', { 'for': ['nix'] }
Plug 'matze/vim-move'
Plug 'mhartington/nvim-typescript', { 'for': ['typescript', 'tsx'], 'build': './install.sh' }
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-cssomni'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-syntax' | Plug 'Shougo/neco-syntax'
Plug 'ncm2/ncm2-tern',  {'do': 'npm install'}
Plug 'ncm2/ncm2-vim' | Plug 'Shougo/neco-vim'
Plug 'ncm2/ncm2'
Plug 'nightsense/snow'
Plug 'ntpeters/vim-better-whitespace'
Plug 'purescript-contrib/purescript-vim'
Plug 'qpkorr/vim-bufkill'
Plug 'roxma/nvim-yarp'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'w0rp/ale'
call plug#end()

" neovim node provider
let g:node_host_prog = '$HOME/.npm-packages/bin/neovim-node-host'

" vim-bufkill
let g:BufKillCreateMappings = 0

" fzf
let g:fzf_command_prefix = 'Fzf'

" languageclient-neovim
let g:LanguageClient_serverCommands = {
  \ 'javascript': ['javascript-typescript-stdio'],
  \ 'typescript': ['javascript-typescript-stdio'],
  \ }

" vim-lightline
let g:lightline = { 'colorscheme': 'snow_dark' }

" vim-move
let g:move_map_keys = 0

" ==========================================================
" settings
" ==========================================================

" black background
set background=dark

" fix backspace
set backspace=indent,eol,start

" yank and paste using system clipboard
set clipboard+=unnamedplus

" highlight 100th column
set colorcolumn=100

" insert-mode completion options
set completeopt=noinsert,menuone,noselect

" use spaces instead of tabs
set expandtab

" enable hidden buffers (navigate away from buffer with unsaved changes)
set hidden

" ignore case when using a search pattern
set ignorecase

" makes search act like search in modern browsers
set incsearch

" disable backup
set nobackup

" diasble current-mode message
set noshowmode

" disable swapfile for buffers
set noswapfile

" disable backup-before-overwriting-file
set nowb

" line numbers
set nu

" number of lines to show around cursor
set scrolloff=30

" disable shada
set shada="NONE"

" use zsh as default in terminal
set shell=~/.nix-profile/bin/zsh

" override 'ignorecase' when pattern has upper case character
set smartcase

" open new splits below and to the right
set splitright
set splitbelow

" enable 24-bit colors in terminal vim
set termguicolors

" ignore common files/dirs
set wildignore+=*/build/*
set wildignore+=*/cache/*
set wildignore+=*/log/*
set wildignore+=*/node_modules/*
set wildignore+=*/tmp/*
set wildignore+=*/vendor/*

" line wrapping
set wrap

" theme
colorscheme snow
highlight Normal ctermfg=249 ctermbg=236 guifg=#afb7c0 guibg=111213 guisp=NONE cterm=NONE gui=NONE

" ==========================================================
" key mapping
" ==========================================================

" Navigate splits
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" close current buffer without closing split
nmap <Leader>bk :BD<CR>

" split buffers
nmap <Leader>bsh :leftabove vnew<CR>
nmap <Leader>bsj :rightbelow new<CR>
nmap <Leader>bsk :leftabove new<CR>
nmap <Leader>bsl :rightbelow vnew<CR>

" split windows
nmap <Leader>wsh :topleft vnew<CR>
nmap <Leader>wsj :botright new<CR>
nmap <Leader>wsk :topleft new<CR>
nmap <Leader>wsl :botright vnew<CR>

" remain in visual mode while indenting
vmap < <gv
vmap > >gv

" fzf
nmap <Leader>fb :FzfBuffers<cr>
nmap <Leader>fc :FzfCommands<cr>
nmap <Leader>ff :FzfGFiles<cr>
nmap <Leader>fF :FzfFile<cr>
nmap <Leader>fh :FzfHelptags<cr>
nmap <Leader>fm :FzfMaps<cr>
nmap <Leader>fl :FzfBLines<cr>

" language-client
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

" vim-move
nmap <C-o> <Plug>MoveLineDown
nmap <C-p> <Plug>MoveLineUp
vmap <C-o> <Plug>MoveBlockDown
vmap <C-p> <Plug>MoveBlockUp

" ==========================================================
" autocommads
" ==========================================================

" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" automatically switch to insert mode when navigating to a terminal window
autocmd BufWinEnter,WinEnter term://* startinsert

" stripe whitespace on save
autocmd BufWritePre * StripWhitespace

" reload file on focus if it changed on disk
autocmd FocusGained * silent! checktime
