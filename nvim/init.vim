scriptencoding utf-8
let g:mapleader = "\<Space>"
let g:maplocalleader = ','

call plug#begin('~/.local/share/nvim/plugged')
Plug 'HerringtonDarkholme/yats.vim'
Plug 'LnL7/vim-nix', { 'for': ['nix'] }
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'artnez/vim-wipeout'
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
Plug 'easymotion/vim-easymotion'
Plug 'elzr/vim-json'
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'itchyny/lightline.vim'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/fzf', { 'dir': '~/.local/share/nvim/fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-peekaboo'
Plug 'justinmk/vim-dirvish'
Plug 'liuchengxu/vim-which-key'
Plug 'markonm/traces.vim'
Plug 'matze/vim-move'
Plug 'mhartington/nvim-typescript', { 'for': ['typescript', 'tsx'], 'build': './install.sh' }
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-cssomni'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-syntax' | Plug 'Shougo/neco-syntax'
Plug 'ncm2/ncm2-tern',  {'do': 'npm install'}
Plug 'ncm2/ncm2-vim' | Plug 'Shougo/neco-vim'
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

" vim-bufkill
let g:BufKillCreateMappings = 0

" vim-easymotion
let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1

" fzf
let g:fzf_command_prefix = 'Fzf'

" languageclient-neovim
let g:LanguageClient_serverCommands = {
  \ 'javascript': ['javascript-typescript-stdio'],
  \ 'typescript': ['javascript-typescript-stdio'],
  \ }

" vim-lightline
let g:lightline = { 'colorscheme': 'one' }

" vim-move
let g:move_map_keys = 0

" vim-json
let g:vim_json_syntax_conceal = 0

" ==========================================================
" settings
" ==========================================================

" black background
set background=dark

" fix backspace
set backspace=indent,eol,start

" affects the way cindent works
set cinoptions=j1,J1,(2,W2

" yank and paste using system clipboard
set clipboard+=unnamedplus

" highlight 100th column
set colorcolumn=100

" insert-mode completion options
set completeopt=noinsert,menuone,noselect

" highlight current line
set cursorline

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

" round indent to multiple of 'shiftwidth'
set shiftround

" number of spaces to use for each step of (auto)indent
set shiftwidth=2

" override 'ignorecase' when pattern has upper case character
set smartcase

" autoindent new lines
set smartindent

" open new splits below and to the right
set splitright
set splitbelow

" number of spaces that a <Tab> in the file counts for
set tabstop=2

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
colorscheme onedark
highlight Normal ctermfg=249 ctermbg=236 guifg=#afb7c0 guibg=111213 guisp=NONE cterm=NONE gui=NONE

" ==========================================================
" non-mnemonic key mapping
" ==========================================================

" navigate splits
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" remain in visual mode while indenting
vmap < <gv
vmap > >gv

" search
nmap <M-/> :nohlsearch<cr>

" terminal
tnoremap <Esc> <C-\><C-n>

" vim-easymotion
nmap <leader><leader> <Plug>(easymotion-sn)

" vim-move
nmap <C-o> <Plug>MoveLineDown
nmap <C-p> <Plug>MoveLineUp
vmap <C-o> <Plug>MoveBlockDown
vmap <C-p> <Plug>MoveBlockUp

" which-key
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<cr>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<cr>

" ==========================================================
" mnemonic key mapping
" ==========================================================

" buffers
nmap <leader>bc :close<cr>
nmap <leader>bk :BD<cr>
nmap <leader>bsh :leftabove vnew<cr>
nmap <leader>bsj :rightbelow new<cr>
nmap <leader>bsk :leftabove new<cr>
nmap <leader>bsl :rightbelow vnew<cr>

" git
nmap <leader>gs :Gstatus<cr>

" language
nmap <silent> ld :call LanguageClient#textDocument_definition()<cr>
nmap <silent> lh :call LanguageClient#textDocument_hover()<cr>
nmap <silent> lr :call LanguageClient#textDocument_rename()<cr>

autocmd FileType typescript map <buffer> <leader>lD :TSDoc<cr>
autocmd FileType typescript map <buffer> <leader>lf :TSGetCodeFix<cr>
autocmd FileType typescript map <buffer> <leader>li :TSImport<cr>
autocmd FileType typescript map <buffer> <leader>lp :TSDefPreview<cr>
autocmd FileType typescript map <buffer> <leader>lt :TSTypeDef<cr>

" search
nmap <leader>sC :FzfColors<cr>
nmap <leader>sF :FzfFiles<cr>
nmap <leader>sb :FzfBuffers<cr>
nmap <leader>sc :FzfCommands<cr>
nmap <leader>sf :FzfGFiles<cr>
nmap <leader>sh :FzfHelptags<cr>
nmap <leader>sl :FzfBLines<cr>
nmap <leader>sm :FzfMaps<cr>
nmap <leader>sr :FzfRg<cr>

" windows
nmap <leader>wsh :topleft vnew<cr>
nmap <leader>wsj :botright new<cr>
nmap <leader>wsk :topleft new<cr>
nmap <leader>wsl :botright vnew<cr>

" ==========================================================
" autocommands
" ==========================================================

" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" stripe whitespace on save
autocmd BufWritePre * StripWhitespace

" reload file on focus if it changed on disk
autocmd FocusGained * silent! checktime

