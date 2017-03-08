scriptencoding utf-8

set nocompatible              " be iMproved, required
filetype off                  " required
let g:mapleader = "\<Space>"

set rtp^=$DOTFILES/vim

call plug#begin('$DOTFILES/vim/plugged')

Plug 'albfan/ag.vim'
let g:ag_working_path_mode="r"

Plug 'dyng/ctrlsf.vim'
map <Leader>r <Plug>CtrlSFPrompt
map <Leader>R <Plug>CtrlSFVwordExec

Plug 'Raimondi/delimitMate'

Plug 'junegunn/fzf', { 'dir': '~/.fzf' }
Plug 'junegunn/fzf.vim'
let g:fzf_command_prefix = 'Fzf'

fun! s:fzf_root()
  let path = finddir(".git", expand("%:p:h").";")
  return fnamemodify(substitute(path, ".git", "", ""), ":p:h")
endfun

let g:fzf_colors = {
      \ 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'Comment'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }

nmap <Leader>cD :FzfHelptags<cr>
nmap <Leader>cF :exe 'FzfFiles ' . <SID>fzf_root()<CR>
nmap <Leader>ca :FzfAg<cr>
nmap <Leader>cb :FzfBuffers<cr>
nmap <Leader>cd <plug>(fzf-maps-n)
nmap <Leader>cf :FzfGitFiles<cr>
nmap <Leader>cx :FzfCommands<cr>
nmap <Leader>s :FzfBLines<cr>

Plug 'sjl/gundo.vim'
nmap <Leader>u :GundoToggle<cr>

Plug 'neomake/neomake'
let g:neomake_error_sign = { 'text': '✗' }
let g:neomake_javascript_enabled_makers = ["eslint"]
let g:neomake_javascript_eslint_exe = './node_modules/.bin/eslint'
let g:neomake_jsx_enabled_makers = ["eslint"]
let g:neomake_ruby_enabled_makers = []
let g:neomake_ruby_rubocop_maker = { 'args': ['--config=./.rubocop.yml'] }
let g:neomake_warning_sign = { 'text': '⚠' }
autocmd! BufWinEnter * Neomake
autocmd! BufWritePost * Neomake

Plug 'arcticicestudio/nord-vim'
augroup nord-overrides
  autocmd!
  autocmd ColorScheme nord highlight xmlTagName guifg=5 ctermfg=5
  autocmd ColorScheme nord highlight xmlTag guifg=5 ctermfg=5
augroup END

Plug 'raichoo/purescript-vim'

Plug 'vim-scripts/SyntaxAttr.vim'
map <Leader>x :call SyntaxAttr()<cr>

Plug 'webdevel/tabulous'

Plug 'vim-airline/vim-airline'
let g:airline#extensions#hunks#enabled=0

Plug 'Chiel92/vim-autoformat'
nmap <Leader>f :Autoformat<cr>

Plug 'djoshea/vim-autoread'

Plug 'ntpeters/vim-better-whitespace'
autocmd BufWritePre * StripWhitespace

Plug 'jgdavey/vim-blockle', { 'for': 'ruby' }
let g:blockle_mapping="∫"

Plug 'qpkorr/vim-bufkill'
let g:BufKillCreateMappings=0
nmap <Leader>bK :Wipeout<CR>
nmap <Leader>bk :BD<CR>

Plug 'tpope/vim-commentary'
Plug 'bronson/vim-crosshairs'
Plug 'justinmk/vim-dirvish'
Plug 'tpope/vim-dispatch'

Plug 'easymotion/vim-easymotion'
let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1
nmap <Leader><Leader> <Plug>(easymotion-sn)

Plug 'tpope/vim-endwise'
let g:endwise_no_mappings = 1  " prevent conflict with YouCompleteMe

Plug 'tpope/vim-eunuch'

Plug 'flowtype/vim-flow', { 'for': 'javascript' }
let g:flow#autoclose = 1

Plug 'tpope/vim-fugitive'
let g:fugitive_no_maps=1
nmap <Leader>gs :Gstatus<cr>

Plug 'airblade/vim-gitgutter'

Plug 'matze/vim-move'
let g:move_map_keys = 0
nmap <C-o> <Plug>MoveLineDown
nmap <C-p> <Plug>MoveLineUp
vmap <C-o> <Plug>MoveBlockDown
vmap <C-p> <Plug>MoveBlockUp

Plug 'tpope/vim-obsession'
Plug 'junegunn/vim-oblique'
Plug 'junegunn/vim-peekaboo'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'junegunn/vim-pseudocl'
Plug 'airblade/vim-rooter'
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'slim-template/vim-slim'

Plug 'garbas/vim-snipmate' | Plug 'MarcWeber/vim-addon-mw-utils' | Plug 'tomtom/tlib_vim', { 'tag': '1.22' }
imap <Tab> <Plug>snipMateNextOrTrigger
smap <Tab> <Plug>snipMateNextOrTrigger

Plug 'tpope/vim-surround'
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }

Plug 'janko-m/vim-test'
let test#ruby#bundle_exec = 0

Plug 'syngan/vim-vimlint'
Plug 'ynkdir/vim-vimlparser'
Plug 'artnez/vim-wipeout'

Plug 'Valloric/YouCompleteMe'
let g:ycm_key_list_previous_completion=[]
let g:ycm_key_list_select_completion=[]
imap <expr><cr> pumvisible() ? "\<c-y>" : "\<cr>\<Plug>DiscretionaryEnd"

Plug 'esneider/YUNOcommit.vim'

" Order-dependent

Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
let g:javascript_plugin_flow = 1

Plug 'mxw/vim-jsx', { 'for': 'javascript' }
let g:jsx_ext_required = 0

" Add plugins to &runtimepath
call plug#end()

filetype plugin indent on    " required
syntax enable
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set background=dark
colorscheme nord
set guifont=Inconsolata
set shell=~/.dotfiles/zsh/bin/zsh

" Cursor shapes
let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
let &t_SI .= "\<Esc>[5 q" " insert mode - line
let &t_SR .= "\<Esc>[4 q" "replace mode - underline
let &t_EI .= "\<Esc>[3 q" "common - block

" More frequent updates for, e.g. signs.
set updatetime=750

" Always show status bar
set laststatus=2

" Enable mouse support (useful for resizing windows)
set mouse=a

" Highlight current line
:set cursorline

" Line numbers
set nu

" Highlight 120th column
set colorcolumn=120

" Enable hidden buffers (navigate away from buffer with unsaved changes)
set hidden

" Yank and paste using system clipboard
set clipboard+=unnamedplus

" Use spaces instead of tabs
set expandtab

" Indenting
set shiftround
set shiftwidth=2
set smartindent
set tabstop=2
set cinoptions=j1,J1,(2,W2

" Line wrapping
set wrap

" Turn backup off
set nobackup
set nowb
set noswapfile

set incsearch  " Makes search act like search in modern browsers
set ignorecase " Ignore case when using a search pattern
set smartcase  " Override 'ignorecase' when pattern
               " has upper case character

" Fix backspace
set backspace=indent,eol,start

" Open new splits below and to the right
set splitright
set splitbelow

" Number of lines to show around cursor
set scrolloff=30

" Hide current mode in command bar
set noshowmode

" Map to trigger sudo prompt
cmap w!! %!sudo tee > /dev/null %

" Map .md file extension to markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Map .es6 file extension to javascript
autocmd BufNewFile,BufReadPost *.es6 set filetype=javascript

" Ignore common files/dirs
set wildignore+=*/cache/*
set wildignore+=*/log/*
set wildignore+=*/node_modules/*
set wildignore+=*/tmp/*
set wildignore+=*/vendor/*

" Don't automatically insert comment chars
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Pgformatter
au FileType sql setl formatprg=/usr/local/bin/pg_format\ -

" Automatically switch to insert mode when navigating to a terminal window
autocmd BufWinEnter,WinEnter term://* startinsert

" Remain in visual mode while indenting
vmap < <gv
vmap > >gv

" Toggle highlighted search results
map <Leader>H :set hlsearch! hlsearch?<CR>

" Split buffers
nmap <Leader>bs<Left> :leftabove vnew<CR>
nmap <Leader>bs<Right> :rightbelow vnew<CR>
nmap <Leader>bs<Up> :leftabove new<CR>
nmap <Leader>bs<Down> :rightbelow new<CR>

" Split window
nmap <Leader>ws<Left> :topleft vnew<CR>
nmap <Leader>ws<Right> :botright vnew<CR>
nmap <Leader>ws<Up> :topleft new<CR>
nmap <Leader>ws<Down> :botright new<CR>

" Navigate tabs (alt-t new, alt-q close, alt-j/k navigate)
map ˙ gT
map ¬ gt
map † :tabnew<CR>
map œ :tabclose<CR>

" Navigate splits
map <C-H> <C-W>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
tmap <ESC> <c-\><c-n>
tmap <c-j> <c-\><c-n><c-w>j
tmap <c-k> <c-\><c-n><c-w>k
tmap <c-l> <c-\><c-n><c-w>l
tmap <c-h> <c-\><c-n><c-w>h

