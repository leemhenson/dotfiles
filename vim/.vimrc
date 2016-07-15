scriptencoding utf-9

set nocompatible              " be iMproved, required
filetype off                  " required
let g:mapleader = "\<Space>"

set rtp^=$DOTFILES/vim

call plug#begin('$DOTFILES/vim/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'albfan/ag.vim'
Plug 'artnez/vim-wipeout'
Plug 'neomake/neomake'
Plug 'bronson/vim-crosshairs'
Plug 'Chiel92/vim-autoformat'
Plug 'djoshea/vim-autoread'
Plug 'dyng/ctrlsf.vim'
Plug 'easymotion/vim-easymotion'
Plug 'esneider/YUNOcommit.vim'
Plug 'garbas/vim-snipmate' | Plug 'MarcWeber/vim-addon-mw-utils' | Plug 'tomtom/tlib_vim'
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'jacoborus/tender'
Plug 'janko-m/vim-test'
Plug 'jgdavey/vim-blockle', { 'for': 'ruby' }
Plug 'jreybert/vimagit'
Plug 'junegunn/fzf', { 'dir': '~/.fzf' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-fnr' | Plug 'junegunn/vim-pseudocl'
Plug 'junegunn/vim-oblique' | Plug 'junegunn/vim-pseudocl'
Plug 'junegunn/vim-peekaboo'
Plug 'justinmk/vim-dirvish'
Plug 'matze/vim-move'
Plug 'mhinz/vim-startify'
Plug 'mtth/scratch.vim'
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'ntpeters/vim-better-whitespace'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'qpkorr/vim-bufkill'
Plug 'Raimondi/delimitMate'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'sjl/gundo.vim'
Plug 'slim-template/vim-slim'
Plug 'syngan/vim-vimlint' | Plug 'ynkdir/vim-vimlparser'
Plug 't9md/vim-choosewin'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-surround'
Plug 'Valloric/YouCompleteMe'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'webdevel/tabulous'

" Add plugins to &runtimepath
call plug#end()

filetype plugin indent on    " required
syntax enable
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set background=dark
colorscheme tender
let g:airline_theme='term'
let g:airline#extensions#hunks#enabled=0
set guifont=Inconsolata
set shell=~/.dotfiles/zsh/bin/zsh

" More frequent updates for, e.g. signs.
set updatetime=750

" Always show status bar
set laststatus=2

" Enable mouse support (useful for resizing windows)
set mouse=a

" Highlight current line
:set cursorline
highlight CursorLine cterm=NONE ctermbg=black ctermfg=NONE guibg=black guifg=NON

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

" Neomake
let g:neomake_error_sign = { 'text': '✗' }
let g:neomake_warning_sign = { 'text': '⚠' }
let g:neomake_ruby_rubocop_maker = { 'args': ['--config=./.rubocop.yml'] }

autocmd! BufWinEnter * Neomake
autocmd! BufWritePost * Neomake

" strip whitespace on save
autocmd BufWritePre * StripWhitespace

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

" Remove airline seperators (arrows)
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_section_b=''
let g:airline_section_z = airline#section#create(['%{ObsessionStatus(''$'', '''')}', 'windowswap', '%3p%% ', 'linenr', ':%3v '])

" Blockle
let g:blockle_mapping="∫"

" Bufkill
let g:BufKillCreateMappings=0

" fzf
let g:fzf_command_prefix = 'Fzf'

fun! s:fzf_root()
  let path = finddir(".git", expand("%:p:h").";")
  return fnamemodify(substitute(path, ".git", "", ""), ":p:h")
endfun

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
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

" JSX
let g:jsx_ext_required = 0

" Don't automatically insert comment chars
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" vim-move
let g:move_map_keys = 0

" vim-test
let test#ruby#bundle_exec = 0

" ag.vim
let g:ag_working_path_mode="r"

" pgformatter
au FileType sql setl formatprg=/usr/local/bin/pg_format\ -

" automatically switch to insert mode when navigating to a terminal window
autocmd BufWinEnter,WinEnter term://* startinsert

" Startify
let g:startify_session_dir = "$HOME/icloud/docs/work/vim-sessions"
let g:startify_list_order = ['sessions', 'files', 'dir', 'bookmarks']

" YouCompleteMe
" prevent conflict with endwise
let g:endwise_no_mappings = 1
imap <expr><cr> pumvisible() ? "\<c-y>" : "\<cr>\<Plug>DiscretionaryEnd"
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]

" ChooseWin
let g:choosewin_overlay_enable = 1

" Easymotion
let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1

" Fugitive
let g:fugitive_no_maps=1

" ================
" Mappings
" ================

" 1 Buffers
nmap <Leader>sb :FzfBuffers<cr>

" 2 Editing
nmap <Leader>f :Autoformat<cr>
nmap <Leader>u :GundoToggle<cr>
nmap <C-o> <Plug>MoveLineDown
nmap <C-p> <Plug>MoveLineUp
vmap <C-o> <Plug>MoveBlockDown
vmap <C-p> <Plug>MoveBlockUp
imap <Tab> <Plug>snipMateNextOrTrigger
smap <Tab> <Plug>snipMateNextOrTrigger

  " 2.1 Remain in visual mode while indenting
  vmap < <gv
  vmap > >gv

" 3 Git
nmap <Leader>gs :Magit<cr>

" 4 Help
nmap <Leader>h: :FzfCommands<cr>
nmap <Leader>hm <plug>(fzf-maps-n)
nmap <Leader>ht :FzfHelptags<cr>

" 5 Motions
nmap <Leader><Leader> <Plug>(easymotion-sn)

" 6 Search
nmap <Leader>sg :FzfGitFiles<cr>
nmap <Leader>sf :exe 'FzfFiles ' . <SID>fzf_root()<CR>
nmap <Leader>st :FzfAg<cr>
vmap <Leader>S <Plug>CtrlSFVwordPath

  " 6.1 Toggle highlighted search results
  map <leader>sh :set hlsearch! hlsearch?<CR>


" 7 Testing
nmap <Leader>tf :vsp term://ruby\ %\ \&\&\ read<CR>
nmap <Leader>tF :sp term://ruby\ %\ \&\&\ read<CR>
nmap <Leader>tl :vsp term://mtest\ %:<C-r>=line('.')<CR><CR>
nmap <Leader>tL :sp term://mtest\ %:<C-r>=line('.')<CR><CR>
nmap <Leader>ts :TestSuite<CR>

" 8 Tabs (alt-t new, alt-q close, alt-j/k navigate)
map ˙ gT
map ¬ gt
map † :tabnew<CR>
map œ :tabclose<CR>

" 9 Windows
nmap  <Leader>w <Plug>(choosewin)

  " 9.1 Navigate splits with ctrl-jklh
  map <c-j> <c-w>j
  map <c-k> <c-w>k
  map <c-l> <c-w>l
  map <c-h> <c-w>h
  map <c-w>s :sp<CR>

  " 9.2 Navigate windows from neovim terminal
  tmap <ESC> <c-\><c-n>
  tmap <c-j> <c-\><c-n><c-w>j
  tmap <c-k> <c-\><c-n><c-w>k
  tmap <c-l> <c-\><c-n><c-w>l
  tmap <c-h> <c-\><c-n><c-w>h

