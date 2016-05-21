scriptencoding utf-9

set nocompatible              " be iMproved, required
filetype off                  " required
let g:mapleader = ","

set rtp^=$DOTFILES/vim

call plug#begin('$DOTFILES/vim/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'artnez/vim-wipeout'
Plug 'neomake/neomake'
Plug 'bronson/vim-crosshairs'
Plug 'Chiel92/vim-autoformat'
Plug 'djoshea/vim-autoread'
Plug 'dyng/ctrlsf.vim'
Plug 'easymotion/vim-easymotion'
Plug 'esneider/YUNOcommit.vim'
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'janko-m/vim-test'
Plug 'jgdavey/vim-blockle', { 'for': 'ruby' }
Plug 'jceb/vim-orgmode'
Plug 'jordwalke/flatlandia'
Plug 'junegunn/fzf', { 'dir': '~/.fzf' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-fnr' | Plug 'junegunn/vim-pseudocl'
Plug 'junegunn/vim-oblique' | Plug 'junegunn/vim-pseudocl'
Plug 'junegunn/vim-peekaboo'
Plug 'ludovicchabant/vim-gutentags'
Plug 'matze/vim-move'
Plug 'mhinz/vim-startify'
Plug 'mtth/scratch.vim'
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'ntpeters/vim-better-whitespace'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'peterhorne/tabline.vim'
Plug 'qpkorr/vim-bufkill'
Plug 'Raimondi/delimitMate'
Plug 'rking/ag.vim'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'sjl/gundo.vim'
Plug 'slim-template/vim-slim'
Plug 'syngan/vim-vimlint' | Plug 'ynkdir/vim-vimlparser'
Plug 'terryma/vim-multiple-cursors'
Plug 'thewatts/wattslandia'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-surround'
Plug 'Valloric/YouCompleteMe'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'w0ng/vim-hybrid'

" Add plugins to &runtimepath
call plug#end()

filetype plugin indent on    " required
syntax enable
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set background=dark
colorscheme wattslandia
set guifont=Inconsolata
let g:airline_theme='flatlandia'
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

" Default indent (2 spaces)
set shiftwidth=2
set tabstop=2
set shiftround

set autoindent
set smartindent

set wrap
set breakindent

" Turn backup off
set nobackup
set nowb
set noswapfile

set incsearch  " Makes search act like search in modern browsers
set ignorecase " Ignore case when using a search pattern
set smartcase  " Override 'ignorecase' when pattern
               " has upper case character

" Toggle highlighted search results
noremap <leader>h :set hlsearch! hlsearch?<CR>

" Fix backspace
set backspace=indent,eol,start

" Open new splits below and to the right
set splitright
set splitbelow

" Number of lines to show around cursor
set scrolloff=20

" Hide current mode in command bar
set noshowmode

" navigate splits with ctrl-jklh
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l
noremap <c-h> <c-w>h
noremap <c-w>s :sp<CR>

" Neomake
let g:neomake_error_sign = { 'text': '✗' }
let g:neomake_warning_sign = { 'text': '⚠' }
let g:neomake_ruby_rubocop_maker = { 'args': ['--config=./.rubocop.yml'] }

autocmd! BufWinEnter * Neomake
autocmd! BufWritePost * Neomake

" strip whitespace on save
autocmd BufWritePre * StripWhitespace

" Navigate tabs (alt-t new, alt-q close, alt-j/k navigate)
noremap ˙ gT
noremap ¬ gt
noremap † :tabnew<CR>
noremap œ :tabclose<CR>

" Map to trigger sudo prompt
cmap w!! %!sudo tee > /dev/null %

" Map .md file extension to markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Map .es6 file extension to javascript
autocmd BufNewFile,BufReadPost *.es6 set filetype=javascript

" Ignore common files/dirs
set wildignore+=*/cache/*
set wildignore+=*/vendor/*

" Enable smartcase for easymotion
let g:EasyMotion_smartcase = 1

" Use easymotion prefix search by default: ,,<char>
map <Leader>; <Plug>(easymotion-s)

" Remove airline seperators (arrows)
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_section_z=''
let g:airline_section_z = airline#section#create(['%{ObsessionStatus(''$'', '''')}', 'windowswap', '%3p%% ', 'linenr', ':%3v '])

" Blockle
let g:blockle_mapping="∫"

" fzf
let g:fzf_command_prefix = 'Fzf'

imap <Leader>m <plug>(fzf-maps-i)
nmap <Leader>m <plug>(fzf-maps-n)
omap <Leader>m <plug>(fzf-maps-o)
xmap <Leader>m <plug>(fzf-maps-x)

imap <Leader>cb <plug>(fzf-complete-buffer-line)
imap <Leader>cf <plug>(fzf-complete-file)
imap <Leader>cg <plug>(fzf-complete-file-ag)
imap <Leader>cl <plug>(fzf-complete-line)
imap <Leader>cp <plug>(fzf-complete-path)
imap <Leader>cw <plug>(fzf-complete-word)

fun! s:fzf_root()
  let path = finddir(".git", expand("%:p:h").";")
  return fnamemodify(substitute(path, ".git", "", ""), ":p:h")
endfun

nnoremap <Leader>: :FzfCommands<cr>
nnoremap <Leader>a :FzfAg<tab>
nnoremap <Leader>cb :FzfBCommits<cr>
nnoremap <Leader>cr :FzfCommits<cr>
nnoremap <Leader>fb :FzfBuffers<cr>
nnoremap <Leader>ff :exe 'FzfFiles ' . <SID>fzf_root()<CR>
nnoremap <Leader>fg :FzfGitFiles<cr>
nnoremap <Leader>fh :FzfHelptags<cr>
nnoremap <Leader>fl :FzfLines<cr>
nnoremap <Leader>ft :FzfTags<tab>
nnoremap <Leader>hc :FzfHistory:<cr>
nnoremap <Leader>hh :FzfHistory<cr>
nnoremap <Leader>hs :FzfHistory/<cr>

" ctrlsf
vmap <Leader>s <Plug>CtrlSFVwordPath

" JSX
let g:jsx_ext_required = 0

" Don't automatically insert comment chars
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" vim-move
let g:move_map_keys = 0

vmap <C-o> <Plug>MoveBlockDown
vmap <C-p> <Plug>MoveBlockUp
nmap <C-o> <Plug>MoveLineDown
nmap <C-p> <Plug>MoveLineUp

" vim-test
let test#ruby#bundle_exec = 0

nnoremap <Leader>tF :sp term://ruby\ %\ \&\&\ read<CR>
nnoremap <Leader>tf :vsp term://ruby\ %\ \&\&\ read<CR>
nnoremap <Leader>tL :sp term://mtest\ %:<C-r>=line('.')<CR><CR>
nnoremap <Leader>tl :vsp term://mtest\ %:<C-r>=line('.')<CR><CR>
nnoremap <Leader>ts :TestSuite<CR>

" remain in visual mode while indenting
vnoremap < <gv
vnoremap > >gv

" vim-vinegar
nmap - <Plug>VinegarSplitUp

" ag.vim
let g:ag_working_path_mode="r"

" fugitive
nnoremap <C-g> :Gstatus<cr>

" pgformatter
au FileType sql setl formatprg=/usr/local/bin/pg_format\ -

" Navigate splits when in NeoVim's :terminal
tnoremap <ESC> <c-\><c-n>
tnoremap <c-j> <c-\><c-n><c-w>j
tnoremap <c-k> <c-\><c-n><c-w>k
tnoremap <c-l> <c-\><c-n><c-w>l
tnoremap <c-h> <c-\><c-n><c-w>h

" automatically switch to insert mode when navigating to a terminal window
autocmd BufWinEnter,WinEnter term://* startinsert

" Gundo
nnoremap <Leader>u :GundoToggle<cr>

" Startify
let g:startify_session_dir = "$HOME/icloud/docs/work/vim-sessions"
let g:startify_list_order = ['sessions', 'files', 'dir', 'bookmarks']

let g:startify_custom_header = map(split(system('fortune -s | cowsay'), '\n'), '"   ". v:val') + ['','']

" VimCompleteMe
" prevent conflict with endwise
let g:endwise_no_mappings = 1
imap <expr><cr> pumvisible() ? "\<c-y>" : "\<cr>\<Plug>DiscretionaryEnd"

