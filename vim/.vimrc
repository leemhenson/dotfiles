scriptencoding utf-8

set nocompatible              " be iMproved, required
filetype off                  " required
let g:mapleader = ","

set rtp^=$DOTFILES/vim

call plug#begin('$DOTFILES/vim/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'benekastah/neomake'
Plug 'bling/vim-airline'
Plug 'Chiel92/vim-autoformat'
Plug 'craigemery/vim-autotag'
Plug 'djoshea/vim-autoread'
Plug 'easymotion/vim-easymotion'
Plug 'gregsexton/gitv'
Plug 'hashivim/vim-terraform'
Plug 'janko-m/vim-test'
Plug 'jgdavey/vim-blockle'
Plug 'junegunn/fzf', { 'dir': '~/.fzf' }
Plug 'junegunn/fzf.vim'
"Plug 'junkblocker/unite-codesearch'
Plug 'luochen1990/rainbow'
Plug 'matze/vim-move'
Plug 'mhinz/vim-startify'
Plug 'mxw/vim-jsx'
Plug 'ntpeters/vim-better-whitespace'
Plug 'pangloss/vim-javascript'
Plug 'peterhorne/tabline.vim'
Plug 'Raimondi/delimitMate'
Plug 'rking/ag.vim'
Plug 'Shougo/deoplete.nvim'
"Plug 'Shougo/neomru.vim'
"Plug 'Shougo/neoyank.vim'
"Plug 'Shougo/unite.vim'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'sjl/gundo.vim'
Plug 'syngan/vim-vimlint' | Plug 'ynkdir/vim-vimlparser'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-vinegar'
"Plug 'tsukkee/unite-tag'
Plug 'tpope/vim-surround'
"Plug 'ujihisa/unite-colorscheme'
Plug 'w0ng/vim-hybrid'

" Add plugins to &runtimepath
call plug#end()

filetype plugin indent on    " required
set background=dark
colorscheme hybrid
syntax enable
set guifont=Inconsolata

" More frequent updates for, e.g. signs.
set updatetime=750

" Always show status bar
set laststatus=2

" Enable mouse support (useful for resizing windows)
set mouse=a

" Highlight current line
set cul

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
set scrolloff=10

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
let g:blockle_mapping = '<Leader>d'

" fzf
map <Leader>m <plug>(fzf-maps-n)
" map <Leader>mi <plug>(fzf-maps-i)
" map <Leader>mv <plug>(fzf-maps-x)

" " Unite
" call unite#filters#matcher_default#use(['matcher_context'])
" call unite#filters#sorter_default#use(['sorter_rank'])

" let g:unite_source_menu_menus = {}
" let g:unite_source_menu_menus.git = {}

" let g:unite_source_menu_menus.git.command_candidates = [
"     \['▷ git status       (Fugitive)', 'Gstatus'],
"     \['▷ git diff         (Fugitive)', 'Gdiff'],
"     \['▷ git log          (Fugitive)', 'exe "silent Glog | Unite quickfix"'],
"     \['▷ git blame        (Fugitive)', 'Gblame'],
"     \['▷ git push         (Fugitive)', 'Git! push'],
"     \['▷ git pull         (Fugitive)', 'Git! pull'],
"     \]

" nnoremap <Leader>a :Unite -force-redraw -direction=dynamicbottom -start-insert -buffer-name=tags tag<cr>
" nnoremap <Leader>b :Unite -direction=dynamicbottom -start-insert -buffer-name=buffers buffer<cr>
" nnoremap <Leader>c :Unite -direction=dynamicbottom -start-insert -buffer-name=commands command<cr>
" nnoremap <Leader>C :Unite -direction=dynamicbottom -start-insert -buffer-name=commands colorscheme<cr>
" nnoremap <Leader>f :Unite -force-redraw -direction=dynamicbottom -start-insert -buffer-name=files file_rec/async:!<cr>
" nnoremap <Leader>g :Unite -direction=dynamicbottom -start-insert -buffer-name=gitfiles file_rec/git<cr>
" nnoremap <Leader>m :Unite -force-redraw -direction=dynamicbottom -start-insert -buffer-name=mappings mapping<cr>
" nnoremap <Leader>r :UniteResume<cr>
" nnoremap <Leader>s :Unite -force-redraw -direction=dynamicbottom -start-insert -buffer-name=tehcodez codesearch<cr>
" nnoremap <Leader>t :Unite -force-redraw -direction=dynamicbottom -buffer-name=todos todo<cr>
" nnoremap <Leader>v :Unite -direction=dynamicbottom -start-insert menu:git<cr>
" nnoremap <Leader>y :Unite -force-redraw -direction=dynamicbottom -start-insert -buffer-name=yanks history/yank<cr>
" nnoremap <Leader>/ :Unite -force-redraw -direction=dynamicbottom -buffer-name=greps grep:.<cr>

" autocmd FileType unite call s:unite_my_settings()
" function! s:unite_my_settings()
"   imap <silent><buffer><expr> <C-h> unite#do_action('split')
"   imap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
" endfunction

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

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#max_list = 5

call deoplete#custom#set('_', 'matchers', ['matcher_head'])

" vim-test
let test#ruby#bundle_exec = 0

nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ts :TestSuite<CR>
nnoremap <Leader>tl :sp term://mtest\ %:<C-r>=line('.')<CR><CR>

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
let g:startify_session_dir = "$PRIVATE_DOTFILES/vim/sessions"
let g:startify_list_order = ['sessions', 'files', 'dir', 'bookmarks']

let g:startify_custom_header = map(split(system('fortune -s | cowsay'), '\n'), '"   ". v:val') + ['','']

" Rainbow
let g:rainbow_active = 1
