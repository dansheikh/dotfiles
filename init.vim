scriptencoding utf-8
set encoding=utf-8
set termencoding=utf-8

if &compatible
   set nocompatible
endif

filetype off

let s:bundle=$DOTFILES . "/vim/bundle"
execute "set rtp+=" . s:bundle . "/repos/github.com/Shougo/dein.vim"

if dein#load_state(s:bundle)
  call dein#begin(s:bundle)
  call dein#add('chriskempson/base16-vim')
  call dein#add('dracula/vim')
  call dein#add('eagletmt/ghcmod-vim')
  call dein#add('eagletmt/neco-ghc')
  call dein#add('fatih/vim-go')
  call dein#add('fsharp/vim-fsharp')
  call dein#add('guns/vim-clojure-static')
  call dein#add('kien/rainbow_parentheses.vim')
  call dein#add('let-def/ocp-indent-vim')
  call dein#add('mileszs/ack.vim')
  call dein#add('neomake/neomake')
  call dein#add('scrooloose/nerdtree')
  call dein#add('Shougo/dein.vim')
  call dein#add('Shougo/denite.nvim')
  call dein#add('Shougo/vimproc.vim', {'build': 'make'})
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('carlitux/deoplete-ternjs', {'build': 'npm install -g tern', 'depends': ['deoplete.nvim']})
  call dein#add('tpope/vim-fireplace')
  call dein#add('tpope/vim-fugitive')
  call dein#add('tpope/vim-surround')
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('vim-scripts/paredit.vim')
  call dein#add('zchee/deoplete-jedi', {'depends': ['deoplete.nvim']})
  call dein#add('zchee/deoplete-clang', {'depends': ['deoplete.nvim']})
  call dein#add('zchee/deoplete-go', {'build': 'make', 'depends': ['deoplete.nvim']})
  call dein#end()
  call dein#save_state()
endif

syntax on
filetype plugin indent on

" GUI Settings:
if has('gui_running')
  set guioptions-=m                       " Remove menu bar.
  set guioptions-=T                       " Remove tool bar.
  set guioptions-=r                       " Remove right scroll bar.
  set guifont=Droid\ Sans\ Mono\ Slashed\ for\ Powerline\ 11
endif

" Custom Behaviour:
let mapleader=","                       " Set mapleader key to comma.
set t_Co=256                            " Set to 256 colors.
set background=dark                     " Darken background.
set showmode                            " Always display current mode.
set nomodeline                          " Disable modeline.
set wrap                                " Wrap lines.
set linebreak
set nolist
set tabstop=2                           " Set tab to two spaces.
set softtabstop=2                       " On <BS> key press, even in the event of spaces, pretend as if a tab is removed.
set expandtab                           " Expand tabs by default.
set shiftwidth=2                        " Set number of spaces to use for autoindenting.
set shiftround                          " Use multiple of shiftwidth when indenting with '<' and '>'.
set backspace=indent,eol,start          " Allow backspacing over everything in insert mode.
set autoindent                          " Auto-indenting by default.
set copyindent                          " Copy the previous indentation on autoindenting.
set number                              " Always show line numbers.
set showmatch                           " Show matching parenthesis.
set ignorecase                          " Ignore case when searching.
set smartcase                           " Ignore case if search pattern is all lowercase, case-sensitive otherwise.
set smarttab                            " Insert tabs on the start of a line according to shiftwidth, not tabstop.
set scrolloff=4                         " Keep 4 lines off the edges of the screen when scrolling.
set virtualedit=all                     " Allow the cursor to go in to 'invalid' places.
set hlsearch                            " Highlight search terms.
set incsearch                           " Show search matches as typed.
set gdefault                            " Search/replace 'globally' (on a line) by default.
set fileencoding=utf-8
let &listchars="tab:\u25B8 ,nbsp:\u00BB,eol:\u00AC"
set list                                " Disable invisible characters by default, enable on-demand.
set mouse=a                             " Enable mouse if terminal emulator support is available.
set nobackup                            " Disable auto-backup.
set noswapfile                          " Disable swapfile.
set fileformat=unix
set fileformats=unix,dos
set laststatus=2                        " Display airline at all times.
set splitbelow
set splitright
set noerrorbells visualbell t_vb=
set completeopt=menuone,longest,preview,noinsert

if has('autocmd')
    autocmd GUIEnter * set visualbell t_vb=
endif

" Neomake:
autocmd! BufWritePost,BufEnter * Neomake
let g:neomake_open_list = 2
let g:neomake_cpp_clang_maker = {'exe': 'clang++', 'args': ['-Wall', '-Wextra', '-Weverything', '-Wpedantic']}
let g:neomake_ocaml_enabled_makers = ['merlin']
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_python_enabled_makers = ['flake8']
let g:neomake_go_enabled_makers = ['go', 'gometalinter']
let g:neomake_haskell_enabled_makers = ['hlint']
let g:neomake_cpp_enabled_makers = ['clang']

" Neovim Python:
let g:python3_host_prog = '/usr/bin/python3'
let g:python_host_prog = '/usr/bin/python2'

" OCaml:
let s:merlin=substitute(system('opam config var share'),'\n$','','g') . "/merlin/vim"
execute "set rtp+=" . s:merlin

" Runtime Path Configurations:
if exists('$DOTFILES')
  let s:ocp_indent=$DOTFILES."/vim/bundle/ocp-indent-vim"
  let s:base16=$DOTFILES."/vim/bundle/base16-vim"
  execute "set rtp+=" . s:ocp_indent
  execute "set rtp+=" . s:base16
endif

" Airline:
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'

" Deoplete:
let g:deoplete#enable_at_startup = 1

" Denite:
nnoremap <leader>f :Denite -direction=dynamicbottom -auto-preview file_rec<CR>
nnoremap <leader>b :Denite -direction=dynamicbottom -auto-preview buffer<CR>

" Paredit:
let g:paredit_electric_return = 0
let g:clojure_align_subforms = 1

" JavaScript:
au BufNewFile,BufRead *.js setlocal tabstop=4 softtabstop=4 shiftwidth=4

" Python:
au BufNewFile,BufRead *.py setlocal tabstop=4 softtabstop=4 shiftwidth=4

" Color Settings:
colorscheme dracula     " Use dracula as default color scheme.

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

" Mappings:
map <C-n> :NERDTreeToggle<CR>
map <ESC>[C <C-Right>
map <ESC>[D <C-Left>
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>
nmap <leader>l :set list!<CR>
nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>
nnoremap <C-Tab> :bnext<CR>
nnoremap <C-S-Tab> :bprevious<CR>
nnoremap <C-Right> :bnext<CR>
nnoremap <C-Left> :bprevious<CR>
vmap <M-j> gj
vmap <M-k> gk
vmap <M-4> g$
vmap <M-6> g^
vmap <M-0> g^
nmap <M-j> gj
nmap <M-k> gk
nmap <M-4> g$
nmap <M-6> g^
nmap <M-0> g^
