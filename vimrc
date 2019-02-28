scriptencoding utf-8
set encoding=utf-8
set termencoding=utf-8

if &compatible
   set nocompatible
endif

filetype off

if empty(glob('~/.vim/pack/minpac/opt/minpac'))
  silent !mkdir -p ~/.vim/pack/bundle
  silent !mkdir -p ~/.vim/pack/minpac/{start,opt}
  silent !git clone https://github.com/k-takata/minpac.git ~/.vim/pack/minpac/opt/minpac
endif

if exists('*minpac#init')
  call minpac#init()
  call minpac#add('k-takata/minpac', {'type': 'opt'})
  call minpac#add('davidhalter/jedi-vim')
  call minpac#add('dracula/vim')
  call minpac#add('eagletmt/ghcmod-vim')
  call minpac#add('eagletmt/neco-ghc')
  call minpac#add('fatih/vim-go', {'do': ':GoUpdateBinaries'})
  call minpac#add('fsharp/vim-fsharp')
  call minpac#add('junegunn/fzf.vim')
  call minpac#add('let-def/ocp-indent-vim')
  call minpac#add('mileszs/ack.vim')
  if has('nvim')
    call minpac#add('Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'})
  else
    call minpac#add('Shougo/deoplete.nvim')
    call minpac#add('roxma/nvim-yarp')
    call minpac#add('roxma/vim-hug-neovim-rpc')
  endif
  call minpac#add('Shougo/vimproc.vim', {'do': 'make'})
  call minpac#add('scrooloose/nerdtree')
  call minpac#add('ternjs/tern_for_vim', {'do': 'cd ~/.vim/pack/minpac/start/tern_for_vim && npm install'})
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-fugitive')
  call minpac#add('vim-airline/vim-airline')
  call minpac#add('vim-airline/vim-airline-themes')
  call minpac#add('vim-scripts/paredit.vim')
  call minpac#add('w0rp/ale')
endif

command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackStatus packadd minpac | source $MYVIMRC | call minpac#status()
command! PackClean packadd minpac | source $MYVIMRC | call minpac#clean()

syntax on
filetype plugin indent on

" GUI Settings:
if has('gui_running')
  set guioptions-=m                       " Remove menu bar.
  set guioptions-=T                       " Remove tool bar.
  set guioptions-=r                       " Remove right scroll bar.
  set macligatures
  set guifont=Fira\ Code:h12
  set columns=200                         " Window width.
  set lines=50                            " Window height.
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
set shiftwidth=2                        " Set number of spaces to use for auto-indenting.
set shiftround                          " Use multiple of shiftwidth when indenting with '<' and '>'.
set backspace=indent,eol,start          " Allow backspacing over everything in insert mode.
set autoindent                          " Indent by default.
set autoread                            " Read file on-change.
set copyindent                          " Copy the previous indentation on auto-indenting.
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
    autocmd FocusGained,BufEnter * :checktime
    autocmd FileType fzf tunmap <Esc>
endif

" Python binaries:
let g:python3_host_prog = '/usr/bin/python3'
let g:python_host_prog = '/usr/bin/python2'

" Ale
let g:ale_linters_explicit = 0
let g:ale_sign_error = '✗'
let g:ale_sign_warning = '⚠'
let g:airline#extensions#ale#enabled = 1

" Airline:
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:python3_host_prog = '/usr/local/bin/python3'

" Paredit:
let g:paredit_electric_return = 0
let g:clojure_align_subforms = 1

" Color Settings:
colorscheme dracula

" Mappings:
inoremap <C-h> <Esc><c-w>h
inoremap <C-j> <Esc><c-w>j
inoremap <C-k> <Esc><c-w>k
inoremap <C-l> <Esc><c-w>l
map <C-n> :NERDTreeToggle<CR>
map <Esc>[1;5D <C-Left>
map <Esc>[1;5C <C-Right>
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>
nmap <leader>b :Buffers<CR>
nmap <leader>f :Files<CR>
nmap <leader>g :GFiles<CR>
nmap <leader>h :History<CR>
nmap <leader>l :set list!<CR>
nmap <M-j> gj
nmap <M-k> gk
nmap <M-4> g$
nmap <M-6> g^
nmap <M-0> g^
nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>
nnoremap <C-S-Tab> :bprevious<CR>
nnoremap <C-Tab> :bnext<CR>
nnoremap <M-h> :bprevious<CR>
nnoremap <M-l> :bnext<CR>
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>
tnoremap <M-[> <Esc>
vmap <M-j> gj
vmap <M-k> gk
vmap <M-4> g$
vmap <M-6> g^
vmap <M-0> g^
vnoremap <C-h> <Esc><C-w>h
vnoremap <C-j> <Esc><C-w>j
vnoremap <C-k> <Esc><C-w>k
vnoremap <C-l> <Esc><C-w>l
