scriptencoding utf-8
set encoding=utf-8
set termencoding=utf-8
set nocompatible

filetype off

execute pathogen#infect()
execute pathogen#helptags()
syntax on
filetype plugin indent on

" Custom behaviour
let mapleader=","                       " set mapleader key to comma
set t_Co=256                            " set to 256 colors
set term=xterm-256color                 " set to xterm mode
set background=dark                     " darken background
colorscheme Tomorrow-Night-Eighties     " use Tomorrow-Night as default colorscheme
set guioptions-=m                       " remove menu bar
set guioptions-=T                       " remove tool bar
set showmode                            " always show what mode we're currently editing in
set nomodeline                          " disable modeline
set wrap                                " wrap lines
set linebreak
set nolist
set tabstop=2                           " a tab is two spaces
set softtabstop=2                       " when hitting <BS>, pretend like a tab is removed, even if spaces
set expandtab                           " expand tabs by default (overloadable per file type later)
set shiftwidth=2                        " number of spaces to use for autoindenting
set shiftround                          " use multiple of shiftwidth when indenting with '<' and '>'
set backspace=indent,eol,start          " allow backspacing over everything in insert mode
set autoindent                          " always set autoindenting on
set copyindent                          " copy the previous indentation on autoindenting
set number                              " always show line numbers
set showmatch                           " set show matching parenthesis
set ignorecase                          " ignore case when searching
set smartcase                           " ignore case if search pattern is all lowercase,
                                        " case-sensitive otherwise
set smarttab                            " insert tabs on the start of a line according to
                                        " shiftwidth, not tabstop
set scrolloff=4                         " keep 4 lines off the edges of the screen when scrolling
set virtualedit=all                     " allow the cursor to go in to "invalid" places
set hlsearch                            " highlight search terms
set incsearch                           " show search matches as you type
set gdefault                            " search/replace "globally" (on a line) by default
set fileencoding=utf-8
let &listchars="tab:\u25B8 ,nbsp:\u00BB,eol:\u00AC"
set list                                " don't show invisible characters by default,
                                        " but it is enabled for some file types (see later)
set mouse=a                             " enable using the mouse if terminal emulator supports it (xterm does)
set nobackup                            " disable auto-backup
set noswapfile                          " disable swapfile
set fileformat=unix
set fileformats=unix,dos
set laststatus=2                        " display airline at all times
set guifont=Droid\ Sans\ Mono\ Slashed\ for\ Powerline\ 10
set splitbelow
set splitright
set noerrorbells visualbell t_vb=
if has('autocmd')
    autocmd GUIEnter * set visualbell t_vb=
endif

let s:merlin=substitute(system('opam config var share'),'\n$','','g') . "/merlin/vim"
set rtp+=s:merlin

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

" NeoComplete
let g:acp_enableAtStartup = 0 " Disable AutoComplPop.
let g:neocomplete#enable_at_startup = 1 " Use neocomplete.
let g:neocomplete#enable_smart_case = 1 " Use smartcase.
let g:neocomplete#sources#syntax#min_keyword_length = 3 " Set minimum syntax keyword length.

" Unite
nnoremap <leader>f :Unite -direction=dynamicbottom -start-insert -auto-preview file<CR>
nnoremap <leader>b :Unite -direction=dynamicbottom -quick-match -auto-preview buffer<CR>

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_python_checkers = ['flake8']
let g:syntastic_ruby_checkers = ['rubocop']

" Python
au BufNewFile, BufRead *.py
  \ set tabstop=4
  \ set softtabstop=4
  \ set shiftwidth=4
  \ set expandtab
  \ set autoindent

" Mappings
map <C-n> :NERDTreeToggle<CR>
map <ESC>[C <C-Right>
map <ESC>[D <C-Left>
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
