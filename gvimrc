scriptencoding utf-8
set encoding=utf-8
set nocompatible

filetype off

execute pathogen#infect()
execute pathogen#helptags()
syntax on
filetype plugin indent on

" Custom behaviour
let mapleader=","               " set mapleader key to comma
set background=dark             " dark background
colorscheme Tomorrow-Night      " use Tomorrow-Night as default colorscheme
set guioptions-=m               " remove menu bar
set guioptions-=T               " remove tool bar
set showmode                    " always show what mode we're currently editing in
set nomodeline                  " disable modeline
set nowrap                      " don't wrap lines
set tabstop=4                   " a tab is two spaces
set softtabstop=4               " when hitting <BS>, pretend like a tab is removed, even if spaces
set expandtab                   " expand tabs by default (overloadable per file type later)
set shiftwidth=4                " number of spaces to use for autoindenting
set shiftround                  " use multiple of shiftwidth when indenting with '<' and '>'
set backspace=indent,eol,start  " allow backspacing over everything in insert mode
set autoindent                  " always set autoindenting on
set copyindent                  " copy the previous indentation on autoindenting
set number                      " always show line numbers
set showmatch                   " set show matching parenthesis
set ignorecase                  " ignore case when searching
set smartcase                   " ignore case if search pattern is all lowercase,
                                " case-sensitive otherwise
set smarttab                    " insert tabs on the start of a line according to
                                " shiftwidth, not tabstop
set scrolloff=4                 " keep 4 lines off the edges of the screen when scrolling
set virtualedit=all             " allow the cursor to go in to "invalid" places
set hlsearch                    " highlight search terms
set incsearch                   " show search matches as you type
set gdefault                    " search/replace "globally" (on a line) by default
set fileencoding=utf-8
let &listchars="tab:\u25B8 ,nbsp:\u00BB,eol:\u00AC"
set list                        " don't show invisible characters by default,
                                " but it is enabled for some file types (see later)
set mouse=a                     " enable using the mouse if terminal emulator supports it (xterm does)
set nobackup                    " disable auto-backup
set noswapfile                  " disable swapfile
set fileformat=unix             
set fileformats=unix,dos
set laststatus=2                " display airline at all times
set columns=120 lines=30        " set window size to 120x30
set guifont=Consolas:h11
set splitbelow
set splitright
set noerrorbells visualbell t_vb=
if has('autocmd')
    autocmd GUIEnter * set visualbell t_vb=
endif

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['eslint']

" Mappings
nmap <leader>l :set list!<CR>
map <C-n> :NERDTreeToggle<CR>
nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>
nnoremap <C-Tab> :bnext<CR>
nnoremap <C-S-Tab> :bprevious<CR>
