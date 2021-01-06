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

function! PackInit() abort
  packadd minpac

  call minpac#init()
  call minpac#add('k-takata/minpac', {'type': 'opt'})
  call minpac#add('arcticicestudio/nord-vim')
  call minpac#add('bhurlow/vim-parinfer')
  call minpac#add('chriskempson/base16-vim')
  call minpac#add('derekwyatt/vim-scala')
  call minpac#add('eagletmt/ghcmod-vim')
  call minpac#add('eagletmt/neco-ghc')
  call minpac#add('easymotion/vim-easymotion')
  call minpac#add('elixir-editors/vim-elixir')
  call minpac#add('elmcast/elm-vim')
  call minpac#add('fatih/vim-go', {'do': ':GoUpdateBinaries'})
  call minpac#add('fsharp/vim-fsharp', {'do': '!make fsautocomplete'})
  call minpac#add('guns/vim-sexp')
  call minpac#add('haya14busa/incsearch.vim')
  call minpac#add('haya14busa/incsearch-easymotion.vim')
  call minpac#add('junegunn/fzf.vim')
  call minpac#add('kien/rainbow_parentheses.vim')
  call minpac#add('let-def/ocp-indent-vim')
  call minpac#add('mattn/emmet-vim')
  call minpac#add('mileszs/ack.vim')
  call minpac#add('neoclide/coc.nvim', {'branch': 'release'})
  call minpac#add('Olical/conjure', {'tag': 'v2.1.2'})
  call minpac#add('prettier/vim-prettier', {'do': '!npm install'})
  call minpac#add('racer-rust/vim-racer')
  call minpac#add('reasonml-editor/vim-reason-plus')
  call minpac#add('rust-lang/rust.vim')
  call minpac#add('scrooloose/nerdtree')
  call minpac#add('Shougo/vimproc.vim', {'do': '!make'})
  call minpac#add('ternjs/tern_for_vim', {'do': '!npm install'})
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-fugitive')
  call minpac#add('vim-airline/vim-airline')
  call minpac#add('vim-airline/vim-airline-themes')
  call minpac#add('w0rp/ale')
endfunction

command! PackUpdate source $MYVIMRC | call PackInit() | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean source $MYVIMRC | call PackInit() | call minpac#clean()
command! PackStatus packadd minpac | call minpac#status()

syntax on
filetype plugin indent on

let &t_8f = "\<Esc>[38:2:%lu:%lu:%lum"
let &t_8b = "\<Esc>[48:2:%lu:%lu:%lum"

if exists("$TMUX")
    let &t_SI .= "\<Esc>Ptmux;\<Esc>\<Esc>[1 q\<Esc>\\"
    let &t_SR .= "\<Esc>Ptmux;\<Esc>\<Esc>[1 q\<Esc>\\"
    let &t_EI .= "\<Esc>Ptmux;\<Esc>\<Esc>[2 q\<Esc>\\"
else
    let &t_SI .= "\<Esc>[1 q"
    let &t_SR .= "\<Esc>[1 q"
    let &t_EI .= "\<Esc>[2 q"
endif

if has('termguicolors')
  set termguicolors
endif

" GUI Settings:
if has('gui_running')
  set guioptions-=m                       " Remove menu bar.
  set guioptions-=T                       " Remove tool bar.
  set guioptions-=r                       " Remove right scroll bar.
  set macligatures
  set guifont=Source\ Code\ Pro\ for\ Powerline:h12
  set columns=231                         " Window width.
  set lines=66                            " Window height.
endif

" Color Settings:
colorscheme nord

" Custom Behaviour:
let mapleader=","                       " Set mapleader key to comma.
let &listchars="tab:\u25B8 ,nbsp:\u00BB,eol:\u00AC"
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
set updatetime=300
set shortmess+=c

if has('autocmd')
    autocmd GUIEnter * set visualbell t_vb=
    autocmd FocusGained,BufEnter * :checktime
    autocmd FileType fzf silent! tunmap <Esc>
endif

" Python binaries
let g:python3_host_prog = '/usr/bin/python3'
let g:python_host_prog = '/usr/bin/python2'

" Fuzzy Finder
let g:fzf_preview_window = 'right:50%'
let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.9 } }

" Ale
let g:ale_fix_on_save = 1
let g:ale_fixers = {
      \ 'javascript': ['prettier'],
      \ 'css': ['prettier']
      \ }
let g:ale_linters = {
      \ 'go': ['gopls']
      \ }
let g:ale_linters_explicit = 1
let g:ale_sign_error = '✗'
let g:ale_sign_warning = '⚠'
let g:airline#extensions#ale#enabled = 1

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'nord'

" Emmet
let g:user_emmet_leader_key = '<TAB>'
let g:user_emmet_settings = {'javascript.jsx': {'extends': 'jsx'}}

" Prettier
let g:prettier#auto_format_config_present = 1

" Rainbow Parentheses
autocmd VimEnter * RainbowParenthesesToggle
autocmd Syntax * RainbowParenthesesLoadRound
autocmd Syntax * RainbowParenthesesLoadSquare
autocmd Syntax * RainbowParenthesesLoadBraces

" OCaml
if executable('opam')
  let g:opam_share = substitute(system('opam config var share'), '\n$', '', '''')
  execute "set rtp+=" . g:opam_share . "/merlin/vim"
endif

" Mappings
inoremap <C-c> <Esc><C-w>c
inoremap <C-h> <Esc><C-w>h
inoremap <C-j> <Esc><C-w>j
inoremap <C-k> <Esc><C-w>k
inoremap <C-l> <Esc><C-w>l
inoremap <C-o> <Esc><C-w>o
inoremap <silent><expr> <C-Space> coc#refresh()
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
map <C-n> :NERDTreeToggle<CR>
map <Esc>[1;5D <C-Left>
map <Esc>[1;5C <C-Right>
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>
map <leader> <plug>(easymotion-prefix)
map / <plug>(incsearch-forward)
map ? <plug>(incsearch-backward)
map <leader>/ <plug>(incsearch-easymotion-/)
map <leader>? <plug>(incsearch-easymotion-?)
noremap <space> :
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
nmap <silent> [g <plug>(coc-diagnostic-prev)
nmap <silent> ]g <plug>(coc-diagnostic-next)
nnoremap + <C-w>+
nnoremap - <C-w>-
nnoremap > <C-w>>
nnoremap < <C-w><
nnoremap _ <C-w>
nnoremap <bar> <C-w>|
nnoremap <C-R> <C-W>R
nnoremap <C-T> <C-W>T
nnoremap <C-c> <C-w>c
nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>
nnoremap <C-o> <C-w>o
nnoremap <C-S-Tab> :bprevious<CR>
nnoremap <C-Tab> :bnext<CR>
nnoremap <M-Left> :bprevious<CR>
nnoremap <M-Right> :bnext<CR>
nnoremap ˙ :bprevious<CR>
nnoremap ¬ :bnext<CR>
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

" Scala
autocmd BufRead,BufNewFile *.sbt set FileType=scala
