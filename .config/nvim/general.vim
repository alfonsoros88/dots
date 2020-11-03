filetype plugin indent on

" show existing tab with 4 spaces width
set tabstop=4

" when indenting with '>', use 4 spaces width
set shiftwidth=4

" On pressing tab, insert 4 spaces
set expandtab

" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" no swap files
set noswapfile

" split orientation
set splitright
set splitbelow

" set max column width to 80 characters
set tw=80
set fo+=t
set wrap linebreak nolist

set noautochdir

" search
set ignorecase

" leader key
let mapleader = " "

set scrolloff=8

" color theme
if (has("termguicolors"))
  set termguicolors
endif
colorscheme gruvbox

" automatic remove trailing white spaces
augroup ALLTHEFILES
    autocmd!
    autocmd BufWritePre * %s/\s\+$//e
augroup END

