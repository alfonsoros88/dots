call plug#begin('~/.vim/plugged')
" general
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'stsewd/fzf-checkout.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'airblade/vim-gitgutter'
Plug 'easymotion/vim-easymotion'
Plug 'wellle/targets.vim'
Plug 'machakann/vim-sandwich'
Plug 'jiangmiao/auto-pairs'
Plug 'AndrewRadev/splitjoin.vim'
" Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" tmux
Plug 'christoomey/vim-tmux-navigator'

" multipl word highlight
Plug 'inkarkat/vim-ingo-library'
Plug 'inkarkat/vim-mark'

" automatic highlight disable
Plug 'haya14busa/incsearch.vim'
Plug 'haya14busa/incsearch-easymotion.vim'

" vimwiki
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'vimwiki/vimwiki'

" rust
Plug 'rust-lang/rust.vim'

" themes
Plug 'morhetz/gruvbox'

" status line
Plug 'itchyny/lightline.vim'
Plug 'itchyny/vim-gitbranch'

" fish support
Plug 'dag/vim-fish'

" toml files
Plug 'cespare/vim-toml'

" snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" autocomplete LSP
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'liuchengxu/vista.vim'

" C++
Plug 'rhysd/vim-clang-format'
call plug#end()
