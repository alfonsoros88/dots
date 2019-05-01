call plug#begin('~/.vim/plugged')
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'easymotion/vim-easymotion'
Plug 'christoomey/vim-tmux-navigator'
Plug 'morhetz/gruvbox'
call plug#end()

" easymotion
nmap s <Plug>(easymotion-s)

" space instead of tab
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

colorscheme gruvbox
