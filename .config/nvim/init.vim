source $HOME/.config/nvim/general.vim
source $HOME/.config/nvim/plug.vim

colorscheme gruvbox
nnoremap <leader>cv :call EditVimConfig()<cr>

function! EditVimConfig() abort
    tabnew $MYVIMRC
    lcd %:p:h
    setlocal path=.,**,,
endfunction

" TODO find a better place to put this
let g:AutoPairs['<']='>'

