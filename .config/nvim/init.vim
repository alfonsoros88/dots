source $HOME/.config/nvim/plug.vim
source $HOME/.config/nvim/general.vim

nnoremap <leader>cv :call EditVimConfig()<cr>

function! EditVimConfig() abort
    tabnew $MYVIMRC
    lcd %:p:h
    setlocal path=.,**,,
endfunction
