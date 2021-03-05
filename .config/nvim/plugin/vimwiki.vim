nnoremap <leader>x :VimwikiToggleListItem<cr>

let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]

let g:vimwiki_global_ext = 0

" let g:vimwiki_folding = 'custom'

" vimwiki overrides coc tab completion
au filetype vimwiki silent! iunmap <buffer> <Tab>
