nnoremap <leader>p :Files<cr>
nnoremap <leader>o :Buffers<cr>
nnoremap <leader><c-r> :History:<cr>
nnoremap <leader>rg :Rg <c-r><c-w><cr>
nnoremap <leader>gc :BCommits<cr>
nnoremap <leader>gb :GBranches<cr>

nnoremap <leader>gp :GFiles?<cr>

" let g:fzf_preview_git_status_preview_command =
"     \ "[[ $(git diff --cached -- {-1}) != \"\" ]] && git diff --cached --color=always -- {-1} | delta || " .
"     \ "[[ $(git diff -- {-1}) != \"\" ]] && git diff --color=always -- {-1} | delta || " .
"     \ g:fzf_preview_command
