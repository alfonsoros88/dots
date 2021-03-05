" replaces nerdtree
noremap <leader>n :CocCommand explorer<cr>

nmap <leader>sd <Plug>(coc-definition)
nmap <leader>sx <Plug>(coc-declaration)
nmap <leader>sc <Plug>(coc-references)
nmap <leader>sr <Plug>(coc-rename)
nmap <leader>st <Plug>(coc-type-definition)

" code action
nmap <silent> <leader>sa <Plug>(coc-codeaction)
nmap <silent> <leader>ca <Plug>(coc-codeaction-cursor)

" range select
nmap <silent> <leader>rs <Plug>(coc-range-select)
xmap <silent> <leader>rs <Plug>(coc-range-select)

nnoremap <leader>cr :CocRestart<cr>

nmap [w <Plug>(coc-diagnostic-prev)
nmap ]w <Plug>(coc-diagnostic-next)

inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<c-n>'
let g:coc_snippet_prev = '<c-p>'

" function text object
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" edit snippets
nnoremap <leader>se :CocCommand snippets.editSnippets<cr>
nnoremap <silent><nowait> <leader>ss  :<C-u>CocList -I symbols<cr>

" show documentation
nnoremap <silent> <leader>sh :call <SID>show_documentation()<CR>

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Use c-j and c-k in dropdowns to navigate too std
inoremap <silent><expr> <c-j> pumvisible() ? "\<c-n>"
                              \: "\<esc>:TmuxNavigateDown\<CR>"
inoremap <silent><expr> <c-k> pumvisible() ? "\<c-p>"
                              \: "\<esc>:TmuxNavigateUp\<CR>"

