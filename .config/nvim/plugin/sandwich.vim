let g:sandwich_no_default_key_mappings = 1
let g:operator_sandwich_no_default_key_mappings = 1

map <leader>wa <Plug>(operator-sandwich-add)
map <leader>wd <Plug>(operator-sandwich-delete)
map <leader>wc <Plug>(operator-sandwich-replace)

omap ib <Plug>(textobj-sandwich-auto-i)
omap ab <Plug>(textobj-sandwich-auto-a)
omap is <Plug>(textobj-sandwich-query-i)
omap as <Plug>(textobj-sandwich-query-a)


