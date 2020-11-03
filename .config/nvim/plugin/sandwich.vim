let g:sandwich_no_default_key_mappings = 1

nmap <leader>wa <Plug>(operator-sandwich-add)
nmap <leader>wd <Plug>(operator-sandwich-delete)
nmap <leader>wr <Plug>(operator-sandwich-delete)

omap ib <Plug>(textobj-sandwich-auto-i)
omap ab <Plug>(textobj-sandwich-auto-a)
omap is <Plug>(textobj-sandwich-query-i)
omap as <Plug>(textobj-sandwich-query-a)
