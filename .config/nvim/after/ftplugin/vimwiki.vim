" function! Foldexpr_markdown(lnum)
"     let l0 = getline(a:lnum)

"     " keep track of fenced code blocks
"     if l0 =~ '````*' || l0 =~ '\~\~\~\~*'
"         if b:fenced_block == 0
"             let b:fenced_block = 1
"             return 'a1'
"         elseif b:fenced_block == 1
"             let b:fenced_block = 0
"             return 's1'
"         endif
"     endif

"     if b:fenced_block == 1
"         " keep previous foldlevel
"         return '='
"     endif

"     let l2 = getline(a:lnum+1)
"     if  l2 =~ '^==\+\s*'
"         " next line is underlined (level 1)
"         return '>1'
"     elseif l2 =~ '^--\+\s*'
"         " next line is underlined (level 2)
"         return '>2'
"     endif

"     if l0 =~ '^#'
"         " previous line starts with hashes
"         return '>'.matchend(l0, '^#\+')
"     else
"         " keep previous foldlevel
"         return '='
"     endif
" endfunction

" let b:fenced_block = 0

" setlocal foldexpr=Foldexpr_markdown(v:lnum)
" setlocal foldmethod=expr
" setlocal foldlevel=1
