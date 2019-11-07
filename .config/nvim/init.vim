" Plug ---------------------------------------------------------------------{{{
call plug#begin('~/.vim/plugged')

" General ------------------------------------------------------------------{{{
let g:dispatch_no_maps = 1
let g:nvimgdb_disable_start_keymaps = 1

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-obsession'
Plug 'airblade/vim-gitgutter'
Plug 'itchyny/lightline.vim'
Plug 'easymotion/vim-easymotion'
Plug 'romainl/vim-qf'
Plug 'sakhnik/nvim-gdb', { 'do': ':!./install.sh \| UpdateRemotePlugins' }
" --------------------------------------------------------------------------}}}

" FZF ----------------------------------------------------------------------{{{
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" --------------------------------------------------------------------------}}}

" tmux ---------------------------------------------------------------------{{{
Plug 'christoomey/vim-tmux-navigator'
" --------------------------------------------------------------------------}}}

" Bazel --------------------------------------------------------------------{{{
Plug 'google/vim-maktaba'
Plug 'bazelbuild/vim-bazel'
" --------------------------------------------------------------------------}}}

" Syntax Highlight ---------------------------------------------------------{{{
Plug 'GutenYe/json5.vim'
" Plug 'dag/vim-fish'
" --------------------------------------------------------------------------}}}

" Colorschemes -------------------------------------------------------------{{{
Plug 'morhetz/gruvbox'
" --------------------------------------------------------------------------}}}

" LSP ----------------------------------------------------------------------{{{
Plug 'liuchengxu/vista.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Plug 'Valloric/YouCompleteMe', {'do': './install.py --clang-completion --clangd-completion --rust-completer' }
" --------------------------------------------------------------------------}}}

call plug#end()
" --------------------------------------------------------------------------}}}

" General config -----------------------------------------------------------{{{
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
let mapleader = " "
colorscheme gruvbox
let g:lightline = { 'colorscheme': 'wombat', }
set ignorecase
set smartcase

set hidden
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes
set mouse=a

set shell=/bin/bash\ --login
" --------------------------------------------------------------------------}}}

" folding ------------------------------------------------------------------{{{
augroup vimrc
  autocmd!
  autocmd BufReadPre * setlocal foldmethod=marker
augroup END
" --------------------------------------------------------------------------}}}

" HighLight ----------------------------------------------------------------{{{
augroup manifestfiles
  autocmd!
  autocmd BufRead,BufNewFile manifest.json setlocal filetype=json5
  autocmd BufRead,BufNewFile *.osl setlocal filetype=cpp
  autocmd BufRead,BufNewFile *.osl let b:coc_enabled = 0
augroup END
" --------------------------------------------------------------------------}}}

" Navigation ---------------------------------------------------------------{{{
nnoremap <leader>p :Files<cr>
nnoremap <leader>o :Buffers<cr>
nnoremap <leader>r :History:<cr>
nnoremap <leader>rg :execute 'Rg <c-r><c-w>'<cr>
nmap s <Plug>(easymotion-s)
" --------------------------------------------------------------------------}}}

" LSP ----------------------------------------------------------------------{{{
" let g:ycm_clangd_binary_path = "/usr/bin/clangd-6.0"
" nnoremap <leader>tg :YcmCompleter GoToDefinition<cr>
" nnoremap <leader>td :YcmCompleter GoToDeclaration<cr>
" nnoremap <leader>ti :YcmCompleter GoToInclude<cr>
" nnoremap <leader>th :YcmCompleter GetDoc<cr>
" nnoremap <leader>tt :YcmCompleter GetType<cr>

inoremap <silent><expr> <tab>
    \ pumvisible() ? "\<c-n>" :
    \ <sid>check_back_space() ? "\<tab>" :
    \ coc#refresh()
inoremap <expr> <s-tab> pumvisible() ? "\<c-p>": "\<c-h>"

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1] =~# '\s'
endfunction

"inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? "\<c-y>" : "\<c-g>u\<cr>"

let g:vista_fzf_preview = ['right:50%']
let g:vista_executive_for = {
    \ 'cpp': 'coc',
    \ }
let g:vista_sidebar_width = 90

nmap <leader>sd <Plug>(coc-definition)
nmap <leader>sf <Plug>(coc-declaration)
nmap <leader>si <Plug>(coc-implementation)
nmap <leader>sr <Plug>(coc-rename)
nmap <leader>sc <Plug>(coc-references)
nnoremap <leader>so :Vista!!<cr>
nnoremap <leader>ss :Vista finder coc<cr>
nnoremap <leader>sh :call <SID>show_documentation()<cr>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

augroup coc
    autocmd!
    autocmd CursorHold * silent call CocActionAsync('highlight')
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end
" --------------------------------------------------------------------------}}}

" Autocmd Bazel ------------------------------------------------------------{{{
let g:bazel_command = "~/werkstatt/tools/docker/bazel.py"

function! GetBazelRootPath() 
  let l:path =  getcwd()
  " Search upward for WORKSPACE.
  let l:file = findfile('WORKSPACE', l:path . ';')
  if !empty(l:file)
    " Get the absolute path and strip 'WORKSPACE'
    return fnamemodify(l:file, ':p:h')
  else
    throw maktaba#error#NotFound('No workspace found')
  endif
endfunction

function! DispatchBazelBuild(...)
    if (a:0 > 0)
        let l:bazel_target = a:1
    else
        let l:bazel_fullname = trim(system(g:bazel_command . ' query --noshow_progress ' . expand('%')))
        let l:bazel_package = substitute(l:bazel_fullname, ":.*", "", "")
        let l:bazel_target = trim(system(g:bazel_command . ' query --noshow_progress "attr(srcs, ' . l:bazel_fullname . ', ' . l:bazel_package . ':*)"'))
    endif
    let l:dispatch_command = "Dispatch -compiler=gcc " . g:bazel_command . " build --color=no -- " . l:bazel_target
    call setreg("t", l:bazel_target)
    call histadd("cmd", l:dispatch_command)
    silent exec l:dispatch_command
endfunction

function! DispatchBazelTest(...)
    if (a:0 > 0)
        let l:bazel_test = a:1
    else
        let l:bazel_fullname = trim(system(g:bazel_command . ' query --noshow_progress ' . expand('%')))
        let l:bazel_package = substitute(l:bazel_fullname, ":.*", "", "")
        let l:bazel_target = trim(system(g:bazel_command . ' query --noshow_progress "attr(srcs, ' . l:bazel_fullname . ', ' . l:bazel_package . ':*)"'))
        let l:bazel_test = trim(system(g:bazel_command . ' query --noshow_progress "tests(' . l:bazel_target . ')"'))
    endif
    let g:last_bazel_target = l:bazel_test
    let l:dispatch_command = "Dispatch -compiler=gcc " . g:bazel_command . " test --color=no --cache_test_results=no --test_output=all " . l:bazel_test
    call setreg("t", l:bazel_test)
    call histadd("cmd", l:dispatch_command)
    silent exec l:dispatch_command
endfunction

function! StartBazelDebug(target)
    let l:dispatch_command = "Start bd --copt=-ggdb " . trim(a:target)
    call histadd("cmd", l:dispatch_command)
    exec l:dispatch_command
endfunction

function! StartBazelDebugProxy(...)
    if (a:0 > 0)
        call StartBazelDebug(a:1)
    else
        let l:bazel_targets_command = g:bazel_command . ' query --noshow_progress "kind(\"cc_.*\", //...)"'
        call fzf#run({ 'source': l:bazel_targets_command, 'down': '40%', 'sink': function('StartBazelDebug')})
    end
endfunction

function! BazelCompletionListWrapper(unused_arg, line, pos) abort
    let l:new_line = substitute(a:line[0:a:pos], '\v\w+', 'bazel build', '')
    return bazel#CompletionList(a:unused_arg, l:new_line, a:pos)
endfunction

command! -nargs=? -complete=customlist,BazelCompletionListWrapper BazelBuild call DispatchBazelBuild(<f-args>)
command! -nargs=? -complete=customlist,BazelCompletionListWrapper BazelDebug call StartBazelDebugProxy(<f-args>)
command! -nargs=? -complete=customlist,BazelCompletionListWrapper BazelTest call DispatchBazelTest(<f-args>)

nnoremap <expr> <leader>bb ':BazelBuild ' . getreg("t")
nnoremap <expr> <leader>bd ':BazelDebug ' . getreg("t")
nnoremap <expr> <leader>bt ':BazelTest ' . getreg("t")


function! FormatBuffer()
    let l:cursor = getcurpos()
    if &ft =~# '^\%(c\|cpp\)$'
        silent exec "%!clang-format -style=File"
    elseif &ft =~# '^\%(bzl\)$'
        silent exec "%!buildifier"
    endif
    call setpos('.', l:cursor)
endfunction

let g:clang_include_fixer_path = "/usr/lib/llvm-8/bin/clang-include-fixer"

let g:nvimgdb_config_override = {
            \ 'key_until':      '<leader>du',
            \ 'key_continue':   '<leader>dc',
            \ 'key_next':       '<leader>dn',
            \ 'key_step':       '<leader>ds',
            \ 'key_finish':     '<leader>df',
            \ 'key_breakpoint': '<leader>db',
            \ 'key_frameup':    '<leader>dk',
            \ 'key_framedown':  '<leader>dj',
            \ 'key_eval':       '<leader>de',
            \ 'split_command': 'split'
            \ }

function! LaunchGdb()
    "let l:gdb_command = "gdb -ex 'target remote :2008' -ex 'directory " . GetBazelRootPath() . "'"
    let l:gdb_command = "gdb -ex 'target remote :2008' -ex 'directory " . GetBazelRootPath() . "'"
    exec "GdbStart " . l:gdb_command
endfunction

nnoremap <leader>dd :call LaunchGdb()<cr>

augroup bazelproject
    autocmd!
    autocmd BufWritePost * call FormatBuffer()
augroup END
" --------------------------------------------------------------------------}}}
