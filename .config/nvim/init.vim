" Plug ---------------------------------------------------------------------{{{
call plug#begin('~/.vim/plugged')

" General ------------------------------------------------------------------{{{
let g:dispatch_no_maps = 1
Plug 'tpope/vim-sensible'
" Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-eunuch'
Plug 'airblade/vim-gitgutter'
Plug 'itchyny/lightline.vim'
Plug 'itchyny/vim-gitbranch'
Plug 'easymotion/vim-easymotion'
Plug 'romainl/vim-qf'
Plug 'sakhnik/nvim-gdb', { 'do': ':!./install.sh \| UpdateRemotePlugins' }
Plug 'scrooloose/nerdtree'
Plug 'chrisbra/NrrwRgn'
Plug 'francoiscabrol/ranger.vim'
Plug 't9md/vim-quickhl'

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
Plug 'google/vim-glaive'
Plug 'bazelbuild/vim-bazel'
" --------------------------------------------------------------------------}}}

" Syntax Highlight ---------------------------------------------------------{{{
Plug 'GutenYe/json5.vim'
Plug 'dag/vim-fish'
Plug 'octol/vim-cpp-enhanced-highlight'
" --------------------------------------------------------------------------}}}

" Colorschemes -------------------------------------------------------------{{{
Plug 'morhetz/gruvbox'
" --------------------------------------------------------------------------}}}

" Formatting ---------------------------------------------------------------{{{
Plug 'rhysd/vim-clang-format'
" --------------------------------------------------------------------------}}}

" LSP ----------------------------------------------------------------------{{{
Plug 'liuchengxu/vista.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" --------------------------------------------------------------------------}}}

" Rust ---------------------------------------------------------------------{{{
Plug 'rust-lang/rust.vim'
" --------------------------------------------------------------------------}}}

call plug#end()
" --------------------------------------------------------------------------}}}

" General config -----------------------------------------------------------{{{
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
let mapleader = " "

let g:gruvbox_contrast_dark='soft'
colorscheme gruvbox
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ }

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
set splitright
set number
set termguicolors
set shell=/bin/bash

" avoid jump when using *
noremap * m`:keepjumps normal! *``<cr>
noremap <silent><expr> <leader>g &ft=="fugitive" ? ":normal gq<cr>" : ":Gstatus<cr>"

let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
noremap <silent><expr> <leader>f &ft=="nerdtree" ? ":NERDTreeClose<cr>" : ":NERDTreeFind<cr>"

let g:ranger_map_keys = 0
noremap <silent><expr> <leader>w ":Ranger<cr>"

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25

let g:nrrw_rgn_nomap_nr = 1
vnoremap <silent><expr> <leader>nr ":NR!<cr>"
vnoremap <silent><expr> <leader>nw ":NW<cr>"

augroup cppsrc
    autocmd!
    autocmd FileType cpp setlocal foldmethod=indent |
                       \ setlocal foldnestmax=10 |
                       \ setlocal nofoldenable |
                       \ setlocal foldlevel=2
augroup END

nmap <leader>m <Plug>(quickhl-manual-this)
nmap <leader>M <Plug>(quickhl-manual-reset)

" --------------------------------------------------------------------------}}}

" folding ------------------------------------------------------------------{{{
augroup vimrc
  autocmd!
  autocmd FileType vim setlocal foldmethod=marker
augroup END
" --------------------------------------------------------------------------}}}

" HighLight ----------------------------------------------------------------{{{
augroup manifestfiles
  autocmd!
  autocmd BufRead,BufNewFile manifest.json setlocal filetype=json5
  autocmd BufRead,BufNewFile *.osl setlocal filetype=cpp
  autocmd BufRead,BufNewFile *.osl exec ":CocDisable"
augroup END
" --------------------------------------------------------------------------}}}

" Navigation ---------------------------------------------------------------{{{
nnoremap <silent> <leader>p :Files<cr>
nnoremap <silent> <leader>o :Buffers<cr>
nnoremap <silent> <leader>r :History:<cr>
nnoremap <silent> <leader>rg :execute 'Rg <c-r><c-w>'<cr>
map s <Plug>(easymotion-s)

function! s:open_branch_fzf(line)
  let l:parser = split(a:line)
  let l:branch = l:parser[0]
  if l:branch ==? '*'
    let l:branch = l:parser[1]
  endif
  let l:branch = substitute(l:branch, 'remotes/origin/', '', '')
  execute '!git checkout ' . l:branch
endfunction

command! -bang -nargs=0 Gcheckout
  \ call fzf#vim#grep(
  \   'git branch -v -a', 0,
  \   {
  \     'sink': function('s:open_branch_fzf')
  \   },
  \   <bang>0
  \ )
" --------------------------------------------------------------------------}}}

" LSP ----------------------------------------------------------------------{{{
inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<tab>'
let g:coc_snippet_prev = '<s-tab>'

let g:vista_fzf_preview = ['right:50%']
let g:vista_executive_for = {
    \ 'cpp': 'coc',
    \ }
let g:vista_sidebar_width = 90

nmap <leader>sd <Plug>(coc-definition)
nmap <leader>sf <Plug>(coc-declaration)
nmap <leader>st <Plug>(coc-type-definition)
nmap <leader>si <Plug>(coc-implementation)
nmap <leader>sr <Plug>(coc-rename)
nmap <leader>sc <Plug>(coc-references)

nnoremap <leader>ss :CocList --interactive --number-select symbols<cr>
nnoremap <leader>so :Vista!!<cr>
nnoremap <leader>sv :Vista finder coc<cr>
nnoremap <silent> <leader>sh :call <SID>show_documentation()<cr>

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

" Formatting ---------------------------------------------------------------{{{
let g:clang_format#detect_style_file = 1
let g:clang_format#auto_format = 1
" --------------------------------------------------------------------------}}}

" Rust ---------------------------------------------------------------------{{{
let g:rustfmt_autosave = 1
let g:rust_clip_command = 'xclip -selection clipboard'
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
    let l:dispatch_command = "Start /home/rosdosal/werkstatt/tools/docker/bazel_gdbserver.sh --copt=-g --copt=-O0 " . trim(a:target)
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


let g:clang_include_fixer_path = "/usr/lib/llvm-8/bin/clang-include-fixer"

function! LaunchGdb()
    let l:gdb_command = "gdb -ex 'target remote :2008' -ex 'directory " . GetBazelRootPath() . "'"
    exec "GdbStart " . l:gdb_command
endfunction

nnoremap <leader>dd :call LaunchGdb()<cr>

" --------------------------------------------------------------------------}}}
