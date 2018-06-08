" ==============================================================================
"                                  ale
" ==============================================================================
nnoremap <F11> :ALEToggle<CR>
nnoremap <F9> :ALEFix<CR>
let b:ale_fixers = {
    \ 'javascript': ['eslint'],
    \ 'python': ['autopep8'],
    \ 'c': ['clang-format'],
    \ 'cpp': ['clang-format'],
    \ }

