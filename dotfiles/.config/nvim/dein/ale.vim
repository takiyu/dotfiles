" ==============================================================================
"                                  ale
" ==============================================================================
nnoremap <F11> :ALEToggle<CR>
nnoremap <F9> :ALEFix<CR>
let g:ale_fixers = {
    \ 'javascript': ['eslint'],
    \ 'python': ['autopep8'],
    \ 'c': ['clang-format'],
    \ 'cpp': ['clang-format'],
    \ }

let g:ale_lint_on_text_changed = 0
let g:ale_completion_delay = 50
