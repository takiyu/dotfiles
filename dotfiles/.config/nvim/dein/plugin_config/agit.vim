" ==============================================================================
"                                    agit
" ==============================================================================
let g:agit_enable_auto_show_commit = 0
autocmd FileType agit_stat nmap <silent><buffer><CR> <Plug>(agit-diff)
