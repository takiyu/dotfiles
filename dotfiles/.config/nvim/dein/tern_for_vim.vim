" ==============================================================================
"                                  tern_for_vim
" ==============================================================================
" let g:tern_show_argument_hints = 'on_move'
" let g:tern_show_argument_hints = 'on_hold'
" au FileType javascript nmap <silent> <Leader>t :TernType<CR>
au FileType javascript nmap <silent> <c-t> :TernType<CR>
au FileType javascript nmap <silent> <Leader>d :TernDoc<CR>
let g:tern_show_signature_in_pum = 1

" For Ubuntu command
" let g:tern#command = ["nodejs", expand('$HOME').
"                    \ '/.vim/bundle/tern_for_vim/node_modules/tern/bin/tern',
"                    \ '--no-port-file']
