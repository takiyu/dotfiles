" ==============================================================================
"                                  vim-go
" ==============================================================================
au FileType go nmap <Leader>i <Plug>(go-info)
au FileType go nmap <Leader>dc <Plug>(go-doc)
au FileType go nmap <Leader>db <Plug>(go-doc-browser)
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <Leader>e <Plug>(go-rename)
" let g:go_highlight_functions = 1
" let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_fmt_autosave = 1
