" ==============================================================================
"                                        caw
" ==============================================================================
nmap \c <Plug>(caw:zeropos:toggle)
vmap \c <Plug>(caw:zeropos:toggle)
nmap \C <Plug>(caw:zeropos:uncomment)
vmap \C <Plug>(caw:zeropos:uncomment)
autocmd FileType glsl let b:caw_oneline_comment = '//'
