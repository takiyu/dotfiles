" ==============================================================================
"                                    gtrans-web
" ==============================================================================
let g:gtransweb_async_mode = 1
let g:gtransweb_src_lang = 'en'
let g:gtransweb_tgt_lang = 'ja'
vnoremap <C-g>t :GtransWebPreview<CR>
vnoremap <C-g>r :GtransWebReplace<CR>
nnoremap <C-g>s :GtransWebSwapLangs<CR>
