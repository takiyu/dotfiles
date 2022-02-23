" ==============================================================================
"                                  neosnippet
" ==============================================================================
let g:neosnippet#disable_runtime_snippets = {'_' : 1 }
" For snippet_complete marker
if has('conceal')
    " set conceallevel=2
    set concealcursor=niv
endif
" スニペットファイルの保存ディレクトリのパスを登録
let g:neosnippet#snippets_directory =
    \ g:dein_cache_path . 'repos/github.com/takiyu/my-vim-snippets/snippets'
