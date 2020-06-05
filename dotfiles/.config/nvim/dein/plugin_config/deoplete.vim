" ==============================================================================
"                                  deoplete
" ==============================================================================
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option({
    \ 'auto_complete': v:true,
    \ 'auto_complete_delay': 0,
    \ 'ignore_case': v:true,
    \ 'smart_case': v:true,
    \ 'refresh_always': v:false,
    \ 'enable_buffer_path': v:true,
    \ 'max_list': 500,
    \ 'skip_multibyte': v:true,
    \ })

" Enterで次へジャンプ、または補完を決定
imap <expr><CR> neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" :
              \ (pumvisible() ? "\<Plug>(neosnippet_expand)" :"\<CR>")
" Ctrl+Enterで次へジャンプ
imap <expr><C-CR> neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" : "\<CR>"
" Tabで選択
imap <expr><TAB> pumvisible() ? "\<C-n>" :
              \ (neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" : "\<TAB>")
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
              \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
" 補完のShift-Tab
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"
" Escで補完ポップアップを閉じて標準モード
inoremap <expr><Esc> pumvisible() ? deoplete#close_popup()."<Esc>" : "<Esc>"

" Order of sources
call deoplete#custom#source('neosnippet', 'rank', 9999)
call deoplete#custom#source('LanguageClient-neovim', 'rank', 1000)
call deoplete#custom#source('buffer', 'rank', 100)
call deoplete#custom#source('deoplete-tabnine', 'rank', 10)
