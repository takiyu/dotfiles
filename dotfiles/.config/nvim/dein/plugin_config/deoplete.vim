" ==============================================================================
"                                  deoplete
" ==============================================================================
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete = 1
let g:deoplete#auto_complete_delay = 0
let g:deoplete#camel_case = 1
let g:deoplete#ignore_case = 1
let g:deoplete#smart_case = 1
let g:deoplete#refresh_always = 0
let g:deoplete#file#enable_buffer_path = 1
let g:deoplete#max_list = 500

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
