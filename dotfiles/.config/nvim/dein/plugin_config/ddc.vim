" ==============================================================================
"                                    ddc
" ==============================================================================
" Sources
call ddc#custom#patch_global('sources',
    \                        ['file', 'neosnippet', 'nvim-lsp', 'around'])
" Global設定
call ddc#custom#patch_global('sourceOptions', {
    \     '_': {
    \         'matchers': ['matcher_head'],
    \         'sorters': ['sorter_rank']
    \     },
    \     'around': {
    \         'mark': 'around',
    \         'maxSize': 500,
    \     },
    \     'file': {
    \       'mark': 'file',
    \       'isVolatile': v:true,
    \       'forceCompletionPattern': '\S/\S*',
    \     },
    \     'neosnippet': {
    \         'mark': 'ns',
    \         'dup': v:true,
    \     },
    \     'nvim-lsp': {
    \         'mark': 'lsp',
    \         'forceCompletionPattern': '\.\w*|:\w*|->\w*',
    \     },
    \ })

" File for Windows files
call ddc#custom#patch_filetype(['ps1', 'dosbatch', 'autohotkey', 'registry'], {
   \ 'sourceOptions': {
   \     'file': {
   \         'forceCompletionPattern': '\S\\\S*',
   \     },
   \ },
   \ 'sourceParams': {
   \     'file': {
   \         'mode': 'win32',
   \     },
   \ }})

" ddc有効化
call ddc#enable()


" Enterで次へジャンプ、または補完を決定
imap <expr><CR> neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" :
              \ neosnippet#expandable() ? "\<Plug>(neosnippet_expand)" :
              \ ddc#map#pum_visible() ? "<ESC>" : "\<CR>"
" Ctrl+Enterで次へジャンプ
imap <expr><C-CR> neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" : "\<CR>"
" Tabで選択
imap <expr><TAB> ddc#map#pum_visible() ? "\<C-n>" :
              \ (neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" : "\<TAB>")
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
              \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
" Shift-Tabで逆向き選択
inoremap <expr><S-TAB>  ddc#map#pum_visible() ? "\<C-p>" : "\<S-TAB>"
" Ctrl+Oで手動補完 (file補完を除外)
imap <expr><C-o> ddc#map#manual_complete(['nvim-lsp', 'neosnippet', 'around'])