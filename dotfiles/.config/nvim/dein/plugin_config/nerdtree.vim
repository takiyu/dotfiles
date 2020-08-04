" ==============================================================================
"                                  nerdtree
" ==============================================================================
noremap <C-e> :NERDTreeToggle<CR>
map <expr><C-f> g:NERDTree.IsOpen() ? ":NERDTreeClose<CR>:NERDTreeFind<CR>"
                                  \ : "<C-f>"
let NERDTreeQuitOnOpen = 1           " 開いたら非表示
let NERDTreeShowBookmarks = 1
" let NERDTreeMapOpenInTab='<ENTER>' " デフォルトでタブで開く (フォルダ移動などはoを使用)
