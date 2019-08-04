" ==============================================================================
"                             LanguageClient-neovim
" ==============================================================================
let g:LanguageClient_autoStart = 1
let g:LanguageClient_loadSettings = 1
let g:LanguageClient_serverCommands = {
    \ 'c': ['clangd', '-compile-commands-dir=' . getcwd() . '/build'],
    \ 'cpp': ['clangd', '-compile-commands-dir=' . getcwd() . '/build'],
\ }
let g:LanguageClient_rootMarkers = ['build', '.git']

" Enable syntax check
let g:LanguageClient_diagnosticsEnable = 1
let g:LanguageClient_diagnosticsDisplay = {
    \ 1: {
    \     "name": "Error",
    \     "signText": ">>",
    \     "signTexthl": "Error",
    \     "virtualTexthl": "Error",
    \ },
    \ 2: {
    \     "name": "Warning",
    \     "signText": ">>",
    \     "signTexthl": "Todo",
    \     "virtualTexthl": "Todo",
    \ },
    \ 3: {
    \     "name": "Information",
    \     "signText": "i",
    \     "signTexthl": "Keyword",
    \     "virtualTexthl": "Keyword",
    \ },
    \ 4: {
    \     "name": "Hint",
    \     "signText": "?",
    \     "signTexthl": "Keyword",
    \     "virtualTexthl": "Keyword",
    \ },
\ }
let g:LanguageClient_useVirtualText = 0  " Hide inline texts

" Key mappings
nnoremap <F8> :call LanguageClient_contextMenu()<CR>
nnoremap K :call LanguageClient#textDocument_hover()<CR>
nnoremap <leader>d :call LanguageClient#textDocument_definition()<CR>
nnoremap <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <F9> :call LanguageClient#textDocument_formatting()<CR>
