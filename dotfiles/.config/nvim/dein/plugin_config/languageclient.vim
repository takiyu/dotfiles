" ==============================================================================
"                             LanguageClient-neovim
" ==============================================================================
let g:LanguageClient_autoStart = 1
let g:LanguageClient_loadSettings = 1
let g:LanguageClient_hasSnippetSupport = 0          " No snippets
let g:LanguageClient_useVirtualText = 1             " Use inline texts
let g:LanguageClient_diagnosticsList = "Disabled"   " Escape location list break
let g:LanguageClient_selectionUI = "Quickfix"       " Disable fzf selection
let g:LanguageClient_fzfContextMenu = 0             " Disable fzf menu
let g:LanguageClient_rootMarkers = ['build', '.git', 'build_*']

" Set LSP server
if !executable('cquery')
    echoerr 'cquery is not installed'
endif
let g:LanguageClient_serverCommands = {
    \ 'c': ['cquery',
    \       '--log-file=/tmp/cquery/cq.log',
    \       '--init={"cacheDirectory":"/tmp/cquery/", ' .
    \       '        "completion": {"filterAndSort": false}}'],
    \ 'cpp': ['cquery',
    \         '--log-file=/tmp/cquery/cq.log',
    \         '--init={"cacheDirectory":"/tmp/cquery/", ' .
    \         '        "completion": {"filterAndSort": false}}'],
\ }

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

" Key mappings
nnoremap <F8> :call LanguageClient_contextMenu()<CR>
nnoremap K :call LanguageClient#textDocument_hover()<CR>
nnoremap <leader>d :call LanguageClient#textDocument_definition()<CR>
nnoremap <leader>t :call LanguageClient#textDocument_typeDefinition()<CR>
nnoremap <leader>i :call LanguageClient#textDocument_implementation()<CR>
nnoremap <leader>r :call LanguageClient#textDocument_references()<CR>
nnoremap <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <F9> :call LanguageClient#textDocument_formatting()<CR>
