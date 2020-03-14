" ==============================================================================
"                             LanguageClient-neovim
" ==============================================================================
let g:LanguageClient_autoStart = 1
let g:LanguageClient_loadSettings = 1
let g:LanguageClient_hasSnippetSupport = 0          " No snippets
let g:LanguageClient_useVirtualText = "No"          " Disable inline texts
let g:LanguageClient_diagnosticsList = "Disabled"   " Escape location list break
let g:LanguageClient_selectionUI = "Quickfix"       " Disable fzf selection
let g:LanguageClient_fzfContextMenu = 0             " Disable fzf menu
let g:LanguageClient_rootMarkers = ['build', '.git', 'build_*']
let g:LanguageClient_useFloatingHover = 1  " Neovim
let g:LanguageClient_usePopupHover = 1     " Vim 8.2

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
nnoremap <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <F9> :call LanguageClient#textDocument_formatting()<CR>
nnoremap <leader>k :call LanguageClient#textDocument_hover()<CR>
nnoremap <leader>e :call LanguageClient#explainErrorAtPoint()<CR>
nnoremap <leader>d :call LanguageClient#textDocument_definition()<CR>
nnoremap <leader>t :call LanguageClient#textDocument_typeDefinition()<CR>
nnoremap <leader>i :call LanguageClient#textDocument_implementation()<CR>
nnoremap <leader>r :call LanguageClient#textDocument_references()<CR>

nmap H <F8>
nmap K <leader>k
nmap E <leader>e
nmap gd <leader>d
nmap gt <leader>t
nmap gi <leader>i
nmap gr <leader>r

" Set LSP server
let g:LanguageClient_serverCommands = {}

" ------------------------------------ C++ -------------------------------------
let g:cpp_language_server_priority = ['clangd', 'ccls', 'cquery']
let g:cpp_language_server_cmds = {
    \   'clangd': ['clangd', '--compile-commands-dir=./build'],
    \   'ccls': ['ccls', '--log-file=/tmp/ccls.log',
    \                    '--init={"compilationDatabaseDirectory": "./build", ' .
    \                    '        "completion": {"filterAndSort": false}}'],
    \   'cquery': ['cquery', '--log-file=/tmp/cq.log',
    \                        '--init={"cacheDirectory":"/tmp/", ' .
    \                        '        "completion": {"filterAndSort": false}}'],
    \ }

for name in g:cpp_language_server_priority
    if executable(name)
        let s:cmd = g:cpp_language_server_cmds[name]
        let g:LanguageClient_serverCommands['c'] = s:cmd
        let g:LanguageClient_serverCommands['cpp'] = s:cmd
    endif
endfor

" ----------------------------------- Python -----------------------------------
" " Install commands
" " ```
" " pip install python-language-server
" " pip install 'python-language-server[pycodestyle]'
" " ```
if executable('pyls')
    let g:LanguageClient_serverCommands['python'] = ['pyls']
endif

" ------------------------------------ Java ------------------------------------
if executable('jdtls')
    let g:LanguageClient_serverCommands['java'] = ['jdtls']
endif
