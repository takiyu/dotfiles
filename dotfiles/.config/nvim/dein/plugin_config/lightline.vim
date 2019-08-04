" ==============================================================================
"                                  lightline
" ==============================================================================
let g:lightline = {
            \ 'colorscheme': 'Tomorrow_Night_Bright',
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ],
            \             [ 'readonly', 'filename', 'modified' ] ],
            \   'right': [ [ 'percentlineinfo' ],
            \              [ 'fileinfo' ],
            \              [ 'gitstatus'],
            \              [ 'linter_checking', 'linter_errors',
            \                'linter_warnings', 'linter_ok' ], ['asyncrun']],
            \ },
            \ 'component': {
            \   'readonly': '%{&readonly?"R":"W"}',
            \   'percentlineinfo': '%3p%% %3l:%-2v',
            \   'fileinfo': '%{&fileencoding}  %{&fileformat}  %{&filetype}',
            \   'asyncrun': '%{g:asyncrun_status}',
            \ },
            \ 'component_function': {
            \   'gitstatus': 'LightlineGitStatus',
            \ },
            \ 'component_expand': {
            \   'linter_checking': 'lightline#lsc#checking',
            \   'linter_warnings': 'lightline#lsc#warnings',
            \   'linter_errors': 'lightline#lsc#errors',
            \   'linter_ok': 'lightline#lsc#ok',
            \ },
            \ 'component_type': {
            \   'linter_checking': 'left',
            \   'linter_warnings': 'warning',
            \   'linter_errors': 'error',
            \   'linter_ok': 'left',
            \ },
            \ 'separator': {'left': '', 'right': ' '},
            \ 'subseparator': {'left': '|', 'right': '|'},
            \ }

" Git状態のステータスライン表示
function! LightlineGitStatus()
    if winwidth('.') <= 60
        return ''
    endif
    let ret = []
    try
        if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
            " 変更行数表示
            if exists('*GitGutterGetHunkSummary') && get(g:, 'gitgutter_enabled', 0)
                let symbols = ['++', '-+', '--']
                let hunks = GitGutterGetHunkSummary()
                for i in range(3)
                    if hunks[i] > 0
                        call add(ret, symbols[i] . hunks[i])
                    endif
                endfor
            endif
            " Branch名
            let mark = ' '  " edit here for cool mark
            let branch = fugitive#head()
            if branch !=# ''
                call add(ret, mark.branch)
            endif
        endif
    catch
    endtry
    return join(ret, ' ')
endfunction

autocmd TextChanged * call lightline#update()
autocmd TextChangedI * call lightline#update()
