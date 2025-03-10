vim.cmd([[
" Global configuration
let g:lightline = {
    \   'enable': {
    \     'statusline': 1,
    \     'tabline': 1
    \   },
    \   'colorscheme': 'Tomorrow_Night_Bright',
    \   'component': {
    \     'readonly': '%{&readonly ? "R" : "W"}',
    \     'percentlineinfo': '[%3p%% %3l:%-2v]',
    \     'fileinfo': '%{&fileencoding}  %{&fileformat}  %{&filetype}',
    \     'asyncrun': '%{g:asyncrun_status}',
    \   },
    \   'component_function': {
    \     'gitstatus': 'LightlineGitStatus',
    \   },
    \ 'separator': { 'left': '', 'right': '' },
    \ 'subseparator': { 'left': '', 'right': '' }
    \ }

" Status line configuration
let g:lightline.active = {
    \   'left': [ [ 'mode' ],
    \             [ 'readonly', 'filename', 'modified' ] ],
    \   'right': [ [ 'percentlineinfo' ],
    \              [ 'fileinfo' ],
    \              [ 'gitstatus'],
    \              [ 'lsp_status' ],
    \              [ 'lsp_info', 'lsp_hints', 'lsp_errors', 'lsp_warnings',
    \                'lsp_ok' ],
    \              [ 'asyncrun' ] ],
    \ }

" Tab line configuration
let g:lightline.tabline = {
    \   'left': [ [ 'tabs' ] ],
    \   'right': []
    \ }

" Git状態のステータスライン表示
function! LightlineGitStatus()
    if winwidth('.') <= 60
        return ''
    endif
    let ret = []
    try
        if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler'
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
            let mark = ''  " edit here for cool mark
            let branch = fugitive#head()
            if branch !=# ''
                call add(ret, mark.'['.branch.']')
            endif
        endif
    catch
        echohl '[takiyu] Error in generating git status line string.'
    endtry
    return join(ret, ' ')
endfunction
]])

return {
    {
        'josa42/nvim-lightline-lsp',
        dependency = { 'neovim/nvim-lspconfig' },
        config = function()
            vim.cmd([[ call lightline#lsp#register() ]])
        end
    },
    {
        'itchyny/lightline.vim',
        dependency = { 'tpope/vim-fugitive', 'skywind3000/asyncrun.vim',
            'josa42/nvim-lightline-lsp' },
        config = function()
            vim.cmd([[
        autocmd TextChanged * call lightline#update()
        autocmd TextChangedI * call lightline#update()
        autocmd CursorHold * call lightline#update()
        autocmd CursorHoldI * call lightline#update()
    ]])
        end
    },
}
