" ==============================================================================
"                                  quickhl
" ==============================================================================
let g:quickhl_manual_hl_priority = 0       " プライオリティの設定
" let g:quickhl_cword_enable_at_startup = 1 " カーソル下の単語をハイライト
" 色指定(同時に個数も指定)
let g:quickhl_manual_colors = [
    \ "gui=bold ctermbg=Cyan    ctermfg=Black guibg=#8CCBEA guifg=Black",
    \ "gui=bold ctermbg=Green   ctermfg=Black guibg=#A4E57E guifg=Black",
    \ "gui=bold ctermbg=Yellow  ctermfg=Black guibg=#FFDB72 guifg=Black",
    \ "gui=bold ctermbg=Red     ctermfg=Black guibg=#FF7272 guifg=Black",
    \ "gui=bold ctermbg=Magenta ctermfg=Black guibg=#FFB3FF guifg=Black",
    \ "gui=bold ctermbg=Blue    ctermfg=Black guibg=#9999FF guifg=Black",
    \ "gui=bold ctermbg=DarkCyan    ctermfg=Black guibg=#436170 guifg=Black",
    \ "gui=bold ctermbg=DarkGreen   ctermfg=Black guibg=#62894b guifg=Black",
    \ "gui=bold ctermbg=DarkYellow  ctermfg=Black guibg=#998344 guifg=Black",
    \ "gui=bold ctermbg=DarkRed     ctermfg=Black guibg=#994444 guifg=Black",
    \ "gui=bold ctermbg=DarkMagenta ctermfg=Black guibg=#996b99 guifg=Black",
    \ "gui=bold ctermbg=DarkBlue    ctermfg=Black guibg=#5b5b99 guifg=Black",
    \ ]
" ハイライトショートカット
nmap m <Plug>(quickhl-manual-this)
vmap m <Plug>(quickhl-manual-this)
" ハイライトを削除
nmap M <Plug>(quickhl-manual-reset)
vmap M <Plug>(quickhl-manual-reset)
