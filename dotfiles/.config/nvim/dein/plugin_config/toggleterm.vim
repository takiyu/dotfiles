" ==============================================================================
"                                    toggleterm
" ==============================================================================
lua << EOF
require("toggleterm").setup()

EOF

nnoremap <F4> :ToggleTerm direction=float<CR>
inoremap <F4> <ESC>:ToggleTermToggleAll<CR>
tnoremap <F4> <C-\><C-n>:ToggleTerm<CR>

nnoremap <C-t> :ToggleTerm direction=float<CR>
inoremap <C-t> <ESC>:ToggleTermToggleAll<CR>
tnoremap <C-t> <C-\><C-n>:ToggleTerm<CR>
