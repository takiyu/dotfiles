" ==============================================================================
"                                    telescope
" ==============================================================================
lua << EOF
require('telescope').setup{
}

EOF

nnoremap <F3> <cmd>Telescope live_grep<cr>
nnoremap <C-F3> <cmd>Telescope find_files<cr>
