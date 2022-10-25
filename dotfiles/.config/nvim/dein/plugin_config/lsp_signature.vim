" ==============================================================================
"                                   lsp_signature
" ==============================================================================
lua << EOF
require 'lsp_signature'.setup({
   bind = true,
   handler_opts = {
      border = 'rounded'
   },
   doc_lines = 3,
   hint_prefix = '🐬 ',
   toggle_key = '<F10>',
   fix_pos = true,
   floating_window_above_cur_line = true,
   floating_window_off_y = 1000,
   floating_window_off_x = 1000,
})

EOF
