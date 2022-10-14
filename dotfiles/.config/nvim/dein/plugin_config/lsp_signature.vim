" ==============================================================================
"                                   lsp_signature
" ==============================================================================
lua << EOF
require 'lsp_signature'.setup({
   bind = true,
   handler_opts = {
      border = 'rounded'
   },
   hint_prefix = '🐬 ',
   toggle_key = '<F10>',
   floating_window_off_y = -7,
   floating_window_off_x = 20,
})

EOF
