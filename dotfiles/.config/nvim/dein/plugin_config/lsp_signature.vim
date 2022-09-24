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
})

EOF
