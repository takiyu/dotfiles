" ==============================================================================
"                             mason / mason-lspconfig.nvim
" ==============================================================================
lua << EOF
require('mason').setup()
require('mason-lspconfig').setup_handlers({ function(server)
   -- Setup `cmp_nvim_lsp`
   local opt = {
      capabilities = require('cmp_nvim_lsp').update_capabilities(
         vim.lsp.protocol.make_client_capabilities()
      )
   }
   -- Setup `lspconfig`
   require('lspconfig')[server].setup(opt)
end })

EOF
