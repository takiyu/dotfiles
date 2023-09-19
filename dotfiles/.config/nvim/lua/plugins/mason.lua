local vimscript_code = [[
" ==============================================================================
"                             mason / mason-lspconfig.nvim
" ==============================================================================
lua << EOF
require('mason').setup()
require('mason-lspconfig').setup_handlers({ function(server)
   local opt = {}

   -- Setup `cmp_nvim_lsp`
   opt.capabilities = require('cmp_nvim_lsp').default_capabilities(
      vim.lsp.protocol.make_client_capabilities()
   )

   -- Arguments for Clangd
   if server == 'clangd' then
      opt.cmd = {
         'clangd',
         '--background-index=false',
         '--cross-file-rename',
         '--header-insertion=never',
      }
   end

   -- Pass to `lspconfig` setup
   require('lspconfig')[server].setup(opt)
end })

EOF
]]

return {}
