" ==============================================================================
"                                  lspconfig
" ==============================================================================
lua << EOF
local lsp_installer = require("nvim-lsp-installer")

-- Register a handler
lsp_installer.on_server_ready(function(server)
    server:setup({})
end)
EOF
