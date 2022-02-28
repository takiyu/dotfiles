" ==============================================================================
"                                    lspconfig
" ==============================================================================
lua << EOF
-- Mappings
local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '{', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
vim.api.nvim_set_keymap('n', '}', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
local on_attach = function(client, bufnr)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>d', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>D', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>i', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>r', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>s', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
end

-- Map buffer local keybindings when the language server attaches
local servers = { 'clangd', 'pylsp' }
for _, lsp in pairs(servers) do
    require('lspconfig')[lsp].setup {
        on_attach = on_attach,
        flags = {
            debounce_text_changes = 150,  -- default in neovim 0.7+
        }
    }
end

-- Diagnostic appearance
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
        virtual_text = false,
        signs = true,
        underline = true,
        update_in_insert = false,
    }
)
EOF

" Show diagnostic floating automatically
autocmd CursorHold * lua vim.diagnostic.open_float()
