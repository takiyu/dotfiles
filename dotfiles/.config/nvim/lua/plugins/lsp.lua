return {
  ------------------------------------------------------------------------------
  ------------------------------------- LSP ------------------------------------
  ------------------------------------------------------------------------------
  {
    'neovim/nvim-lspconfig',
    init = function()
      -- Global floating window border configuration
      local org_func = vim.lsp.util.open_floating_preview
      function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
        opts = opts or {}
        opts.border = opts.border or 'rounded'
        return org_func(contents, syntax, opts, ...)
      end

      -- Diagnostic gutter
      local signs = { Error = "🔥", Warn = "🚧", Hint = "🐬", Info = "🖊️" }
      for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
      end
    end,
    config = function()
      -- Key mappings
      local opts = { noremap = true, silent = true }
      vim.api.nvim_set_keymap('n', '{', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
      vim.api.nvim_set_keymap('n', '}', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
      vim.api.nvim_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
      vim.api.nvim_set_keymap('n', '<leader>d', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
      vim.api.nvim_set_keymap('n', '<leader>D', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
      vim.api.nvim_set_keymap('n', '<leader>i', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
      vim.api.nvim_set_keymap('n', '<leader>r', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
      vim.api.nvim_set_keymap('n', '<leader>s', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
      vim.api.nvim_set_keymap('n', '<leader>f', '<cmd>lua vim.lsp.buf.format()<CR>', opts)
      vim.api.nvim_set_keymap('n', '<leader>a', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
      -- Show diagnostic floating automatically
      vim.api.nvim_create_autocmd('CursorHold', {
        pattern = { '*' },
        callback = function() vim.diagnostic.open_float() end
      })
      -- Set diagnostic signs
      vim.diagnostic.config({
        virtual_text = false,
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = '🔥',
            [vim.diagnostic.severity.WARN] = '🚧',
            [vim.diagnostic.severity.HINT] = '🐬',
            [vim.diagnostic.severity.INFO] = '🖊️',
          },
        },
        underline = true,
        update_in_insert = false,
      })
    end
  },

  ------------------------------------------------------------------------------
  ---------------------------------- LSP Icon ----------------------------------
  ------------------------------------------------------------------------------
  { 'onsails/lspkind.nvim' },

  ------------------------------------------------------------------------------
  -------------------------------- LSP Signature -------------------------------
  ------------------------------------------------------------------------------
  {
    'ray-x/lsp_signature.nvim',
    config = function()
      require 'lsp_signature'.setup({
        bind = true,
        doc_lines = 3,
        hint_prefix = '🐬 ',
        -- toggle_key = '<F10>',
        fix_pos = true,
        floating_window_above_cur_line = true,
        floating_window_off_y = 1000,
        floating_window_off_x = 1000,
        max_width=70,
        max_height=5
      })
    end
  },

  ------------------------------------------------------------------------------
  ------------------------------------ Mason -----------------------------------
  ------------------------------------------------------------------------------
  { 'williamboman/mason.nvim' }, -- LSP installer
  {
    'williamboman/mason-lspconfig.nvim',
    dependencies = { 'williamboman/mason.nvim', 'neovim/nvim-lspconfig' },
    config = function()
      require('mason').setup()
      require('mason-lspconfig').setup({
        automatic_enable = true,
      })

      -- Setup `cmp_nvim_lsp` capabilities
      local capabilities = require('cmp_nvim_lsp').default_capabilities(
        vim.lsp.protocol.make_client_capabilities()
      )

      -- Configure Clangd
      vim.lsp.config('clangd', {
        cmd = {
          'clangd',
          '--background-index=false',
          '--cross-file-rename',
          '--header-insertion=never',
        },
        capabilities = capabilities,
      })

      -- Configure PyRight
      vim.lsp.config('pyright', {
        settings = {
          python = {
            venvPath = ".",
            venv = ".venv",
            pythonPath = "./.venv/bin/python",
            analysis = {
              extraPaths = { "." }
            }
          }
        },
        capabilities = capabilities,
      })

      -- Configure other LSP servers with default capabilities
      vim.lsp.config('*', {
        capabilities = capabilities,
      })
    end
  },

  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
}
