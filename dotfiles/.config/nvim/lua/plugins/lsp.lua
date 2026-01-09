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

      -- Diagnostic appearance
      vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
        vim.lsp.diagnostic.on_publish_diagnostics, {
          virtual_text = false, -- Hide virtual text
          signs = true,
          underline = true,
          update_in_insert = false,
        }
      )
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
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = '🔥',
            [vim.diagnostic.severity.WARN] = '🚧',
            [vim.diagnostic.severity.HINT] = '🐬',
            [vim.diagnostic.severity.INFO] = '🖊️',
          },
        },
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
    dependency = { 'williamboman/mason.nvim', 'neovim/nvim-lspconfig' },
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
  --------------------------------- Completion ---------------------------------
  ------------------------------------------------------------------------------
  { 'hrsh7th/cmp-nvim-lsp' },
  { 'hrsh7th/vim-vsnip' },
  { 'hrsh7th/cmp-vsnip' },
  { 'hrsh7th/cmp-buffer' },
  { 'hrsh7th/cmp-path' },
  { 'hrsh7th/cmp-cmdline' },
  { 'hrsh7th/cmp-emoji' },
  { 'hrsh7th/cmp-calc' },
  { 'octaltree/cmp-look' },
  -- { 'takiyu/cmp-tabby' },
  {
    'takiyu/copilot.lua', -- alt to 'zbirenbaum/copilot.lua'
    branch = 'takiyu'
  },
  {
    'litoj/cmp-copilot', -- alt to 'zbirenbaum/copilot-cmp'
    dependency = { 'copilot.lua' },
  },
  {
    'hrsh7th/nvim-cmp',
    dependency = { 'cmp-nvim-lsp', 'vim-vsnip', 'cmp-vsnip', 'cmp-buffer',
      'cmp-path', 'cmp-cmdline', 'cmp-emoji', 'cmp-calc',
      'cmp-look', 'cmp-tabby', 'cmp-copilot', 'lspkind.nvim' },
    config = function()
      local cmp = require('cmp')
      local compare = require('cmp.config.compare')

      -- Copilot
      require('copilot').setup({
        suggestion = { enabled = false },
        panel = {
          layout = {
            position = 'float',
            ratio = 0.5,
          }
        },
        copilot_node_command = 'node'
      })
      require('cmp_copilot').setup()
      -- Copilot keymap
      vim.api.nvim_set_keymap('i', '<C-H>', '<ESC>:Copilot panel<CR>', { noremap = true })

      -- Tabby
      -- local tabby = require('cmp_tabby.config')
      -- tabby:setup({
      --   host = 'http://localhost:8080',
      --   max_lines = 200,
      -- })

      -- lspkind
      require('lspkind').init({
        preset = 'default',
        symbol_map = {
          Text = '📔',
          Method = '🎓',
          Function = '🎩',
          Constructor = '🔨',
          Field = '🏷️',
          Variable = '🅰️ ',
          Class = '🏫',
          Interface = '🪟 ',
          Module = '📦',
          Property = '🏷️',
          Unit = '📏',
          Value = '💰',
          Enum = '📶',
          Keyword = '🔑',
          Snippet = '🌟',
          Color = '🎨',
          File = '📄',
          Reference = '🔗',
          Folder = '📂',
          EnumMember = '📶',
          Constant = '🗿',
          Struct = '🧳',
          Event = '🎉',
          Operator = '🧭',
          TypeParameter = '',
        },
      })

      -- nvim-cmp: Completion for general
      cmp.setup({
        snippet = {
          expand = function(args)
            vim.fn['vsnip#anonymous'](args.body)
          end,
        },
        sources = cmp.config.sources({
          -- Source group 1
          { name = 'copilot' },
          { name = 'calc' },
          { name = 'vsnip' },
          { name = 'path' },
          { name = 'emoji',  insert = true },
        }, {
          -- Source group 2
          { name = 'nvim_lsp' },
          { name = 'buffer',  max_item_count = 10 },
          {
            name = 'look',
            max_item_count = 10,
            option = { convert_case = true, loud = true }
          },
        }),
        mapping = cmp.mapping.preset.insert({
          ['<tab>'] = cmp.mapping.select_next_item(),
          ['<S-tab>'] = cmp.mapping.select_prev_item(),
          ['<C-n>'] = cmp.mapping.select_next_item(),
          ['<C-p>'] = cmp.mapping.select_prev_item(),
          ['<Down>'] = cmp.mapping.select_next_item(),
          ['<Up>'] = cmp.mapping.select_prev_item(),
          ['<C-o>'] = cmp.mapping.complete(),
          ['<C-l>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              -- Confirm visible completion
              return cmp.confirm({ select = true })
            else
              -- Start AUTO completion
              return cmp.complete({
                config = {
                  sources = {
                    { name = 'copilot' }
                  }
                }
              })
            end
          end, { 'i', 'c' }),
          ['<CR>'] = cmp.mapping(function(fallback)
            if cmp.get_active_entry() then
              -- Confirm with explicit selection
              return cmp.confirm({ select = true })
            end
            fallback()
          end, { 'i', 'c' }),
        }),
        formatting = {
          format = require('lspkind').cmp_format({
            mode = 'symbol_text',
            maxwidth = 50,
            -- Custom Icons
            before = function(entry, vim_item)
              if entry.source.name == 'copilot' then
                vim_item.kind = '🎁 Copilot'
              elseif entry.source.name == 'cmp_tabby' then
                vim_item.kind = '🎁 Tabby'
              end
              return vim_item
            end
          })
        },
        sorting = {
          priority_weight = 2,
          comparators = {
            require("cmp_copilot.comparators").prioritize,
            compare.offset,
            -- compare.exact,  -- Disable exact match for copilot
            compare.score,
            compare.recently_used,
            compare.kind,
            compare.sort_text,
            compare.length,
            compare.order,
          },
        },
        window = {
          completion = cmp.config.window.bordered({
            border = 'rounded',
          }),
          documentation = cmp.config.window.bordered({
            border = 'rounded',
          }),
        },
        experimental = {
          ghost_text = { hl_group = 'GhostText' }, -- Defined in color scheme
        },
      })

      -- Completion for command
      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'path' },
          { name = 'cmdline' },
        },
        completion = {
          keyword_length = 2, -- Hide for a single letter
        },
      })

      -- Completion for search
      cmp.setup.cmdline('/', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' }
        },
      })

      -- Key mappings for vim-vsnip
      -- Expand or jump
      vim.cmd("imap <expr> <CR>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<CR>'")
      vim.cmd("smap <expr> <CR>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<CR>'")
      vim.cmd("nmap <expr> <CR>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<CR>'")
      -- Jump forward or backward
      vim.cmd("imap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'")
      vim.cmd("smap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'")
      vim.cmd("nmap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'")
      vim.cmd("imap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'")
      vim.cmd("smap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'")
      vim.cmd("nmap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'")
    end
  },

  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
}
