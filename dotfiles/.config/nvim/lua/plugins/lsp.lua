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

  ------------------------------------------------------------------------------
  ------------------------------ Minuet AI Completion --------------------------
  ------------------------------------------------------------------------------
  {
    'milanglacier/minuet-ai.nvim',
    config = function()
      -- Resolve local LLM model
      local function get_local_llm_config()
        -- 1. Check for local env variable
        local host = os.getenv('LLM_API_HOST') or 'localhost:9000'
        -- 2. Determine local model
        local models_json = vim.fn.system('curl -sf http://' .. host .. '/v1/models')
        local ok, models = pcall(vim.json.decode, models_json)
        local model_id = ok and models and models.data and models.data[1] and models.data[1].id
        -- 3. Use completion API
        local endpoint = 'http://' .. host .. '/v1/completions'
        return { endpoint = endpoint, model = model_id or 'local-model' }
      end
      local llm_config = get_local_llm_config()

      -- Setup Minuet
      require('minuet').setup({
        provider = 'openai_fim_compatible',
        n_completions = 1,
        context_window = 4096,  -- Larger context window
        context_ratio = 0.75,
        before_cursor_filter_length = 15,
        after_cursor_filter_length = 100,
        request_timeout = 5,
        provider_options = {
          openai_fim_compatible = {
            name = 'LocalLLM',
            end_point = llm_config.endpoint,
            model = llm_config.model,
            api_key = 'TERM',
            stream = true,
            template = {
              suffix = false,  -- vLLM does not support `suffix` on /v1/completions
              prompt = function(context_before_cursor, _, _)
                local language = require('minuet.utils').add_language_comment()
                local tab = require('minuet.utils').add_tab_comment()
                context_before_cursor = language .. '\n' .. tab .. '\n' .. context_before_cursor
                return context_before_cursor
              end,
            },
            optional = {
              max_tokens = 64,
              temperature = 0.8,
              top_p = 0.9,
              stop = {},
            },
          },
        },
        throttle = 300,  -- Reduced for faster auto-completion response.
        debounce = 150,
      })
    end,
  },

  {
    'hrsh7th/nvim-cmp',
    dependency = { 'cmp-nvim-lsp', 'vim-vsnip', 'cmp-vsnip', 'cmp-buffer',
      'cmp-path', 'cmp-cmdline', 'cmp-emoji', 'cmp-calc',
      'cmp-look', 'lspkind.nvim', 'minuet-ai.nvim' },
    config = function()
      local cmp = require('cmp')
      local compare = require('cmp.config.compare')

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
        -- Allow extra time for local LLM responses
        performance = {
          fetching_timeout = 2000,
        },
        snippet = {
          expand = function(args)
            vim.fn['vsnip#anonymous'](args.body)
          end,
        },
        sources = cmp.config.sources({
          -- Source group 1
          { name = 'minuet', priority = 100 },
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
              -- Trigger minuet-only completion manually
              return cmp.complete({
                config = {
                  sources = { { name = 'minuet' } }
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
              -- Assign a distinctive icon to minuet suggestions
              if entry.source.name == 'minuet' then
                vim_item.kind = '🎁 Minuet'
              end
              return vim_item
            end
          })
        },
        sorting = {
          priority_weight = 2,
          comparators = {
            compare.offset,
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
