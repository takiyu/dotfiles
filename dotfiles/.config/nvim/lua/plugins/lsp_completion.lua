return {
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
              temperature = 0.5,
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

  ------------------------------------------------------------------------------
  ------------------------------ CMP Configuration -----------------------------
  ------------------------------------------------------------------------------
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'cmp-nvim-lsp', 'vim-vsnip', 'cmp-vsnip', 'cmp-buffer', 'cmp-path',
      'cmp-cmdline', 'cmp-emoji', 'cmp-calc', 'cmp-look', 'lspkind.nvim',
      'minuet-ai.nvim'
    },
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
