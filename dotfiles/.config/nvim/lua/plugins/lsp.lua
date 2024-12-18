return {
  ------------------------------------------------------------------------------
  ------------------------------------- LSP ------------------------------------
  ------------------------------------------------------------------------------
  {'neovim/nvim-lspconfig',
   init = function()
    -- Diagnostic appearance
    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
        vim.lsp.diagnostic.on_publish_diagnostics, {
            virtual_text = false,
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
      pattern = {'*'},
      callback = function() vim.diagnostic.open_float() end
    })
   end
  },

  ------------------------------------------------------------------------------
  ---------------------------------- LSP Icon ----------------------------------
  ------------------------------------------------------------------------------
  {'onsails/lspkind.nvim'},

  ------------------------------------------------------------------------------
  -------------------------------- LSP Signature -------------------------------
  ------------------------------------------------------------------------------
  {'ray-x/lsp_signature.nvim',
   config = function()
    require 'lsp_signature'.setup({
      bind = true,
      handler_opts = {
         border = 'rounded'
      },
      doc_lines = 3,
      hint_prefix = '🐬 ',
      -- toggle_key = '<F10>',
      fix_pos = true,
      floating_window_above_cur_line = true,
      floating_window_off_y = 1000,
      floating_window_off_x = 1000,
    })
   end
  },

  ------------------------------------------------------------------------------
  ------------------------------------ Mason -----------------------------------
  ------------------------------------------------------------------------------
  {'williamboman/mason.nvim'},               -- LSP installer
  {'williamboman/mason-lspconfig.nvim',
   dependency = {'williamboman/mason.nvim', 'neovim/nvim-lspconfig'},
   config = function()
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

      -- Arguments for PyRight
      if server == 'pyright' then
        opt.settings = {
          python = {
            venvPath = ".",
            pythonPath = "./.venv/bin/python",
            analysis = {
              extraPaths = {"."}
            }
          }
        }
      end

      -- Pass to `lspconfig` setup
      require('lspconfig')[server].setup(opt)
    end })
   end
  },

  ------------------------------------------------------------------------------
  --------------------------------- Completion ---------------------------------
  ------------------------------------------------------------------------------
  {'hrsh7th/cmp-nvim-lsp'},
  {'hrsh7th/vim-vsnip'},
  {'hrsh7th/cmp-vsnip'},
  {'hrsh7th/cmp-buffer'},
  {'hrsh7th/cmp-path'},
  {'hrsh7th/cmp-cmdline'},
  {'hrsh7th/cmp-emoji'},
  {'hrsh7th/cmp-calc'},
  {'octaltree/cmp-look'},
  {'takiyu/cmp-tabby'},
  {'zbirenbaum/copilot.lua'},
  {'zbirenbaum/copilot-cmp',
   dependency = {'copilot.lua'},
  },
  {'hrsh7th/nvim-cmp',
   dependency = {'cmp-nvim-lsp', 'vim-vsnip', 'cmp-vsnip', 'cmp-buffer',
                 'cmp-path', 'cmp-cmdline', 'cmp-emoji', 'cmp-calc',
                 'cmp-look', 'cmp-tabby', 'copilot-cmp', 'lspkind.nvim'},
   config = function()
    local cmp = require('cmp')
    local compare = require('cmp.config.compare')

    -- Tabby
    if false then
      local tabby = require('cmp_tabby.config')
      tabby:setup({
        host = 'http://localhost:8080',
        max_lines = 200,
      })
    end

    -- Copilot
    require('copilot').setup({
      suggestion = {enabled = false},
      panel = {enabled = false},
      copilot_node_command = 'node'
    })
    require('copilot_cmp').setup()

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
          { name = 'calc'},
          { name = 'vsnip'},
          { name = 'path' },
          { name = 'emoji', insert = true },
        }, {
          -- Source group 2
          { name = 'nvim_lsp' },
          { name = 'buffer', max_item_count = 10 },
          { name = 'look', max_item_count = 10,
                           option = {convert_case = true, loud = true} },
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
              config = { sources = {
                { name = 'copilot_cmp' },
              }}
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
          maxwidth = 30,
          -- Custom Icons
          before = function (entry, vim_item)
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
          compare.offset,
          compare.exact,
          compare.score,
          compare.recently_used,
          compare.kind,
          compare.sort_text,
          compare.length,
          compare.order,
        },
      },
      experimental = {
        ghost_text = { hl_group = 'GhostText' },  -- Defined in color scheme
      },
    })

    -- nvim-cmp: Completion for command
    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline(),
      sources = {
        { name = 'path' },
        { name = 'cmdline' },
      },
      completion = {
        keyword_length = 2,  -- Hide for a single letter
      },
    })

    -- nvim-cmp: Completion for search
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
