" ==============================================================================
"                                    nvim-cmp
" ==============================================================================
lua << EOF
local cmp = require('cmp')
local lspkind = require('lspkind')
local compare = require('cmp.config.compare')

-- Completion for general
cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn['vsnip#anonymous'](args.body)
        end,
    },
    sources = {
        { name = 'nvim_lsp' },
        { name = 'buffer', max_item_count = 10 },
        { name = 'path' },
        { name = 'look', max_item_count = 10,
                         option = {convert_case = true, loud = true} },
        { name = 'cmp_tabnine' },
        { name = 'emoji', insert = true },
    },
    mapping = cmp.mapping.preset.insert({
        ['<CR>'] = cmp.mapping.confirm { select = true },
        ['<tab>'] = cmp.mapping.select_next_item(),
        ['<S-tab>'] = cmp.mapping.select_prev_item(),
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-o>'] = cmp.mapping.complete(),
        ['<C-l>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                return cmp.complete_common_string()
            else
                return cmp.complete()
            end
            fallback()
        end, { 'i', 'c' }),
    }),
    formatting = {
        format = lspkind.cmp_format({
            -- Emoji
            mode = 'symbol_text',
            maxwidth = 30,
            before = function (entry, vim_item)
                -- Custom icon & text
                if entry.source.name == "cmp_tabnine" then
                    vim_item.kind = " Tabnine"
                end
                return vim_item
            end
        })
    },
    sorting = {
        priority_weight = 2,
        comparators = {
            require('cmp_tabnine.compare'),  -- Tabnine comes upper
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
        ghost_text = true,
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
        keyword_length = 2,  -- Hide for a single letter
    },
})

-- Completion for search
cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
        { name = 'buffer' }
    },
})

-- Tabnine
local tabnine = require('cmp_tabnine.config')
tabnine.setup({
    max_lines = 200,
    max_num_results = 5,
    sort = true,
    run_on_every_keystroke = false,
    snippet_placeholder = '..',
    ignored_file_types = {},
    show_prediction_strength = true
})

EOF