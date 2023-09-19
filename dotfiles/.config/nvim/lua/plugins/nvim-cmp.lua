local vimscript_code = [[
" ==============================================================================
"                                    nvim-cmp
" ==============================================================================
lua << EOF
local cmp = require('cmp')
local compare = require('cmp.config.compare')

-- Tabnine
local has_tabnine = pcall(require, 'cmp_tabnine')
if has_tabnine then
    local tabnine = require('cmp_tabnine.config')
    tabnine:setup({
        max_lines = 200,
        max_num_results = 5,
        sort = true,
        run_on_every_keystroke = false,
        snippet_placeholder = '..',
        ignored_file_types = {},
        show_prediction_strength = true
    })
else
    print('No Tabnine')
end

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
        TypeParameter = ''
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
        { name = 'calc'},
        { name = 'vsnip'},
        { name = 'path' },
        { name = 'emoji', insert = true },
    }, {
        -- Source group 2
        { name = 'cmp_tabnine' },
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
        ['<C-o>'] = cmp.mapping.complete(),
        ['<C-l>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                -- Confirm visible completion
                return cmp.confirm({ select = true })
            else
                -- Start AUTO completion
                return cmp.complete({
                    config = { sources = { { name = 'cmp_tabnine' } } }
                })
            end
            fallback()
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
            -- Tabnine integration setting
            mode = 'symbol_text',
            maxwidth = 30,
            -- Custom Icon for Tabnine
            before = function (entry, vim_item)
                if entry.source.name == 'cmp_tabnine' then
                    vim_item.kind = '🎁 Tabnine'
                end
                return vim_item
            end
        })
    },
    sorting = {
        priority_weight = 2,
        comparators = {
            has_tabnine and require('cmp_tabnine.compare'),  -- Tabnine comes upper
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

EOF

" Key mappings for vim-vsnip
" Expand or jump
imap <expr> <CR>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<CR>'
smap <expr> <CR>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<CR>'
nmap <expr> <CR>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<CR>'
" Jump forward or backward
imap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'
smap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'
nmap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'
imap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'
smap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'
nmap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'
]]

return {}
