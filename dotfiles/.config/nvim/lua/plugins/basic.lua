return {
  ------------------------------------------------------------------------------
  ------------------------------ Extra Functions -------------------------------
  ------------------------------------------------------------------------------
  { 'dstein64/vim-startuptime' }, -- :StartupTime
  { 'nvim-lua/plenary.nvim' },  -- lua utility
  { 'h-hg/fcitx.nvim' },        -- fcitx自動化
  {
    'embear/vim-localvimrc',    -- lvimrc
    init = function()
      vim.g.localvimrc_ask = 0
      vim.g.localvimrc_sandbox = 0
    end
  },
  {
    'mbbill/undotree', -- undo可視化
    config = function()
      vim.keymap.set('n', 'U', ':<C-u>UndotreeToggle<CR>', { remap = true })
    end
  },
  {
    'simeji/winresizer', -- ウィンドウリサイズ
    init = function()
      vim.g.winresizer_start_key = '<C-w>-'
      vim.g.winresizer_vert_resize = 4
      vim.g.winresizer_horiz_resize = 1
    end
  },
  {
    'vim-scripts/YankRing.vim', -- ヤンク履歴
    init = function()
      vim.g.yankring_history_dir = vim.fn.stdpath("data")
      vim.g.yankring_clipboard_monitor = 0
    end
  },
  {
    'windwp/nvim-autopairs', -- カッコ自動入力
    config = function()
      require("nvim-autopairs").setup {}
    end
  },
  {
    'skywind3000/asyncrun.vim', -- 非同期実行
    config = function()
      vim.cmd('autocmd User AsyncRunStart call asyncrun#quickfix_toggle(2, 1)')
    end
  },
  {
    'akinsho/toggleterm.nvim', -- Terminal
    config = function()
      require("toggleterm").setup({ auto_scroll = false })
      vim.keymap.set('n', '<F4>', ':ToggleTerm direction=float<CR>')
      vim.keymap.set('n', '<C-t>', ':ToggleTerm direction=float<CR>')
      vim.keymap.set('i', '<F4>', '<ESC>:ToggleTermToggleAll<CR>')
      vim.keymap.set('i', '<C-t>', '<ESC>:ToggleTermToggleAll<CR>')
      vim.keymap.set('t', '<F4>', '<C-\\><C-n>:ToggleTerm<CR>')
      vim.keymap.set('t', '<C-t>', '<C-\\><C-n>:ToggleTerm<CR>')
    end
  },
  {
    'nvim-telescope/telescope.nvim', -- Telescope
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('telescope').setup {}
      vim.keymap.set('n', '<F3>', '<cmd>Telescope live_grep<cr>')
      vim.keymap.set('n', '<C-F3>', '<cmd>Telescope find_files<cr>')
    end
  },
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
  },

  ------------------------------------------------------------------------------
  ------------------------------------ Git -------------------------------------
  ------------------------------------------------------------------------------
  {
    'lewis6991/gitsigns.nvim', -- Git差分ガイド
    config = function()
      require('gitsigns').setup {
        signs = {
          add          = { text = '++' },
          change       = { text = '-+' },
          delete       = { text = '__' },
          topdelete    = { text = '__' },
          changedelete = { text = '+_' },
        },
        max_file_length = 2000,
      }
    end
  },
  { 'tpope/vim-fugitive' }, -- Git補助

  ------------------------------------------------------------------------------
  ----------------------------- Indent Assistance ------------------------------
  ------------------------------------------------------------------------------
  { 'tpope/vim-sleuth' },             -- インデント自動検出
  {
    'nathanaelkane/vim-indent-guides', -- インデント明示化
    init = function()
      vim.g.indent_guides_enable_on_vim_startup = 1
      vim.g.indent_guides_exclude_filetypes = { 'help', 'fern' }
    end
  },

  ------------------------------------------------------------------------------
  ----------------------------- Comment Assistance -----------------------------
  ------------------------------------------------------------------------------
  {
    'tyru/caw.vim', -- コメントアウト補助
    config = function()
      vim.keymap.set('n', '\\c', '<Plug>(caw:zeropos:toggle)')
      vim.keymap.set('v', '\\c', '<Plug>(caw:zeropos:toggle)')
      vim.keymap.set('n', '\\C', '<Plug>(caw:zeropos:uncomment)')
      vim.keymap.set('v', '\\C', '<Plug>(caw:zeropos:uncomment)')
      vim.keymap.set('n', 'c', '<Plug>(caw:zeropos:toggle)')
      vim.keymap.set('v', 'c', '<Plug>(caw:zeropos:toggle)')
      vim.cmd("autocmd FileType glsl let b:caw_oneline_comment = '//'")
      vim.cmd("autocmd FileType json5 let b:caw_oneline_comment = '//'")
    end
  },
  { 'takiyu/split_commenter.nvim' }, -- 分割コメント補助 (TODO)

  ------------------------------------------------------------------------------
  ----------------------------- Cursor Assistance ------------------------------
  ------------------------------------------------------------------------------
  {
    'rhysd/clever-f.vim',                               -- カーソル移動 (f)
    init = function()
      vim.g.clever_f_ignore_case = 1                    -- 大文字入力時のみ考慮
      vim.g.clever_f_smart_case = 1
      vim.g.clever_f_use_migemo = 1                     -- 日本語
      vim.g.clever_f_fix_key_direction = 1              -- 移動方向を修正
      vim.g.clever_f_chars_match_any_signs = ';'        -- 記号の代用文字
      vim.g.clever_f_not_overwrites_standard_mappings = 1 -- Enables only `f` and `F` mappings
    end,
    config = function()
      vim.keymap.set('n', 'f', '<Plug>(clever-f-f)', { remap = true })
      vim.keymap.set('x', 'f', '<Plug>(clever-f-f)', { remap = true })
      vim.keymap.set('o', 'f', '<Plug>(clever-f-f)', { remap = true })
      vim.keymap.set('n', 'F', '<Plug>(clever-f-F)', { remap = true })
      vim.keymap.set('x', 'F', '<Plug>(clever-f-F)', { remap = true })
      vim.keymap.set('o', 'F', '<Plug>(clever-f-F)', { remap = true })
    end
  },
  {
    'easymotion/vim-easymotion', -- カーソル移動 (s)
    init = function()
      vim.g.EasyMotion_smartcase = 1
      vim.g.EasyMotion_use_smartsign_jp = 1
      vim.g.EasyMotion_use_migemo = 1
      vim.g.EasyMotion_enter_jump_first = 1
    end,
    config = function()
      vim.keymap.set('n', 's', '<Plug>(easymotion-sn)', { remap = true })
      vim.keymap.set('n', 'S', '<Plug>(easymotion-prefix)', { remap = true })
    end
  },
  {
    'karb94/neoscroll.nvim', -- Smooth scroll
    config = function()
      local neoscroll = require('neoscroll')
      neoscroll.setup({
        mappings = {},  -- disable default mappings
        hide_cursor = true,
        stop_eof = true,
        respect_scrolloff = false,
        cursor_scrolls_alone = true,
      })

      -- Helper to register scroll keymaps with dynamic line evaluation
      local function map_scroll(key, lines_fn, duration)
        vim.keymap.set('n', key, function()
          neoscroll.scroll(lines_fn(),
                           { move_cursor = true, duration = duration })
        end, { silent = true })
      end

      -- Large motion (full-page)
      map_scroll('<Space>', function() return vim.fn.winheight(0) - 2 end, 300)
      map_scroll('<S-Space>', function() return -(vim.fn.winheight(0) - 2) end, 300)
      map_scroll('<PageDown>', function() return vim.fn.winheight(0) - 2 end, 300)
      map_scroll('<PageUp>', function() return -(vim.fn.winheight(0) - 2) end, 300)
      -- Small motion (mouse wheel)
      map_scroll('<S-Down>', function() return vim.wo.scroll end, 300)
      map_scroll('<S-Up>', function() return -vim.wo.scroll end, 300)
      map_scroll('<ScrollWheelDown>', function() return vim.wo.scroll / 4 end, 300)
      map_scroll('<ScrollWheelUp>', function() return -vim.wo.scroll / 4 end, 300)
    end
  },

  ------------------------------------------------------------------------------
  --------------------------------- Appearance ---------------------------------
  ------------------------------------------------------------------------------
  {
    'takiyu/tango-lx', -- Color Scheme
    config = function()
      vim.cmd('colorscheme tango_lx')
    end
  },
  { 'bronson/vim-trailing-whitespace' }, -- 行末スペース可視化
  {
    'vimtaku/hl_matchit.vim',          -- 括弧+αをハイライト
    init = function()
      vim.g.hl_matchit_enable_on_vim_startup = 1
    end,
  },
  {
    't9md/vim-quickhl', -- ハイライト
    init = function()
      vim.g.quickhl_manual_hl_priority = 0
      vim.g.quickhl_manual_colors = {
        "gui=bold ctermbg=Cyan    ctermfg=Black guibg=#8CCBEA guifg=Black",
        "gui=bold ctermbg=Green   ctermfg=Black guibg=#A4E57E guifg=Black",
        "gui=bold ctermbg=Yellow  ctermfg=Black guibg=#FFDB72 guifg=Black",
        "gui=bold ctermbg=Red     ctermfg=Black guibg=#FF7272 guifg=Black",
        "gui=bold ctermbg=Magenta ctermfg=Black guibg=#FFB3FF guifg=Black",
        "gui=bold ctermbg=Blue    ctermfg=Black guibg=#9999FF guifg=Black",
        "gui=bold ctermbg=DarkCyan    ctermfg=Black guibg=#436170 guifg=Black",
        "gui=bold ctermbg=DarkGreen   ctermfg=Black guibg=#62894b guifg=Black",
        "gui=bold ctermbg=DarkYellow  ctermfg=Black guibg=#998344 guifg=Black",
        "gui=bold ctermbg=DarkRed     ctermfg=Black guibg=#994444 guifg=Black",
        "gui=bold ctermbg=DarkMagenta ctermfg=Black guibg=#996b99 guifg=Black",
        "gui=bold ctermbg=DarkBlue    ctermfg=Black guibg=#5b5b99 guifg=Black",
      }
    end,
    config = function()
      vim.keymap.set('n', 'm', '<Plug>(quickhl-manual-this)', { remap = true })
      vim.keymap.set('v', 'm', '<Plug>(quickhl-manual-this)', { remap = true })
      vim.keymap.set('n', 'M', '<Plug>(quickhl-manual-reset)', { remap = true })
      vim.keymap.set('v', 'M', '<Plug>(quickhl-manual-reset)', { remap = true })
    end,
  },
  {
    'brenoprata10/nvim-highlight-colors',
    config = function()
      require('nvim-highlight-colors').setup {}
    end
  }

  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
}
