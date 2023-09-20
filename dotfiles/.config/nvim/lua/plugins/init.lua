return {
  ------------------------------------------------------------------------------
  ------------------------------ Extra Functions -------------------------------
  ------------------------------------------------------------------------------
  {'dstein64/vim-startuptime'},   -- :StartupTime
  {'h-hg/fcitx.nvim'},            -- Auto Fcitx
  {'embear/vim-localvimrc',       -- lvimrc
   init = function()
    vim.g.localvimrc_ask = 0
    vim.g.localvimrc_sandbox = 0
   end
  },
  {'mbbill/undotree',             -- undo可視化
   config = function()
    vim.keymap.set('n', 'U', ':<C-u>UndotreeToggle<CR>', {remap = true})
   end
  },
  {'simeji/winresizer',           -- ウィンドウリサイズ
   init = function()
    vim.g.winresizer_start_key = '<C-w>-'
    vim.g.winresizer_vert_resize = 4
    vim.g.winresizer_horiz_resize = 1
   end
  },
  {'vim-scripts/YankRing.vim',    -- ヤンク履歴
   init = function()
    vim.g.yankring_history_dir = vim.fn.stdpath("data")
    vim.g.yankring_clipboard_monitor = 0
   end
  },
  {'windwp/nvim-autopairs',      -- カッコ自動入力
   config = function()
    require("nvim-autopairs").setup {}
   end
  },
  {'skywind3000/asyncrun.vim',   -- 非同期実行
   config = function()
    vim.cmd('autocmd User AsyncRunStart call asyncrun#quickfix_toggle(2, 1)')
   end
  },
  {'akinsho/toggleterm.nvim',    -- Terminal
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
  {'nvim-lua/plenary.nvim'},    -- Telescope
  {'nvim-telescope/telescope.nvim',
   dependency = {'nvim-lua/plenary.nvim'},
   config = function()
    require('telescope').setup{}
    vim.keymap.set('n', '<F3>', '<cmd>Telescope live_grep<cr>')
    vim.keymap.set('n', '<C-F3>', '<cmd>Telescope find_files<cr>')
   end
  },
  {'nvim-telescope/telescope-fzf-native.nvim',
   dependency = {'nvim-telescope/telescope.nvim'},
  },

  ------------------------------------------------------------------------------
  ------------------------------------ Git -------------------------------------
  ------------------------------------------------------------------------------
  {'airblade/vim-gitgutter',   -- Git差分ガイド
   config = function()
    vim.g.gitgutter_max_signs = 200
    vim.g.gitgutter_sign_added = '++'
    vim.g.gitgutter_sign_modified = '-+'
    vim.g.gitgutter_sign_removed = '__'
    vim.g.gitgutter_sign_modified_removed = '+_'
    vim.g.gitgutter_async = 1
   end
 },
 {'tpope/vim-fugitive'},       -- Git補助

  ------------------------------------------------------------------------------
  ----------------------------- Indent Assistance ------------------------------
  ------------------------------------------------------------------------------
  {'tpope/vim-sleuth'},                -- インデント自動検出
  {'nathanaelkane/vim-indent-guides',  -- インデント明示化
   init = function()
    vim.g.indent_guides_enable_on_vim_startup = 1
    vim.g.indent_guides_exclude_filetypes = {'help', 'fern'}
   end
  },

  ------------------------------------------------------------------------------
  ----------------------------- Comment Assistance -----------------------------
  ------------------------------------------------------------------------------
  {'tyru/caw.vim',                     -- コメントアウト補助
   config = function()
    vim.keymap.set('n', '\\c', '<Plug>(caw:zeropos:toggle)')
    vim.keymap.set('v', '\\c', '<Plug>(caw:zeropos:toggle)')
    vim.keymap.set('n', '\\C', '<Plug>(caw:zeropos:uncomment)')
    vim.keymap.set('v', '\\C', '<Plug>(caw:zeropos:uncomment)')
    vim.cmd("autocmd FileType glsl let b:caw_oneline_comment = '//'")
    vim.cmd("autocmd FileType json5 let b:caw_oneline_comment = '//'")
   end
 },
 {'takiyu/split_commenter.nvim'},      -- 分割コメント補助 (TODO)

  ------------------------------------------------------------------------------
  ----------------------------- Cursor Assistance ------------------------------
  ------------------------------------------------------------------------------
  {'rhysd/clever-f.vim',               -- カーソル移動 (f)
   init = function()
    vim.g.clever_f_ignore_case = 1                       -- 大文字入力時のみ考慮
    vim.g.clever_f_smart_case = 1
    vim.g.clever_f_use_migemo = 1                        -- 日本語
    vim.g.clever_f_fix_key_direction = 1                 -- 移動方向を修正
    vim.g.clever_f_chars_match_any_signs = ';'           -- 記号の代用文字
    vim.g.clever_f_not_overwrites_standard_mappings = 1  -- Enables only `f` and `F` mappings
   end,
   config = function()
    vim.keymap.set('n', 'f', '<Plug>(clever-f-f)',  {remap = true})
    vim.keymap.set('x', 'f', '<Plug>(clever-f-f)',  {remap = true})
    vim.keymap.set('o', 'f', '<Plug>(clever-f-f)',  {remap = true})
    vim.keymap.set('n', 'F', '<Plug>(clever-f-F)',  {remap = true})
    vim.keymap.set('x', 'F', '<Plug>(clever-f-F)',  {remap = true})
    vim.keymap.set('o', 'F', '<Plug>(clever-f-F)',  {remap = true})
   end
  },
  {'easymotion/vim-easymotion',        -- カーソル移動 (s)
   init = function()
    vim.g.EasyMotion_smartcase = 1
    vim.g.EasyMotion_use_smartsign_jp = 1
    vim.g.EasyMotion_use_migemo = 1
    vim.g.EasyMotion_enter_jump_first = 1
   end,
   config = function()
    vim.keymap.set('n', 's', '<Plug>(easymotion-sn)',  {remap = true})
    vim.keymap.set('n', 'S', '<Plug>(easymotion-prefix)',  {remap = true})
   end
 },
 {'yuttie/comfortable-motion.vim',     -- Smooth scroll
   init = function()
    vim.g.comfortable_motion_no_default_key_mappings = 1
    vim.g.comfortable_motion_friction = 120.0
    vim.g.comfortable_motion_air_drag = 3.0
   end,
   config = function()
    -- Large motion
    vim.api.nvim_set_keymap('n', '<Space>', ':call comfortable_motion#flick(120)<CR>',  {silent = true})
    vim.api.nvim_set_keymap('n', '<S-Space>', ':call comfortable_motion#flick(-120)<CR>',  {silent = true})
    vim.api.nvim_set_keymap('n', '<PageDown>', ':call comfortable_motion#flick(120)<CR>',  {silent = true})
    vim.api.nvim_set_keymap('n', '<PageUp>', ':call comfortable_motion#flick(-120)<CR>',  {silent = true})
    vim.api.nvim_set_keymap('n', '<S-Down>', ':call comfortable_motion#flick(120)<CR>',  {silent = true})
    vim.api.nvim_set_keymap('n', '<S-Up>', ':call comfortable_motion#flick(-120)<CR>',  {silent = true})
    -- Small motion
    vim.api.nvim_set_keymap('n', '<ScrollWheelDown>', ':call comfortable_motion#flick(40)<CR>',  {silent = true})
    vim.api.nvim_set_keymap('n', '<ScrollWheelUp>', ':call comfortable_motion#flick(-40)<CR>',  {silent = true})
   end,
  },

  ------------------------------------------------------------------------------
  --------------------------------- Appearance ---------------------------------
  ------------------------------------------------------------------------------
  {'takiyu/tango-lx',                  -- Color Scheme
   config = function()
    vim.cmd('colorscheme tango_lx')
   end
  },
  {'bronson/vim-trailing-whitespace'},  -- 行末スペース可視化
  {'vimtaku/hl_matchit.vim',            -- 括弧+αをハイライト
   init = function()
    vim.g.hl_matchit_enable_on_vim_startup = 1
   end,
  },
  {'t9md/vim-quickhl',                  -- ハイライト
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
    vim.keymap.set('n', 'm', '<Plug>(quickhl-manual-this)',  {remap = true})
    vim.keymap.set('v', 'm', '<Plug>(quickhl-manual-this)',  {remap = true})
    vim.keymap.set('n', 'M', '<Plug>(quickhl-manual-reset)',  {remap = true})
    vim.keymap.set('v', 'M', '<Plug>(quickhl-manual-reset)',  {remap = true})
   end,
  },

  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
}
