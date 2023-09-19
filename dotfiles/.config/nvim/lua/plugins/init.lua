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
    vim.api.nvim_set_keymap('n', 'U', ':<C-u>UndotreeToggle<CR>', {})
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
    vim.api.nvim_set_keymap('n', '<F4>', ':ToggleTerm direction=float<CR>',  {noremap = true})
    vim.api.nvim_set_keymap('n', '<C-t>', ':ToggleTerm direction=float<CR>',  {noremap = true})
    vim.api.nvim_set_keymap('i', '<F4>', '<ESC>:ToggleTermToggleAll<CR>',  {noremap = true})
    vim.api.nvim_set_keymap('i', '<C-t>', '<ESC>:ToggleTermToggleAll<CR>',  {noremap = true})
    vim.api.nvim_set_keymap('t', '<F4>', '<C-\\><C-n>:ToggleTerm<CR>',  {noremap = true})
    vim.api.nvim_set_keymap('t', '<C-t>', '<C-\\><C-n>:ToggleTerm<CR>',  {noremap = true})
   end
  },
  {'nvim-lua/plenary.nvim'},    -- Telescope
  {'nvim-telescope/telescope.nvim',
   dependency = {'nvim-lua/plenary.nvim'},
   config = function()
    require('telescope').setup{}
    vim.api.nvim_set_keymap('n', '<F3>', '<cmd>Telescope live_grep<cr>', {noremap = true})
    vim.api.nvim_set_keymap('n', '<C-F3>', '<cmd>Telescope find_files<cr>', {noremap = true})
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
    vim.api.nvim_set_keymap('n', '\\c', '<Plug>(caw:zeropos:toggle)',  {noremap = true})
    vim.api.nvim_set_keymap('v', '\\c', '<Plug>(caw:zeropos:toggle)',  {noremap = true})
    vim.api.nvim_set_keymap('n', '\\C', '<Plug>(caw:zeropos:uncomment)',  {noremap = true})
    vim.api.nvim_set_keymap('v', '\\C', '<Plug>(caw:zeropos:uncomment)',  {noremap = true})
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
    vim.api.nvim_set_keymap('n', 'f', '<Plug>(clever-f-f)',  {})
    vim.api.nvim_set_keymap('x', 'f', '<Plug>(clever-f-f)',  {})
    vim.api.nvim_set_keymap('o', 'f', '<Plug>(clever-f-f)',  {})
    vim.api.nvim_set_keymap('n', 'F', '<Plug>(clever-f-F)',  {})
    vim.api.nvim_set_keymap('x', 'F', '<Plug>(clever-f-F)',  {})
    vim.api.nvim_set_keymap('o', 'F', '<Plug>(clever-f-F)',  {})
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
    vim.api.nvim_set_keymap('n', 's', '<Plug>(easymotion-sn)',  {})
    vim.api.nvim_set_keymap('n', 'S', '<Plug>(easymotion-prefix)',  {})
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
  {'takiyu/tango-lx'},                  -- Color Scheme
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
    vim.api.nvim_set_keymap('n', 'm', '<Plug>(quickhl-manual-this)',  {})
    vim.api.nvim_set_keymap('v', 'm', '<Plug>(quickhl-manual-this)',  {})
    vim.api.nvim_set_keymap('n', 'M', '<Plug>(quickhl-manual-reset)',  {})
    vim.api.nvim_set_keymap('v', 'M', '<Plug>(quickhl-manual-reset)',  {})
   end,
  },

  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
}
