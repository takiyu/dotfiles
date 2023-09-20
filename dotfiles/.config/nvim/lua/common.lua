--------------------------------------------------------------------------------
-------------------------------- Basic Settings --------------------------------
--------------------------------------------------------------------------------
vim.cmd('set synmaxcol=400')                 -- ハイライトする文字数を制限する
vim.cmd('set backspace=indent,eol,start')    -- インサートモード時にバックスペースを使う
vim.cmd('set whichwrap=b,s,h,l,<,>,[,]')     -- 行頭から前行文末へ移動可能にする
-- vim.cmd('set scrolloff=999')                 -- スクロール時にカーソルを中央へ移動
vim.cmd('set scrolloff=3')                   -- スクロールを開始する行数
vim.cmd('set cindent')                       -- cオートインデント
vim.cmd('set cinoptions=g0')                 -- cppでのpublic宣言を下げる
vim.cmd('set showtabline=2')                 -- タブ(上部)を常に表示する
vim.cmd('set number')                        -- 行数を表示する
vim.cmd('set hlsearch')                      -- 検索文字列を強調
vim.cmd('set ignorecase')                    -- 大文字小文字を無視
vim.cmd('set smartcase')                     -- (ただし大文字入力時のみ考慮)
vim.cmd('set guioptions-=m')                 -- メニューバーを非表示
vim.cmd('set guioptions-=T')                 -- ツールバーを非表示
vim.cmd('set guioptions-=e')                 -- TabのGUI表示をOFF
vim.cmd('set mouse=a')                       -- マウスをすべてのモードで使用する
vim.cmd('set wildmenu')                      -- コマンドモードの補完方法
vim.cmd('set diffopt+=vertical')             -- diffは縦分割
vim.cmd('set conceallevel=0')                -- 非表示文字も表示
vim.cmd('set nobackup')                      -- バックアップhoge~を作成しない
vim.cmd('set belloff=all')                   -- ビープ音無効化
vim.cmd('set termguicolors')                 -- CLIでフルカラー
-- vim.cmd('set ttimeoutlen=50')              -- ノーマルモードに戻る時間
vim.cmd('set cursorline')                    -- カーソル行をハイライト
vim.cmd('set incsearch')                     -- 入力しながら検索
vim.cmd('set breakindent')                   -- 折り返し時にインデントを考慮
vim.cmd('set updatetime=700')                -- CursorHoldイベントの発生タイミング
if vim.loop.os_uname().sysname == 'Windows' then
  vim.cmd('set shell=cmd.exe')               -- コマンドプロンプト bashではPATH形式が異なる
  vim.cmd('set shellcmdflag=-c')
  vim.cmd('set shellxquote=\\')              -- コマンドを囲う引用符
else
  vim.cmd('set shellcmdflag=-c')
end
vim.cmd('set shellslash')                    -- ファイルパスに\の代わりに/を使用

-- 改行時のコメントアウト継続を無効化
vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = {'*'},
  command = 'setlocal formatoptions-=ro',
})
-- 保存時に行末の空白を除去
vim.api.nvim_create_autocmd({ 'BufWritePre' }, {
  pattern = {'*'},
  command = [[%s/\s\+$//ge]],
})

----------------------------------- Encoding -----------------------------------
vim.cmd('set encoding=utf-8')
vim.cmd('set fileencodings=utf-8,iso-2022-jp,euc-jp,sjis,big5,latin1')
vim.cmd('set fileformats=unix,dos,mac')

----------------------------------- Folding ------------------------------------
vim.api.nvim_create_autocmd({ 'FileType' }, {
  pattern = {'*'},
  command = 'set foldmethod=syntax',
})
vim.api.nvim_create_autocmd({ 'FileType' }, {
  pattern = {'python', 'glsl', 'verilog', 'text'},
  command = 'set foldmethod=syntax',
})
vim.cmd('set nofoldenable')   -- 自動では折りたたまない
vim.cmd('set foldlevel=0')
vim.cmd('set foldcolumn=2')

-------------------------------- Preview Window --------------------------------
vim.cmd('set completeopt=menuone,longest,preview')  -- プレビューウインドウで表示
vim.cmd('set previewheight=1')                      -- プレビューウインドウの高さ
vim.cmd('set splitbelow')                           -- 下に表示
vim.cmd('set laststatus=2')                         -- ステータスラインを常に表示

---------------------------------- Tab Settings --------------------------------
-- Soft Tab
vim.api.nvim_create_autocmd({ 'FileType' }, {
  pattern = {'*'},
  command = 'set tabstop=4 shiftwidth=4 expandtab',
})
vim.api.nvim_create_autocmd({ 'FileType' }, {
  pattern = {'javascript', 'lua'},
  command = 'set tabstop=2 shiftwidth=2 expandtab',
})

-------------------------------- Infinity Undo ---------------------------------
if vim.fn.has('persistent_undo') then
  vim.cmd('set undodir=~/.cache/nvim/.vimundo')
  vim.cmd('set undofile')
end

----------------------------------- Quickfix -----------------------------------
vim.api.nvim_create_autocmd({ 'QuickfixCmdPost' }, {
  pattern = {'make', 'grep', 'grapadd', 'vimgrep'},
  command = 'copen',  -- 自動で開く
})

------------------------------- Wrap with vimdiff ------------------------------
vim.cmd('set diffopt-=internal')  -- internalではFilterWritePreが発生しない
vim.api.nvim_create_autocmd({ 'FilterWritePre' }, {
  pattern = {'*'},
  command = 'if &diff | setlocal wrap< | endif',
})

------------------------------------ KeyBind -----------------------------------
-- 再描画
vim.api.nvim_set_keymap('n', '<F5>', '<C-l>', {noremap = true})
vim.api.nvim_set_keymap('i', '<F5>', '<ESC><C-l>a', {noremap = true})
-- 上下移動を表記上のものにする
vim.api.nvim_set_keymap('n', 'j', 'gj', {})
vim.api.nvim_set_keymap('n', 'k', 'gk', {})
vim.api.nvim_set_keymap('n', '<Up>', 'g<Up>', {})
vim.api.nvim_set_keymap('n', '<Down>', 'g<Down>', {})
-- 高速移動 上下移動は滑らかに
vim.api.nvim_set_keymap('n', '<C-h>', '10h',  {noremap = true})
vim.api.nvim_set_keymap('n', '<C-l>', '10l',  {noremap = true})
vim.api.nvim_set_keymap('n', '<C-j>', '10j',  {noremap = true})
vim.api.nvim_set_keymap('n', '<C-k>', '10k',  {noremap = true})
-- 行末行頭への移動
vim.api.nvim_set_keymap('n', '9', '^',  {noremap = true})
vim.api.nvim_set_keymap('n', '0', '$',  {noremap = true})
-- タブ移動
vim.api.nvim_set_keymap('n', '2', 'gT',  {noremap = true})
vim.api.nvim_set_keymap('n', '3', 'gt',  {noremap = true})
vim.api.nvim_set_keymap('n', '"', ':tabm -1 <CR>',  {noremap = true})
vim.api.nvim_set_keymap('n', '#', ':tabm +1 <CR>',  {noremap = true})
-- クリップボードから貼り付け,コピー
vim.api.nvim_set_keymap('i', '<C-v>', '<Esc>"+gp', { noremap = true })
vim.api.nvim_set_keymap('x', '<C-c>', '"+y', { noremap = true })
vim.api.nvim_set_keymap('c', '<C-v>', '<C-r>+', { noremap = true })
-- 折り込み
vim.api.nvim_set_keymap('n', '<C-c>', 'zc', { noremap = true })
vim.api.nvim_set_keymap('i', '<C-c>', '<Esc>zc', { noremap = true })
-- F1のヘルプを無効化
vim.api.nvim_set_keymap('', '<F1>', '<Esc>', {})
-- 終了(q)を無効化
vim.api.nvim_set_keymap('', '<C-q>', '<Esc>', {})
-- Vimgrep
vim.api.nvim_set_keymap('n', '<A-[>', ':cN<CR>', {})
vim.api.nvim_set_keymap('n', '<A-]>', ':cn<CR>', {})
vim.api.nvim_set_keymap('n', '<C-9>', ':cN<CR>', {})
vim.api.nvim_set_keymap('n', '<C-0>', ':cn<CR>', {})
-- Quickfix/Preview/Location window/Float windowの非表示
vim.api.nvim_set_keymap('n', '<silent><C-c>', ':cclose<CR>:pclose<CR>:lclose<CR>:lua for _, win in ipairs(vim.api.nvim_list_wins()) do local config = vim.api.nvim_win_get_config(win); if config.relative ~= "" then vim.api.nvim_win_close(win, false) end end<CR>', { noremap = true })
-- 検索ハイライトのクリア
vim.api.nvim_set_keymap('n', '<silent><Esc>', ':noh<CR>', { noremap = true })
-- 置換 (start from current cursor)
vim.api.nvim_set_keymap('n', '<F2>', ":,$s/\\<<C-r><C-w>\\>//gc|1,''-&&<Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left>", { noremap = true })

------------------------------ Custom line limits ------------------------------
colorcolumn_mode = 0
function NextColorColumn()
  colorcolumn_mode = (colorcolumn_mode + 1) % 4
  if colorcolumn_mode == 0 then
    vim.opt.colorcolumn = ""
    return 'No line limit'
  elseif colorcolumn_mode == 1 then
    if vim.bo.filetype == 'python' then
      vim.opt.colorcolumn = "79"
      return 'Line limit: 79'
    else
      vim.opt.colorcolumn = "80"
      return 'Line limit: 80'
    end
  elseif colorcolumn_mode == 2 then
    vim.opt.colorcolumn = "100"
    return 'Line limit: 100'
  elseif colorcolumn_mode == 3 then
    vim.opt.colorcolumn = "120"
    return 'Line limit: 120'
  end
end
vim.api.nvim_set_keymap('n', '<F10>', ':lua print(NextColorColumn())<CR>', { noremap = true })

--------------------------------- Spell check toggle ---------------------------
vim.opt.spell = true
vim.opt.spelllang = "en_us,cjk" -- Enabled by default
vim.api.nvim_set_keymap('n', '<F12>', ':set spell! spelllang=en_us,cjk<CR>', { noremap = true })

----------------------------------- Terminal -----------------------------------
-- vim.api.nvim_set_keymap('n', '<F4>', ':new Terminal<CR>:resize 10<CR>:set spell! spelllang=<CR>:terminal<CR>', { noremap = true })
vim.api.nvim_set_keymap('t', '<Esc>', '<C-\\><C-n>', { noremap = true })
vim.api.nvim_create_autocmd({ 'TermOpen' }, {
  pattern = {'*'},
  command = 'set spell! spelllang=',   -- Disable spell check in terminal
})

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
