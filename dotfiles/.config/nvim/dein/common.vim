" ==============================================================================
"                                      Common
" ==============================================================================

set synmaxcol=400                 " ハイライトする文字数を制限する
set backspace=indent,eol,start    " インサートモード時にバックスペースを使う
set whichwrap=b,s,h,l,<,>,[,]     " 行頭から前行文末へ移動可能にする
" set scrolloff=999               " スクロール時にカーソルを中央へ移動
set scrolloff=3                   " スクロールを開始する行数
set cindent                       " cオートインデント
set cinoptions=g0                 " cppでのpublic宣言を下げる
set showtabline=2                 " タブ(上部)を常に表示する
set number                        " 行数を表示する
set hlsearch                      " 検索文字列を強調
set ignorecase                    " 大文字小文字を無視
set smartcase                     " (ただし大文字入力時のみ考慮)
set guioptions-=m                 " メニューバーを非表示
set guioptions-=T                 " ツールバーを非表示
set guioptions-=e                 " TabのGUI表示をOFF
set mouse=a                       " マウスをすべてのモードで使用する
set wildmenu                      " コマンドモードの補完方法
set diffopt+=vertical             " diffは縦分割
set conceallevel=0                " 非表示文字も表示
set nobackup                      " バックアップhoge~を作成しない
set belloff=all                   " ビープ音無効化
set termguicolors                 " CLIでフルカラー
" set ttimeoutlen=50              " ノーマルモードに戻る時間
set cursorline                    " カーソル行をハイライト
set incsearch                     " 入力しながら検索
set breakindent                   " 折り返し時にインデントを考慮
set updatetime=700                " CursorHoldイベントの発生タイミング
if has('win32') || has('win64')
    set shell=cmd.exe             " コマンドプロンプト bashではPATH形式が異なる
    " set shell=C:/msys64/usr/bin/bash
    set shellcmdflag=-c
    set shellxquote=\"            " コマンドを囲う引用符
else
    " set shell=bash
    set shellcmdflag=-c
    " set shellcmdflag=--login\ -s  " Linux-likeなシェル
endif
set shellslash                    " ファイルパスに\の代わりに/を使用
" 改行時のコメントアウト継続を無効化
autocmd BufNewFile,BufRead * setlocal formatoptions-=ro
" 保存時に行末の空白を除去
" autocmd BufWritePre * :%s/\s\+$//ge

" === Encoding ===
set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp,sjis,big5,latin1
set fileformats=unix,dos,mac

" === Folding ===
autocmd FileType * set foldmethod=syntax
autocmd FileType python set foldmethod=indent
autocmd FileType glsl set foldmethod=indent
autocmd FileType verilog set foldmethod=indent
autocmd FileType text set foldmethod=indent
set nofoldenable                           " 自動では折りたたまない
set foldlevel=0
set foldcolumn=2

" === PreviewWindow ===
set completeopt=menuone,longest,preview    " プレビューウインドウで表示
set previewheight=1                        " プレビューウインドウの高さ
set splitbelow                             " 下に表示
set laststatus=2                           " ステータスラインを常に表示

" === Tab Settings ===
" Hard Tab
" autocmd FileType * set tabstop=4 shiftwidth=4 noexpandtab
" autocmd FileType * set tabstop=2 shiftwidth=2 expandtab
" Soft Tab
autocmd FileType * set tabstop=4 shiftwidth=4 expandtab
autocmd FileType javascript set tabstop=2 shiftwidth=2 expandtab
autocmd FileType python     set tabstop=4 shiftwidth=4 expandtab
autocmd FileType neosnippet set noexpandtab

"=== Font Settings (for gvim, not for nvim) ===
if !has('nvim')
    if has('win32') || has('win64')
        " Windows
        set guifont=DejavuNotoNerdPowerline
        " set guifont=DejaVu\ Sans\ Mono\ Book\ 10.0
        " set guifont=MS_Gothic:h10
    else
        " Others
        set guifont=DejavuNotoNerdPowerline
        " set guifont=DejaVu\ Sans\ Mono\ Book\ 11.0
    endif
endif

"=== Infinity Undo ===
if has('persistent_undo')
    set undodir=~/.cache/nvim/.vimundo
    set undofile
endif

"===== Quickfix =====
autocmd QuickfixCmdPost make,grep,grepadd,vimgrep copen  "自動で開く

"===== Wrap with vimdiff =====
set diffopt-=internal  " internalではFilterWritePreが発生しない
autocmd FilterWritePre * if &diff | setlocal wrap< | endif

"===== KeyBind =====
" 再描画
nnoremap <F5> <C-l>
inoremap <F5> <Esc><C-l>a
" 上下移動を表記上のものにする
map j gj
map k gk
noremap <Up> g<Up>
noremap <Down> g<Down>
" 高速移動 上下移動は滑らかに
noremap <C-h> 10h
noremap <C-l> 10l
noremap <C-j> 20j
noremap <C-k> 20k
" 行末行頭への移動
noremap 9 ^
noremap 0 $
" タブ移動
nnoremap 3 gt
nnoremap 2 gT
nnoremap " :tabm -1 <CR>
nnoremap # :tabm +1 <CR>
nnoremap <C-2> :tabm -1 <CR>
nnoremap <C-3> :tabm +1 <CR>
" クリップボードから貼り付け,コピー
inoremap <C-v> <ESC>"+gp
vnoremap <C-c> "+y
cnoremap <C-v> <C-r>+
" 折り込み
" nnoremap <C-c> zc
" inoremap <C-c> <Esc>zc
" F1のヘルプを無効化
map <F1> <Esc>
" 終了(q)を無効化
map <C-q> <Esc>
" Vimgrep
nmap <A-[> :cN<CR>
nmap <A-]> :cn<CR>
nmap <C-9> :cN<CR>
nmap <C-0> :cn<CR>
" Quickfix/Preview/Location window/float windowの非表示
nnoremap <silent><C-c> :cclose<CR>:pclose<CR>:lclose<CR>:close<CR>
" omni補完
" inoremap <C-o> <C-x><C-o>
" 検索ハイライトのクリア
nmap <silent><Esc> :noh<CR>
" 置換 (start from current cursor)
nnoremap <F2> :,$s/\<<C-r><C-w>\>//gc\|1,''-&&<Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left>
nmap <leader>s <F2>

"==== Auto fcitx ====
if !has('win32') && executable("fcitx-remote")
    let g:input_toggle = 0
    function! Fcitx2en()
        let s:input_status = system("fcitx-remote")
        if s:input_status == 2
            let g:input_toggle = 1
            let l:a = system("fcitx-remote -c")
        endif
    endfunction
    function! Fcitx2zh()
        let s:input_status = system("fcitx-remote")
        if s:input_status != 2 && g:input_toggle == 1
            let l:a = system("fcitx-remote -o")
            let g:input_toggle = 0
        endif
    endfunction
    set iminsert=0
    set imsearch=0
    autocmd InsertLeave * call Fcitx2en()
    autocmd InsertEnter * call Fcitx2zh()
endif

"===== Custom line limits =====
let s:colorcolumn_mode = 0
function! NextColorColumn()
    let s:colorcolumn_mode = (s:colorcolumn_mode + 1) % 4
    if s:colorcolumn_mode == 0
        set colorcolumn=-1
        return 'No line limit'
    elseif s:colorcolumn_mode == 1
        if &filetype == 'python'
            set colorcolumn=79
            return 'Line limit: 79'
        else
            set colorcolumn=80
            return 'Line limit: 80'
        endif
    elseif s:colorcolumn_mode == 2
        set colorcolumn=100
        return 'Line limit: 100'
    elseif s:colorcolumn_mode == 3
        set colorcolumn=120
        return 'Line limit: 120'
    endif
endfunction
autocmd VimEnter * call NextColorColumn()  " Initial call
nnoremap <silent><F11> :echo NextColorColumn()<CR>

"===== Spell check toggle =====
set spell spelllang=en_us,cjk  " Enabled by default
nnoremap <F12> :set spell! spelllang=en_us,cjk<CR>

"===== Terminal =====
nnoremap <F4> :new Terminal<CR>:resize 10<CR>:set spell! spelllang=<CR>:terminal<CR>
tnoremap <silent><ESC> <C-\><C-n>
let g:terminal_color_0  = "#aaaaaa" "black
let g:terminal_color_1  = "#ed5f67" "red
let g:terminal_color_2  = "#9ac895" "green
let g:terminal_color_3  = "#fbc963" "yellow
let g:terminal_color_4  = "#669acd" "blue
let g:terminal_color_5  = "#c695c6" "magenta
let g:terminal_color_6  = "#5fb4b4" "cyan
let g:terminal_color_7  = "#c1c6cf" "white
let g:terminal_color_8  = "#65737e" "bright black
let g:terminal_color_9  = "#fa9257" "bright red
let g:terminal_color_10 = "#343d46" "bright green
let g:terminal_color_11 = "#4f5b66" "bright yellow
let g:terminal_color_12 = "#a8aebb" "bright blue
let g:terminal_color_13 = "#ced4df" "bright magenta
let g:terminal_color_14 = "#ac7967" "bright cyan
let g:terminal_color_15 = "#d9dfea" "bright white
let g:terminal_color_background="#aaaaaa" "background
let g:terminal_color_foreground="#c1c6cf" "foreground

"===== Python path for conda on Windows =====
if has('win32') || has('win64')
    let s:miniconda_python_prog = 'C:/ProgramData/Miniconda3/python'
    if executable(s:miniconda_python_prog)
        let g:python3_host_prog = s:miniconda_python_prog
    endif
endif
