if has('vim_starting')
    set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

"NeoBundleFetch 'Shougo/neobundle.vim'

set synmaxcol=500                 " ハイライトする文字数を制限する
set backspace=indent,eol,start    " インサートモード時にバックスペースを使う
set whichwrap=b,s,h,l,<,>,[,]     " 行頭から前行文末へ移動可能にする
" set scrolloff=999               " スクロール時にカーソルを中央へ移動
set scrolloff=3                   " スクロールを開始する行数
set cindent                       " cオートインデント
set cinoptions=g0                 " cppでのpublic宣言を下げる
" set showtabline=2               " タブ(上部)を常に表示する
" set mouse=a                     " マウス
" set ttymouse=xterm2
set number                        " 行数を表示する
set nohlsearch                    " 検索文字列を強調を無効化
set ignorecase                    " 大文字小文字を無視
set smartcase                     " (ただし大文字入力時のみ考慮)
set guioptions-=m                 " メニューバーを非表示
set guioptions-=T                 " ツールバーを非表示
" set guioptions-=e               " TabのGUI表示をOFF
set colorcolumn=80                " 80文字のライン
set wildmenu                      " コマンドモードの補完方法
set diffopt+=vertical             " diffは縦分割
set conceallevel=0                " 非表示文字も表示
set nobackup                      " バックアップhoge~を作成しない
set belloff=all                   " ビープ音無効化
set termguicolors                 " CLIでフルカラー
" === Encoding ===
set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp,sjis,latin1
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
" set completeopt=menuone
set previewheight=1                        " プレビューウインドウの高さ
set splitbelow                             " 下に表示
set laststatus=2                           " ステータスラインを常に表示
" autocmd BufWritePre * :%s/\s\+$//ge      " 保存時に行末の空白を除去する
" === Tab Settings ===
" Hard Tab
" autocmd FileType * set tabstop=4 shiftwidth=4 noexpandtab
" autocmd FileType * set tabstop=2 shiftwidth=2 expandtab
" Soft Tab
autocmd FileType * set tabstop=4 shiftwidth=4 expandtab
autocmd FileType javascript set tabstop=2 shiftwidth=2 expandtab
autocmd FileType python     set tabstop=4 shiftwidth=4 expandtab
autocmd FileType neosnippet set noexpandtab " 効いていない？
"=== Font Settings ===
if has('win32') || has('win64')
    set guifont=MS_Gothic:h10 " Windows
else
    " Windows以外
    " set guifont=DejaVu\ Sans\ Mono\ 8.7
    " set guifont=DejaVu\ Sans\ Mono\ 9.8
    set guifont=DejaVu\ Sans\ Mono\ Book\ 11.0
    " set guifont=DejaVu\ Sans\ Mono\ 13
    " set lsp=4 " gvimでの行間
endif
"=== Infinity Undo ===
if has('persistent_undo')
    set undodir=~/.vimundo " ~/.vim/undo
    set undofile
endif
"===== Quickfix =====
autocmd QuickfixCmdPost make,grep,grepadd,vimgrep copen  "自動で開く


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
noremap <C-j> 4j4j4j4j4j
noremap <C-k> 4k4k4k4k4k
" 行末行頭への移動
noremap 9 ^
noremap 0 $
" タブ移動
nnoremap <F3> gt
inoremap <F3> <Esc>gt
nnoremap <F2> gT
inoremap <F2> <Esc>gT
nnoremap 3 gt
nnoremap 2 gT
nnoremap " :tabm -1 <CR>
nnoremap # :tabm +1 <CR>
" クリップボードから貼り付け,コピー
" nnoremap <C-v> "+gp
inoremap <C-v> <ESC>"+gp
vnoremap <C-c> "+y
" 折り込み
" nnoremap <C-c> zc
" inoremap <C-c> <Esc>zc
" F1のヘルプを無効化
map <F1> <Esc>
" Vimgrep
nmap <A-[> :cN<CR>
nmap <A-]> :cn<CR>
" Quickfix windowの非表示
nnoremap <C-c> :cclose<CR>
" omni補完
inoremap <C-o> <C-x><C-o>
" omni補完(marching) オムニ補完時に補完ワードを挿入しない
" au FileType c,cpp imap <buffer> <C-x><C-o> <Plug>(marching_start_omni_complete)
" omni補完(marching) キャッシュを破棄、再取得
" au FileType c,cpp imap <buffer> <C-x><C-x><C-o> <Plug>(marching_force_start_omni_complete)
" Enterで補完を決定、または次へジャンプ
imap <expr><CR> pumvisible() ? "\<Plug>(neosnippet_expand)" :
            \ (neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" :"\<CR>")
" S-Enter,C-Enterで次へジャンプ、または補完を閉じてEnter
imap <expr><S-CR> neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" :
            \ (neocomplete#close_popup() ? "\<CR>" :"\<S-CR>")
imap <expr><C-CR> neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" :
            \ (neocomplete#close_popup() ? "\<CR>" :"\<C-CR>")
" Tabで選択
imap <expr><TAB> pumvisible() ?
            \ "\<C-n>" : (neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" : "\<TAB>")
smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
"補完のShift-Tab
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"
" Escで補完ポップアップを閉じて標準モード
inoremap <expr><Esc> pumvisible() ? neocomplete#close_popup()."<Esc>" : "<Esc>"
" 検索文字列のハイライトが有効なら解除
" noremap <expr><Esc><Esc> v:hlsearch == 1 ? ":nohlsearch<CR>" : "<Esc>"


"===== Plugins =====
"=== 共通 ===
NeoBundle 'takiyu/tango-lx'                 " カラースキーム
NeoBundle 'tyru/caw.vim'                    " コメントアウト補助
NeoBundle 'scrooloose/nerdtree'             " Filer
NeoBundle 'rhysd/clever-f.vim'              " Clever-f
NeoBundle 'itchyny/lightline.vim'           " ステータスライン
NeoBundle 't9md/vim-quickhl'                " ハイライト
NeoBundle 'vimtaku/hl_matchit.vim'          " 括弧+αをハイライト
NeoBundle 'w0rp/ale'                        " 文法チェック
NeoBundle 'maximbaz/lightline-ale'          " 文法チェックのステータスライン表示
NeoBundle 'ujihisa/neco-look'               " 英単語補完
NeoBundle 'vim-scripts/YankRing.vim'        " ヤンク履歴
NeoBundle 'mbbill/undotree'                 " undo可視化
NeoBundle 'tpope/vim-sleuth'                " インデント自動検出
NeoBundle 'nathanaelkane/vim-indent-guides' " インデント明示化
NeoBundle 'bronson/vim-trailing-whitespace' " 行末スペース可視化
NeoBundle 'airblade/vim-gitgutter'          " Git差分ガイド
NeoBundle 'tpope/vim-fugitive'              " Git補助
NeoBundle 'cohama/agit.vim'                 " Git Graph
NeoBundle 'will133/vim-dirdiff'             " ディレクトリ差分
NeoBundle 'takiyu/gtrans-web.vim'           " Google翻訳
"=== 補完 (+luaが必要) ===
NeoBundle 'Shougo/neocomplete'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'takiyu/my-vim-snippets'
NeoBundle 'Shougo/vimproc', {
            \ 'build' : {
            \     'windows' : 'tools\\update-dll-mingw',
            \     'cygwin' : 'make -f make_cygwin.mak',
            \     'mac' : 'make -f make_mac.mak',
            \     'linux' : 'make',
            \     'unix' : 'gmake',
            \    },
            \ }

"=== GLSL ===
NeoBundle 'tikhomirov/vim-glsl'
"===C/C++ ===
NeoBundleLazy 'vim-jp/cpp-vim', {
            \ 'autoload':{ 'filetypes':[ 'cpp' ]} }
" NeoBundleLazy 'rhysd/vim-clang-format', {
"             \ 'autoload':{ 'filetypes':[ 'c', 'cpp' ]} }
" NeoBundleLazy 'Rip-Rip/clang_complete', {
"             \ 'autoload':{ 'filetypes':[ 'c', 'cpp' ]} }
" NeoBundleLazy 'osyo-manga/vim-marching', {
"             \ 'autoload':{ 'filetypes':[ 'c', 'cpp' ]},
"             \ 'depends' : ['Shougo/vimproc'] }
"=== Unity C# ==
" NeoBundleLazy 'nosami/Omnisharp', {
"             \   'autoload': {'filetypes': ['cs']},
"             \   'build': {
"             \     'mac': 'xbuild server/OmniSharp.sln',
"             \     'unix': 'xbuild server/OmniSharp.sln', }}
" NeoBundleLazy 'tpope/vim-dispatch', {
"             \   'autoload': {'filetypes': ['cs']},
"             \ }
"=== TeX ===
NeoBundleLazy 'LaTeX-Box-Team/LaTeX-Box', {
            \ 'autoload':{ 'filetypes':[ 'tex', 'plaintex' ]}
            \ }
"=== Verilog ===
NeoBundleLazy 'vim-scripts/verilog.vim', {
            \ 'autoload':{ 'filetypes':[ 'verilog' ]}
            \ }
"=== JavaScript ===
NeoBundleLazy 'marijnh/tern_for_vim', {
            \ 'autoload':{ 'filetypes':[ 'javascript' ]},
            \ 'build': { 'others': 'npm install' } }
NeoBundleLazy 'othree/yajs.vim', {
             \ 'autoload':{ 'filetypes':[ 'javascript' ]} }
" NeoBundle 'hallison/vim-markdown'
"=== Ruby ===
" NeoBundleLazy 'cespare/ruby-complete', {
"             \'autoload':{'filetypes':[ 'ruby' ]} }
"=== Python ===
" Needed plugins: sudo pip install jedi pep8 pyflakes autopep8
NeoBundleLazy 'davidhalter/jedi-vim', {
            \ 'autoload':{ 'filetypes':[ 'python' ]} }
" NeoBundleLazy 'tmhedberg/SimpylFold', {
"             \ 'autoload':{ 'filetypes':[ 'python' ]} }
"=== Golang ===
NeoBundle 'fatih/vim-go' " filetype認識のため、Lazyにするにはautocmdの必要あり
" :GoInstallBinarys を実行
" NeoBundleLazy 'fatih/vim-go', {
"             \ 'autoload':{ 'filetypes':[ 'go' ]}
"             \ }
" NeoBundle 'google/vim-ft-go' "補完機能等が含まれず
"=== Lisp ===
NeoBundleLazy 'luochen1990/rainbow', {
            \ 'autoload':{ 'filetypes':[ 'lisp' ]}
            \ }
"=== Json ===
NeoBundleLazy 'elzr/vim-json', {
            \ 'autoload':{ 'filetypes':[ 'json' ]}
            \ }
"=== Vim ===
NeoBundleLazy 'LeafCage/vimhelpgenerator', {
            \ 'autoload':{ 'filetypes':[ 'vim' ]}
            \ }
let g:vim_json_syntax_conceal = 0

NeoBundleCheck
call neobundle#end() "call function() はこれ以降でないと無効

filetype plugin indent on
syntax on


"####### Plugin : fatih/vim-go (Golang) #######
au FileType go nmap <Leader>i <Plug>(go-info)
au FileType go nmap <Leader>dc <Plug>(go-doc)
au FileType go nmap <Leader>db <Plug>(go-doc-browser)
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <Leader>e <Plug>(go-rename)
" let g:go_highlight_functions = 1
" let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_fmt_autosave = 1

"####### Plugin : hl-matchit #######
source $VIMRUNTIME/macros/matchit.vim "括弧を追加
let g:hl_matchit_enable_on_vim_startup = 1 "ハイライトを有効

"####### Plugin : rainbow #######
let g:rainbow_active = 1

"####### Plugin : vim-indent-guides #######
let g:indent_guides_enable_on_vim_startup = 1 "autostart

"####### Plugin : OmniSharp #######
let g:OmniSharp_host = "http://localhost:2000"
let g:OmniSharp_typeLookupInPreview = 1

"####### Plugin : latex-box #######
let g:tex_flavor = 'latex'
let g:tex_conceal=''
" let g:tex_conceal='abgms'
" let g:tex_conceal='adbmgs'

autocmd BufWritePost *.tex :Latexmk
" autocmd BufWritePost *.tex :!latexmk
let g:LatexBox_personal_latexmkrc = 1  " To disable g:LatexBox_output_type
" let g:LatexBox_output_type = 'pdf'
" let g:LatexBox_latexmk_options = ''
let g:LatexBox_viewer = 'xdg-open'
let g:LatexBox_quickfix = 2  " cursor stays in the main window
let g:LatexBox_autojump = 0
let g:LatexBox_complete_inlineMath = 1
let g:LatexBox_Folding = 1
let g:LatexBox_latexmk_async = 1

"####### Plugin : lightline #######
let g:lightline = {
            \ 'colorscheme': 'Tomorrow_Night_Bright',
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ],
            \             [ 'readonly', 'filename', 'modified' ] ],
            \   'right': [ [ 'percentlineinfo' ],
            \              [ 'fileinfo' ],
            \              [ 'gitstatus'],
            \              [ 'linter_checking', 'linter_errors',
            \                'linter_warnings', 'linter_ok' ]],
            \ },
            \ 'component': {
            \   'readonly': '%{&readonly?"R":"W"}',
            \   'percentlineinfo': '%3p%% %3l:%-2v',
            \   'fileinfo': '%{&fileencoding}  %{&fileformat}  %{&filetype}',
            \ },
            \ 'component_function': {
            \   'gitstatus': 'LightlineGitStatus',
            \ },
            \ 'component_expand': {
            \   'linter_checking': 'lightline#ale#checking',
            \   'linter_warnings': 'lightline#ale#warnings',
            \   'linter_errors': 'lightline#ale#errors',
            \   'linter_ok': 'lightline#ale#ok',
            \ },
            \ 'component_type': {
            \   'linter_checking': 'left',
            \   'linter_warnings': 'warning',
            \   'linter_errors': 'error',
            \   'linter_ok': 'left',
            \ },
            \ 'separator': {'left': '', 'right': ' '},
            \ 'subseparator': {'left': '|', 'right': '|'},
            \ }
" Git状態のステータスライン表示
function! LightlineGitStatus()
    if winwidth('.') <= 60
        return ''
    endif
    let ret = []
    try
        if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
            " 変更行数表示
            if exists('*GitGutterGetHunkSummary') && get(g:, 'gitgutter_enabled', 0)
                let symbols = ['++', '-+', '--']
                let hunks = GitGutterGetHunkSummary()
                for i in range(3)
                    if hunks[i] > 0
                        call add(ret, symbols[i] . hunks[i])
                    endif
                endfor
            endif
            " Branch名
            let mark = ' '  " edit here for cool mark
            let branch = fugitive#head()
            if branch !=# ''
                call add(ret, mark.branch)
            endif
        endif
    catch
    endtry
    return join(ret, ' ')
endfunction
autocmd TextChanged * call lightline#update()
autocmd TextChangedI * call lightline#update()

"####### Plugin : ALE #######
nnoremap <F11> :ALEToggle<CR>
nnoremap <F9> :ALEFix<CR>
let b:ale_fixers = {
    \ 'javascript': ['eslint'],
    \ 'python': ['autopep8'],
    \ 'c': ['clang-format'],
    \ 'cpp': ['clang-format'],
    \ }

"####### Plugin : nerdtree #######
noremap <C-e> :NERDTreeToggle<CR>
map <expr><C-f> g:NERDTree.IsOpen() ? ":NERDTreeClose<CR>:NERDTreeFind<CR>" : "<C-f>"
let NERDTreeQuitOnOpen = 1           " 開いたら非表示
" let NERDTreeMapOpenInTab='<ENTER>' " デフォルトでタブで開く (フォルダ移動などはoを使用)

"####### Plugin : clever_f #######
" 大文字入力時のみ考慮
let g:clever_f_ignore_case = 1
let g:clever_f_smart_case = 1
" 日本語
autocmd FileType text let g:clever_f_use_migemo = 1
autocmd FileType tex let g:clever_f_use_migemo = 1
autocmd FileType plaintex let g:clever_f_use_migemo = 1
" 移動方向を修正
let g:clever_f_fix_key_direction = 1
" 記号の代用文字
let g:clever_f_chars_match_any_signs = ';'

"####### Plugin : quickhl.vim #######
let g:quickhl_manual_hl_priority = 10       " プライオリティの設定
" let g:quickhl_cword_enable_at_startup = 1 " カーソル下の単語を一時的にハイライト
" 色指定(同時に個数も指定)
let g:quickhl_manual_colors = [
            \ "gui=bold ctermbg=Cyan    ctermfg=Black guibg=#8CCBEA guifg=Black",
            \ "gui=bold ctermbg=Green   ctermfg=Black guibg=#A4E57E guifg=Black",
            \ "gui=bold ctermbg=Yellow  ctermfg=Black guibg=#FFDB72 guifg=Black",
            \ "gui=bold ctermbg=Red     ctermfg=Black guibg=#FF7272 guifg=Black",
            \ "gui=bold ctermbg=Magenta ctermfg=Black guibg=#FFB3FF guifg=Black",
            \ "gui=bold ctermbg=Blue    ctermfg=Black guibg=#9999FF guifg=Black",
            \ "gui=bold ctermbg=DarkCyan    ctermfg=Black guibg=#436170 guifg=Black",
            \ "gui=bold ctermbg=DarkGreen   ctermfg=Black guibg=#62894b guifg=Black",
            \ "gui=bold ctermbg=DarkYellow  ctermfg=Black guibg=#998344 guifg=Black",
            \ "gui=bold ctermbg=DarkRed     ctermfg=Black guibg=#994444 guifg=Black",
            \ "gui=bold ctermbg=DarkMagenta ctermfg=Black guibg=#996b99 guifg=Black",
            \ "gui=bold ctermbg=DarkBlue    ctermfg=Black guibg=#5b5b99 guifg=Black",
            \ ]
" ハイライトショートカット
nmap m <Plug>(quickhl-manual-this)
vmap m <Plug>(quickhl-manual-this)
" 表示トグル
nmap <F6> <Plug>(quickhl-manual-toggle)
" ハイライトを削除
nmap M <Plug>(quickhl-manual-reset)
vmap M <Plug>(quickhl-manual-reset)

"####### Plugin : caw.vim #######
" コメントアウトのショートカット
nmap \c <Plug>(caw:zeropos:toggle)
vmap \c <Plug>(caw:zeropos:toggle)
nmap \C <Plug>(caw:zeropos:uncomment)
vmap \C <Plug>(caw:zeropos:uncomment)
autocmd FileType glsl let b:caw_oneline_comment = '//'

"####### Plugin : yankring.vim #######
let g:yankring_history_dir = $HOME.'/.vim'

"####### Plugin : undotree.vim #######
nmap U :<C-u>UndotreeToggle<CR>

"####### Plugin : vim-indent-guides #######
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree']

"####### Plugin : vim-gitgutter #######
set signcolumn=yes
let g:gitgutter_sign_added = '++'
let g:gitgutter_sign_modified = '-+'
let g:gitgutter_sign_removed = '__'
let g:gitgutter_sign_modified_removed = '+_'

"####### Plugin : agit.vim #######
let g:agit_enable_auto_show_commit = 0

"####### Plugin : gtransweb.vim #######
let g:gtransweb_async_mode = 1
let g:gtransweb_src_lang = 'en'
let g:gtransweb_tgt_lang = 'ja'
vnoremap <C-g>t :GtransWebPreview<CR>
vnoremap <C-g>r :GtransWebReplace<CR>
nnoremap <C-g>s :GtransWebSwapLangs<CR>

"####### Plugin : marching #######
" let g:marching_clang_command = "clang-3.6"
let g:marching_clang_command = "clang"
let g:marching_enable_neocomplete = 1
set updatetime=200
let g:marching_backend = "clang_command"  "非同期
" let g:marching_include_paths = filter(
"     \    split(glob('/usr/include/c++/*'), '\n') +
"     \    split(glob('/usr/include/*/c++/*'), '\n') +
"     \    split(glob('/usr/include/*/'), '\n'),
"     \    'isdirectory(v:val)')
"####### Plugin : clang_complete #######
" " let g:clang_library_path = $HOME.'/dotfiles'
" " let g:clang_library_path = $HOME.'/local/lib'
" " let g:clang_library_path = '/usr/lib/llvm-3.4/lib'
" " clang_complete では自動補完を行わない用に設定
" let g:clang_complete_auto = 0
" let g:clang_auto_select = 0
" let g:clang_sort_patterns = 'none'

"####### Plugin : jedi.vim (Python) #######
let g:jedi#popup_select_first = 0
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
" disable showing function arguments in upper line
let g:jedi#show_call_signatures = 2
" non-auto close preview window
let g:jedi#auto_close_doc = 0

"####### Plugin : term_for_vim (JavaScript) #######
" let g:tern_show_argument_hints = 'on_move'
" let g:tern_show_argument_hints = 'on_hold'
" au FileType javascript nmap <silent> <Leader>t :TernType<CR>
au FileType javascript nmap <silent> <c-t> :TernType<CR>
au FileType javascript nmap <silent> <Leader>d :TernDoc<CR>
let g:tern_show_signature_in_pum = 1
let g:tern#command = ["nodejs", expand('$HOME').'/.vim/bundle/tern_for_vim/node_modules/tern/bin/tern', '--no-port-file'] " for Ubuntu command

"####### Plugin : vim-javascript-syntax (JavaScript) #######
" au FileType javascript call JavaScriptFold()

"####### Plugin : vimhelpgenerator (Vim) #######
let g:vimhelpgenerator_defaultlanguage = 'en'
let g:vimhelpgenerator_author = 'takiyu'

"####### Plugin : neocomplete #######
let g:neocomplete#enable_at_startup = 1                  " neocompleteを有効
let g:neocomplete#enable_auto_select = 0                 " 候補を自動選択しない
let g:neocomplete#auto_completion_stairt_length = 3      " 補完が自動で開始される文字数
let g:neocomplete#skip_auto_completion_time = "0.5"
let g:neocomplete#enable_ignore_case = 1                 " 大文字小文字を無視
let g:neocomplete#enable_smart_case = 1                  " (ただし大文字入力時のみ考慮)
let g:neocomplete#enable_underbar_completion = 0         " アンダーバー補完を有効
let g:neocomplete#sources#syntax#min_keyword_length = 3  " シンタックスをキャッシュするときの最小文字数
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'    " ロックパターン
call neocomplete#custom#source('_', 'sorters', ['sorter_length'])   " ソート
let g:neocomplete#enable_auto_close_preview = 0          " プレビューウインドウを閉じない
let g:neocomplete#use_vimproc = 1                        " バックグラウンド実行
" 辞書設定
let g:neocomplete#sources#dictionary#dictionaries = { 'default' : '', 'vimshell' : $HOME.'/.vimshell_hist', 'scheme' : $HOME.'/.gosh_completions' }
" 補完するためのキーワードパターン
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'  "日本語を補完候補として取得しない

" omni補完 omnifunc
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
" autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
" autocmd FileType c set omnifunc=ccomplete#Complete
" autocmd FileType cpp set omnifunc=cppcomplete#Complete
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType typescript setlocal omnifunc=TSScompleteFunc
autocmd FileType cs set omnifunc=OmniSharp#Complete
" autocmd FileType ruby,eruby setlocal omnifunc=rubycomplete#Complete
" autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
" autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
" autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
" autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType python setlocal omnifunc=jedi#completions

" omni補完 input_pattern
if !exists('g:neocomplete#sources#omni#input_patterns')
    let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.cs = '.*[^=\);]'
let g:neocomplete#sources#omni#input_patterns.typescript = '.*[^=\);]'
let g:neocomplete#sources#omni#input_patterns.javascript = '[^. *\t]\.\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.objc = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplete#sources#omni#input_patterns.objcpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.go = '[^.[:digit:] *\t]\.\w*'

" omni補完 force_input_pattern
if !exists('g:neocomplete#force_omni_input_patterns')
    let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_overwrite_completefunc = 1
" let g:neocomplete#force_omni_input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
" let g:neocomplete#force_omni_input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#force_omni_input_patterns.objc = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.objcpp = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'

"####### Plugin : Eclim #######
" autocmd FileType java noremap \f :JavaFormat<CR>
" autocmd FileType java let g:EclimCompletionMethod = 'omnifunc'
" autocmd FileType java let g:neocomplete#force_omni_input_patterns.java = '\k\.\k*'

"####### Plugin : neosnippet #######
let g:neosnippet#disable_runtime_snippets = {'_' : 1 }
" For snippet_complete marker
if has('conceal')
    " set conceallevel=2
    set concealcursor=niv
endif
" スニペットファイルの保存ディレクトリのパスを登録
let g:neosnippet#snippets_directory='~/.vim/bundle/my-vim-snippets/snippets'
let g:neosnippet#enable_preview = 1

"===== Color Scheme =====
colorscheme tango_lx
"== for plugin (vim-gitgutter) ==
hi GitGutterAdd guifg=#8ae234 gui=bold ctermfg=green cterm=bold
hi GitGutterChange guifg=#8ae234 gui=bold ctermfg=green cterm=bold
hi GitGutterDelete guifg=#f92672 gui=bold ctermfg=red cterm=bold
hi GitGutterChangeDelete guifg=#8ae234 gui=bold ctermfg=green cterm=bold


"===== GUIタブの表示設定 =====
function! GuiTabLabel() " 個別に設定
    let l:label = ''
    let l:bufnrlist = tabpagebuflist(v:lnum)  " タブに含まれるバッファ(ウィンドウ)情報を取得
    " 表示文字列にバッファ名中のファイル名を追加
    let l:bufname = fnamemodify(bufname(l:bufnrlist[tabpagewinnr(v:lnum) - 1]), ':t')
    let l:label .= l:bufname == '' ? 'No title' : l:bufname " バッファ名がなければNo title
    let l:wincount = tabpagewinnr(v:lnum, '$') " タブ内にウィンドウが複数あるときにはその数を追加
    if l:wincount > 1
        let l:label .= '[' . l:wincount . ']'
    endif
    for bufnr in l:bufnrlist " 変更のあるバッファがるときには '[+]' を追加
        if getbufvar(bufnr, "&modified")
            let l:label .= '[+]'
            break
        endif
    endfor
    return l:label
endfunction
" guitablabelに上の関数を設定
set guitablabel=%N:\ %{GuiTabLabel()}


"==== Auto fcitx ====
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
set ttimeoutlen=150
autocmd InsertLeave *.txt call Fcitx2en()
autocmd InsertEnter *.txt call Fcitx2zh()
autocmd InsertLeave *.tex call Fcitx2en()
autocmd InsertEnter *.tex call Fcitx2zh()
autocmd InsertLeave *.md call Fcitx2en()
autocmd InsertEnter *.md call Fcitx2zh()
autocmd InsertLeave *.plaintex call Fcitx2en()
autocmd InsertEnter *.plaintex call Fcitx2zh()


"===== engdict (http://d.hatena.ne.jp/aki-yam/20080629/1214757485) =====
function! EngDict()
    sp +enew | put = system('engdict ' . @*)
    setlocal bufhidden=hide noswapfile noro nomodified
    normal gg
endfunction
vnoremap <silent> <c-d> :call EngDict()<CR>


"===== Spell check toggle =====
let s:spell_check_flag = 1
set spell spelllang=en_us
function! g:Spellcheck_toggle()
    if s:spell_check_flag
        let s:spell_check_flag = 0
        set nospell
        echomsg string('spell off')
    else
        let s:spell_check_flag = 1
        set spell spelllang=en_us
        echomsg string('spell on')
    endif
endfunction
nnoremap <F12> :call g:Spellcheck_toggle()<CR>
