if has('vim_starting')
set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

"NeoBundleFetch 'Shougo/neobundle.vim'
"
set synmaxcol=500				" ハイライトする文字数を制限する
set backspace=indent,eol,start	" インサートモード時にバックスペースを使う
set whichwrap=b,s,h,l,<,>,[,]	" 行頭から前行文末へ移動可能にする
" set scrolloff=999				" スクロール時にカーソルを中央へ移動
set scrolloff=3					" スクロールを開始する行数
set cindent						" cオートインデント
set cinoptions=g0				" cppでのpublic宣言を下げる
" set showtabline=2				" タブ(上部)を常に表示する
set mouse=a						" マウス
" set ttymouse=xterm2
set number						" 行数を表示する
" set hlsearch					" 検索文字列を強調
set ignorecase					" 大文字小文字を無視
set smartcase					" (ただし大文字入力時のみ考慮)
set guioptions-=m				" メニューバーを非表示
set guioptions-=T				" ツールバーを非表示
" set guioptions-=e				" TabのGUI表示をOFF
" === Folding ===
autocmd FileType * set foldmethod=syntax
autocmd FileType python set foldmethod=indent
autocmd FileType glsl set foldmethod=indent
autocmd FileType verilog set foldmethod=indent
autocmd FileType text set foldmethod=indent
" set nofoldenable "自動では折りたたまない
set foldlevel=0
set foldcolumn=2
" === PreviewWindow ===
set completeopt=menuone,longest,preview	" プレビューウインドウで表示
" set completeopt=menuone
set previewheight=1						" プレビューウインドウの高さ
set splitbelow							" 下に表示
set laststatus=2						" ステータスラインを常に表示
" autocmd BufWritePre * :%s/\s\+$//ge	" 保存時に行末の空白を除去する
" === Tab Settings ===
" Hard Tab
autocmd FileType * set tabstop=4 | set shiftwidth=4 | set noexpandtab
                      " タブを挿入幅  タブを表示幅  Hard Tab
" Soft Tab
autocmd FileType javascript set tabstop=2 | set shiftwidth=2 | set expandtab
autocmd FileType python     set tabstop=4 | set shiftwidth=4 | set expandtab
autocmd FileType neosnippet set noexpandtab "効いていない？
"=== Font Settings ===
if has('win32') || has('win64')
	set guifont=MS_Gothic:h13 " Windows
else
    " Windows以外
	set guifont=DejaVu\ Sans\ Mono\ 9
" 	set guifont=DejaVu\ Sans\ Mono\ 13
	" set lsp=4 " gvimでの行間
endif
"=== Infinity Undo ===
if has('persistent_undo')
    set undodir=~/.vimundo " ~/.vim/undo
    set undofile
endif


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
noremap <C-j> 2j2j2j2j2j
noremap <C-k> 2k2k2k2k2k
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
" PreviewWindowの非表示
nnoremap <C-c> <C-w>z
inoremap <C-c> <C-w>z
" omni補完
inoremap <C-o> <C-x><C-o>
" omni補完(marching) オムニ補完時に補完ワードを挿入しない
au FileType c,cpp imap <buffer> <C-x><C-o> <Plug>(marching_start_omni_complete)
" omni補完(marching) キャッシュを破棄、再取得
au FileType c,cpp imap <buffer> <C-x><C-x><C-o> <Plug>(marching_force_start_omni_complete)
" Enterで補完を決定、または次へジャンプ
imap <expr><CR> pumvisible() ? "\<Plug>(neosnippet_jump_or_expand)" :
			\ neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" :"\<CR>"
" Tabで選択
imap <expr><TAB> pumvisible() ? "\<C-n>" :
			\ neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
"補完のShift-Tab
imap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"
" Escで補完ポップアップを閉じて標準モード(続けて書く方法がわからなかった)
imap <expr><Esc> pumvisible() ? neocomplete#close_popup() ? "<Esc>" : "<Esc>" : "<Esc>"
" 検索文字列のハイライトが有効なら解除
" noremap <expr><Esc><Esc> v:hlsearch == 1 ? ":nohlsearch<CR>" : "<Esc>"

"===== Plugins =====
"=== 共通 ===
NeoBundle 'takiyu/tango-lx'				    " カラースキーム
NeoBundle 'tyru/caw.vim'				    " コメントアウト補助
NeoBundle 'scrooloose/nerdtree'			    " Filer
NeoBundle 'scrooloose/syntastic'		    " 文法チェック
NeoBundle 'rhysd/clever-f.vim'			    " Clever-f
NeoBundle 'itchyny/lightline.vim'		    " ステータスライン
NeoBundle 't9md/vim-quickhl'			    " ハイライト
NeoBundle 'vimtaku/hl_matchit.vim'	        " 括弧+αをハイライト
NeoBundle 'ujihisa/neco-look'			    " 英単語補完
NeoBundle 'vim-scripts/YankRing.vim'	    " ヤンク
NeoBundle 'nathanaelkane/vim-indent-guides' " インデント明示化
"=== 補完 (+luaが必要) ===
NeoBundle 'Shougo/neocomplete'
NeoBundle 'Shougo/neosnippet'
" NeoBundle 'Shougo/neosnippet-snippets'
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
" NeoBundleLazy 'Rip-Rip/clang_complete', {
" 			\ 'autoload':{ 'filetypes':[ 'c', 'cpp' ]} }
NeoBundleLazy 'osyo-manga/vim-marching', {
			\ 'autoload':{ 'filetypes':[ 'c', 'cpp' ]},
			\ 'depends' : ['Shougo/vimproc'] }
"=== Unity C# ==
" NeoBundleLazy 'nosami/Omnisharp', {
" 			\   'autoload': {'filetypes': ['cs']},
" 			\   'build': {
" 			\     'mac': 'xbuild server/OmniSharp.sln',
" 			\     'unix': 'xbuild server/OmniSharp.sln', }}
" NeoBundleLazy 'tpope/vim-dispatch', {
" 			\   'autoload': {'filetypes': ['cs']},
" 			\ }
"=== TeX ===
NeoBundleLazy 'LaTeX-Box-Team/LaTeX-Box', {
			\ 'autoload':{ 'filetypes':[ 'tex' ]}
			\ }
"=== Verilog ===
NeoBundleLazy 'vim-scripts/verilog.vim', {
			\ 'autoload':{ 'filetypes':[ 'verilog' ]}
			\ }
"=== JavaScript ===
NeoBundleLazy 'marijnh/tern_for_vim', {
			\ 'autoload':{ 'filetypes':[ 'javascript' ]},
			\ 'build': { 'others': 'npm install' } }
" NeoBundleLazy 'jelera/vim-javascript-syntax', {
" 		 	\ 'autoload':{ 'filetypes':[ 'javascript' ]} }
NeoBundleLazy 'pangloss/vim-javascript', {
		 	\ 'autoload':{ 'filetypes':[ 'javascript' ]} }
NeoBundleLazy 'jiangmiao/simple-javascript-indenter', {
		 	\ 'autoload':{ 'filetypes':[ 'javascript' ]} }
" NeoBundleLazy 'othree/html5-syntax.vim', {
" 			\ 'autoload': { 'filetypes': ['html']} }
" NeoBundle 'hallison/vim-markdown'
"=== Ruby ===
" NeoBundleLazy 'cespare/ruby-complete', {
" 			\'autoload':{'filetypes':[ 'ruby' ]} }
"=== Python ===
NeoBundleLazy 'davidhalter/jedi-vim', {
			\ 'autoload':{ 'filetypes':[ 'python' ]} } "sudo pip install jedi pep8 pyflakes
" NeoBundleLazy 'tmhedberg/SimpylFold', {
" 			\ 'autoload':{ 'filetypes':[ 'python' ]} }
"=== Golang ===
NeoBundle 'fatih/vim-go' "filetype認識のため、Lazyにするにはautocmdの必要あり
                         " :GoInstallBinarys を実行
" NeoBundleLazy 'fatih/vim-go', {
" 			\ 'autoload':{ 'filetypes':[ 'go' ]}
" 			\ }
" NeoBundle 'google/vim-ft-go' "補完機能等が含まれず
"=== Lisp ===
NeoBundleLazy 'luochen1990/rainbow', {
			\ 'autoload':{ 'filetypes':[ 'lisp' ]}
			\ }

NeoBundleCheck
call neobundle#end() "call function() はこれ以降でないと無効

filetype plugin indent on
syntax on


"####### Plugin : fatih/vim-go (Golang) #######
" set rtp+=$GOROOT/misc/vim " misc/vimは廃止された
" exe "set rtp+=".globpath($GOPATH, "src/github.com/nsf/gocode/vim")
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
let g:tex_conceal=''
" let g:tex_conceal='abgms'
" let g:tex_conceal='adbmgs'
setlocal conceallevel=2

autocmd BufWritePost *.tex :Latexmk
let g:LatexBox_output_type = 'pdf'
let g:LatexBox_latexmk_options = '-pdfdvi -recorder-'
let g:LatexBox_viewer = 'xdg-open'
let g:LatexBox_quickfix = 1
let g:LatexBox_autojump = 1
let g:LatexBox_complete_inlineMath = 1
let g:LatexBox_Folding = 1
let g:LatexBox_latexmk_async = 1

"####### Plugin : lightline #######
"ステータスライン 
"       \              [ 'fileencoding', 'filetype' ] ],
let g:lightline = {
	  \ 'colorscheme': 'Tomorrow_Night_Bright',
      \ 'active': {
	  \   'left': [ ['mode', 'paste'],
	  \     ['readonly', 'filename', 'modified'] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'fileencoding', 'filetype', 'syntastic'] ]
      \ },
	  \ 'component': {
	  \   'readonly': '%{&readonly?"R":"W"}',
	  \ },
      \ 'component_expand': {
      \   'syntastic': 'SyntasticStatuslineFlag',
      \ },
      \ 'component_type': {
      \   'syntastic': 'error',
      \ },
	  \ 'separator': {'left': '', 'right': ''},
	  \ 'subseparator': {'left': '|', 'right': '|'},
      \ }
" 保存時にsyntasticでチェックをしてから表示をアップデート
let g:syntastic_mode_map = { 'mode': 'passive' } "自動的には起動しない
" Syntastic Check Toggle
let s:syntastic_check_flag = 1
function! g:syntastic_toggle()
	if s:syntastic_check_flag
		let s:syntastic_check_flag = 0
	else
		let s:syntastic_check_flag = 1
	endif
endfunction
function! s:syntastic_check()
	if s:syntastic_check_flag
		SyntasticCheck
	else
		SyntasticReset
	endif
	call lightline#update()
endfunction
autocmd BufWritePost * call s:syntastic_check()
nnoremap <F11> :call g:syntastic_toggle()<CR>
"####### Plugin : syntastic #######
" let g:syntastic_auto_jump = 1
let g:syntastic_javascript_checkers = ['jshint']
" let g:syntastic_python_checkers = ['pylint']
let g:syntastic_python_checkers = ['pyflakes', 'pep8']
let g:syntastic_python_pep8_args = "--config=".expand("$HOME")."/.pep8"
let g:syntastic_error_symbol = ">>"
let g:syntastic_warning_symbol = ">>"

"####### Plugin : nerdtree #######
noremap <C-e> :NERDTreeToggle<CR>
let NERDTreeQuitOnOpen = 1 " 開いたら非表示
" let NERDTreeMapOpenInTab='<ENTER>' " デフォルトでタブで開く (フォルダ移動などはoを使用)

"####### Plugin : clever_f #######
" 大文字入力時のみ考慮
let g:clever_f_ignore_case = 1
let g:clever_f_smart_case = 1
" 日本語
autocmd FileType text let g:clever_f_use_migemo = 1
autocmd FileType tex let g:clever_f_use_migemo = 1
" 移動方向を修正
let g:clever_f_fix_key_direction = 1
" 記号の代用文字
let g:clever_f_chars_match_any_signs = ';'

"####### Plugin : quickhl.vim #######
let g:quickhl_manual_hl_priority = 10 " プライオリティの設定
" let g:quickhl_cword_enable_at_startup = 1 " カーソル下の単語を一時的にハイライト
" 色指定(同時に個数も指定)
let g:quickhl_manual_colors = [
	\ "gui=bold ctermbg=Cyan    ctermfg=Black guibg=#8CCBEA guifg=Black",
	\ "gui=bold ctermbg=Green   ctermfg=Black guibg=#A4E57E guifg=Black",
	\ "gui=bold ctermbg=Yellow  ctermfg=Black guibg=#FFDB72 guifg=Black",
	\ "gui=bold ctermbg=Red     ctermfg=Black guibg=#FF7272 guifg=Black",
	\ "gui=bold ctermbg=Magenta ctermfg=Black guibg=#FFB3FF guifg=Black",
	\ "gui=bold ctermbg=Blue    ctermfg=Black guibg=#9999FF guifg=Black",
\ ]
" ハイライトショートカット
nmap m <Plug>(quickhl-manual-this)
vmap m <Plug>(quickhl-manual-this)
" 表示トグル
nmap <F8> <Plug>(quickhl-manual-toggle)
" ハイライトを削除
nmap M <Plug>(quickhl-manual-reset)
vmap M <Plug>(quickhl-manual-reset)

"####### Plugin : caw.vim #######
" コメントアウトのショートカット
nmap \c <Plug>(caw:I:toggle)
vmap \c <Plug>(caw:I:toggle)
nmap \C <Plug>(caw:I:uncomment)
vmap \C <Plug>(caw:I:uncomment)

"####### Plugin : yankring.vim #######
let g:yankring_history_dir = $HOME.'/.vim'

"####### Plugin : marching #######
" let g:marching_clang_command = "clang-3.6"
let g:marching_clang_command = "clang"
let g:marching_enable_neocomplete = 1
" set updatetime=200
set updatetime=10
" let g:marching_backend = "sync_clang_command" "同期処理の場合
let g:marching_include_paths = filter(
	\	split(glob('/usr/include/c++/*'), '\n') +
	\	split(glob('/usr/include/*/c++/*'), '\n') +
	\	split(glob('/usr/include/*/'), '\n'),
	\	'isdirectory(v:val)')
"####### Plugin : clang_complete #######
" let g:clang_library_path = $HOME.'/dotfiles'
" let g:clang_library_path = $HOME.'/local/lib'
" let g:clang_library_path = '/usr/lib/llvm-3.4/lib'
" clang_complete では自動補完を行わない用に設定
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

"####### Plugin : neocomplete #######
let g:neocomplete#enable_at_startup = 1				" neocompleteを有効
let g:neocomplete#enable_auto_select = 0			" 候補を自動選択しない
let g:neocomplete#auto_completion_stairt_length = 2	" 補完が自動で開始される文字数
" let g:neocomplete#skip_auto_completion_time = 0
let g:neocomplete#enable_ignore_case = 1			" 大文字小文字を無視
let g:neocomplete#enable_smart_case = 1				" (ただし大文字入力時のみ考慮)
let g:neocomplete#enable_underbar_completion = 0	" アンダーバー補完を有効
let g:neocomplete#sources#syntax#min_keyword_length = 3 " シンタックスをキャッシュするときの最小文字数
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'	" ロックパターン
call neocomplete#custom#source('_', 'sorters', ['sorter_length']) " ソート
let g:neocomplete#enable_auto_close_preview = 0			" プレビューウインドウを閉じない
" 辞書設定
let g:neocomplete#sources#dictionary#dictionaries = { 'default' : '', 'vimshell' : $HOME.'/.vimshell_hist', 'scheme' : $HOME.'/.gosh_completions' }
" 補完するためのキーワードパターン
if !exists('g:neocomplete#keyword_patterns')
	let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*' "日本語を補完候補として取得しない

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
let g:neocomplete#force_omni_input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#force_omni_input_patterns.objc = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.objcpp = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'

"####### Plugin : Eclim #######
" autocmd FileType java noremap \f :JavaFormat<CR>
" autocmd FileType java let g:EclimCompletionMethod = 'omnifunc'
" autocmd FileType java let g:neocomplete#force_omni_input_patterns.java = '\k\.\k*'

"####### Plugin : neosnippet #######
"標準のsnippetを消したら、初めて挿入モードになった時にエラー(直ぐ消える)
" For snippet_complete marker
if has('conceal')
set conceallevel=2 concealcursor=i
endif
" スニペットファイルの保存ディレクトリのパスを登録
let g:neosnippet#snippets_directory='~/.vim/bundle/my-vim-snippets/snippets'
let g:neosnippet#enable_preview = 1

"===== Color Scheme =====
colorscheme tango_lx

"===== GUIタブの表示設定 =====
function! GuiTabLabel() " 個別に設定
	let l:label = ''
	let l:bufnrlist = tabpagebuflist(v:lnum) "タブに含まれるバッファ(ウィンドウ)情報を取得
	" 表示文字列にバッファ名中のファイル名を追加
	let l:bufname = fnamemodify(bufname(l:bufnrlist[tabpagewinnr(v:lnum) - 1]), ':t')
	let l:label .= l:bufname == '' ? 'No title' : l:bufname "バッファ名がなければNo title
	let l:wincount = tabpagewinnr(v:lnum, '$') "タブ内にウィンドウが複数あるときにはその数を追加
	if l:wincount > 1
		let l:label .= '[' . l:wincount . ']'
	endif
	for bufnr in l:bufnrlist "変更のあるバッファがるときには '[+]' を追加
		if getbufvar(bufnr, "&modified")
			let l:label .= '[+]'
			break
		endif
	endfor
	return l:label
endfunction
" guitablabelに上の関数を設定
set guitablabel=%N:\ %{GuiTabLabel()}

"===== engdict (http://d.hatena.ne.jp/aki-yam/20080629/1214757485) =====
function! EngDict()
    sp +enew | put = system('engdict ' . @*)
    set bufhidden=hide noswapfile noro nomodified
	normal gg
endfunction  
vnoremap <silent> <c-d> :call EngDict()<CR>


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
