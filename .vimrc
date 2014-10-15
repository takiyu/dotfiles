if has('vim_starting')
set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

"NeoBundleFetch 'Shougo/neobundle.vim'
"
"フォント設定
if has('win32') || has('win64')
    " Windows
	set guifont=MS_Gothic:h13
else
    " Windows以外
	set guifont=DejaVu\ Sans\ Mono\ 10
" 	set guifont=DejaVu\ Sans\ Mono\ 13
	" set lsp=4 " gvimでの行間
endif
" 無限Undo
if has('persistent_undo')
    set undodir=~/.vimundo " ~/.vim/undo
    set undofile
endif
set backspace=indent,eol,start " インサートモード時にバックスペースを使う
set whichwrap=b,s,h,l,<,>,[,] " 行頭から前行文末へ移動可能にする
" set scrolloff=999 " スクロール時にカーソルを中央へ移動
set scrolloff=3 " スクロールを開始する行数
set cindent " cオートインデント
set cinoptions=g0 " cppでのpublic宣言を下げる
set tabstop=4 " タブを表示するときの幅
set shiftwidth=4 " タブを挿入するときの幅
" set showtabline=2 " タブ(上部)を常に表示する
setlocal noexpandtab "　空白文字ではなくタブ文字を使用する
autocmd FileType neosnippet set noexpandtab "効いていない？
set mouse=a " マウス
set ttymouse=xterm2
set number " 行数を表示する
" set hlsearch " 検索文字列を強調
set ignorecase " 大文字小文字を無視 ただし大文字入力時のみ考慮
set smartcase
set guioptions-=m " メニューバー,ツールバーを非表示
set guioptions-=T
" 折りたたみの設定
set foldmethod=syntax
set foldlevel=0
set foldcolumn=2
" 補完時の設定
" プレビューウインドウで表示
set completeopt=menuone,longest,preview
" set completeopt=menuone
" プレビューウインドウの高さ
set previewheight=1
" 下に表示
set splitbelow
" ステータスラインを常に表示
set laststatus=2
" 保存時に行末の空白を除去する
" autocmd BufWritePre * :%s/\s\+$//ge

"##########keybind###########
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
" omni補完
inoremap <C-o> <C-x><C-o>
" クリップボードから貼り付け,コピー
" nnoremap <C-v> "+gp
inoremap <C-v> <ESC>"+gp
vnoremap <C-c> "+y
" 折り込みショートカット
" nnoremap <C-c> zc
" inoremap <C-c> <Esc>zc
" PreviewWindowの非表示
nnoremap <C-c> <C-w>z
inoremap <C-c> <C-w>z
"補完のキーバインド
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
imap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"
" Enterで補完を決定、または次へジャンプ
imap <expr><CR> pumvisible() ? "\<Plug>(neosnippet_jump_or_expand)" : neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" :"\<CR>"
" Tabで選択
imap <expr><TAB> pumvisible() ? "\<C-n>" : neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
" Escで補完ポップアップを閉じて標準モード(続けて書く方法がわからなかった)
inoremap <expr><Esc> pumvisible() ? neocomplete#close_popup() ? "<Esc>" : "<Esc>" : "<Esc>"
" 検索文字列のハイライトが有効なら解除
" noremap <expr><Esc><Esc> v:hlsearch == 1 ? ":nohlsearch<CR>" : "<Esc>"


"#############add plugins ##############
"#共通#
NeoBundle 'vim-scripts/muzzl.vim' "カラースキーム
NeoBundle 'tyru/caw.vim' "コメントアウト補助
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'scrooloose/syntastic' "文法チェック
NeoBundle 'rhysd/clever-f.vim'
NeoBundle 'itchyny/lightline.vim' "ステータスライン
NeoBundle 't9md/vim-quickhl' "ハイライト
NeoBundle 'vimtaku/hl_matchit.vim.git' "括弧+αをハイライト
NeoBundle 'takiyu/ibus-switcher.vim' "ibus
"#補完# (+luaが必要)
NeoBundle 'Shougo/neocomplete'
NeoBundle 'Shougo/neosnippet'
" NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'takiyu/my-vim-snippets'

"#C/C++#
"シンタックスハイライト
NeoBundleLazy 'vim-jp/cpp-vim', {
			\ 'autoload':{ 'filetypes':[ 'cpp' ]} }
NeoBundleLazy 'Rip-Rip/clang_complete', {
			\ 'autoload':{ 'filetypes':[ 'c', 'cpp' ]} }
" NeoBundle 'Shougo/vimproc', {
" 	\ 'build' : {
" 	\     'mac' : 'make -f make_mac.mak',
" 	\     'unix' : 'make -f make_unix.mak' } }
" NeoBundleLazy 'osyo-manga/vim-marching', {
" 			\ 'autoload':{ 'filetypes':[ 'c', 'cpp' ]},
" 			\ 'depends' : ['Shougo/vimproc', 'osyo-manga/vim-reunions'] }
" "#Unity c#
NeoBundleLazy 'nosami/Omnisharp', {
			\   'autoload': {'filetypes': ['cs']},
			\   'build': {
			\     'mac': 'xbuild server/OmniSharp.sln',
			\     'unix': 'xbuild server/OmniSharp.sln', }}
NeoBundleLazy 'tpope/vim-dispatch', {
			\   'autoload': {'filetypes': ['cs']},
			\ }
"#TeX#
NeoBundleLazy 'LaTeX-Box-Team/LaTeX-Box', {
			\ 'autoload':{ 'filetypes':[ 'tex' ]}
			\ }
"#Verilog#
NeoBundleLazy 'vim-scripts/verilog.vim', {
			\ 'autoload':{ 'filetypes':[ 'verilog' ]}
			\ }
"#JavaScript, HTML#
"シンタックスハイライト
NeoBundleLazy 'mattn/jscomplete-vim', {
			\ 'autoload':{ 'filetypes':[ 'javascript' ]} }
NeoBundleLazy 'jelera/vim-javascript-syntax', {
		 	\ 'autoload':{ 'filetypes':[ 'javascript' ]} }
NeoBundleLazy 'othree/html5-syntax.vim', {
			\ 'autoload': { 'filetypes': ['html']} }
" NeoBundle 'hallison/vim-markdown'
"#Ruby#
" NeoBundleLazy 'cespare/ruby-complete', {
" 			\'autoload':{'filetypes':[ 'ruby' ]} }


"###########matchit, hl-matchit settings##########
source $VIMRUNTIME/macros/matchit.vim "括弧を追加
let g:hl_matchit_enable_on_vim_startup = 1 "ハイライトを有効

"###########golang settings##########
" NeoBundle 'Blackrush/vim-gocode'
set rtp+=$GOROOT/misc/vim
exe "set rtp+=".globpath($GOPATH, "src/github.com/nsf/gocode/vim")

"###########plugin:jscomplete##########
let g:jscomplete_use = ['dom', 'moz', 'xpcom']

"##########plugin:OmniSharp##########
let g:OmniSharp_host = "http://localhost:2000"
let g:OmniSharp_typeLookupInPreview = 1

"###########plugin:latex-box##########
let g:tex_conceal=''
autocmd BufWritePost *.tex :Latexmk
let g:LatexBox_output_type = 'pdf'
let g:LatexBox_latexmk_options = '-pdfdvi -recorder-'
let g:LatexBox_viewer = 'xdg-open'
let g:LatexBox_quickfix = 1
let g:LatexBox_autojump = 1
let g:LatexBox_complete_inlineMath = 1
let g:LatexBox_Folding = 1

"###########plugin:ibus-switcher.vim##########
let g:ibus_switcher_default_engine = 'xkb:jp::jpn'
"tex、texのみで動作
autocmd InsertEnter *.tex call ibus_switcher#load()
autocmd InsertLeave *.tex call ibus_switcher#save()
autocmd insertleave *.tex call ibus_switcher#loadDefault()
autocmd InsertEnter *.txt call ibus_switcher#load()
autocmd InsertLeave *.txt call ibus_switcher#save()
autocmd insertleave *.txt call ibus_switcher#loadDefault()

"##########plugin:lightline##########
"ステータスライン 
"		\ 'colorscheme': 'wombat',
"       \              [ 'fileencoding', 'filetype', 'syntastic'] ]
let g:lightline = {
      \ 'active': {
	  \   'left': [ ['mode', 'paste'],
	  \     ['readonly', 'filename', 'modified'] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'fileencoding', 'filetype' ] ],
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
" let g:syntastic_mode_map = { 'mode': 'passive' } "自動的には起動しない
" autocmd BufWritePost * call s:syntastic_check()
" function! s:syntastic_check()
" 	SyntasticCheck
" 	call lightline#update()
" endfunction
"##########plugin:syntastic##########
let g:syntastic_auto_jump = 1

"##########plugin:nerdtree##########
noremap <C-e> :NERDTreeToggle<CR>
let NERDTreeQuitOnOpen = 1 " 開いたら非表示
" let NERDTreeMapOpenInTab='<ENTER>' " デフォルトでタブで開く (フォルダ移動などはoを使用)

"##########plugin:clever_f##########
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

"##########plugin:quickhl.vim##########
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

"##########plugin:caw.vim##########
" コメントアウトのショートカット
nmap \c <Plug>(caw:I:toggle)
vmap \c <Plug>(caw:I:toggle)
nmap \C <Plug>(caw:I:uncomment)
vmap \C <Plug>(caw:I:uncomment)

"##########plugin:marching##########
" let g:marching_enable_neocomplete = 1
" set updatetime=200
"##########plugin:clang_complete##########
" let g:clang_library_path = $HOME.'/dotfiles'
" let g:clang_library_path = $HOME.'/local/lib'
let g:clang_library_path = '/usr/lib/llvm-3.4/lib'
" clang_complete では自動補完を行わない用に設定
let g:clang_complete_auto = 0
let g:clang_auto_select = 0
let g:clang_sort_patterns = 'none'
"##########plugin:neocomplete##########
let g:neocomplete#enable_at_startup = 1 " neocompleteを有効
let g:neocomplete#auto_completion_stairt_length = 3 " 補完が自動で開始される文字数
" let g:neocomplete#skip_auto_completion_time = 0
let g:neocomplete#enable_ignore_case = 1 "大文字が入力されるまで区別無視
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#enable_underbar_completion = 0 " アンダーバー補完を有効
let g:neocomplete#sources#syntax#min_keyword_length = 3 " シンタックスをキャッシュするときの最小文字数
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*' " ロックパターン
call neocomplete#custom#source('_', 'sorters', ['sorter_length']) " ソート
let g:neocomplete#enable_auto_close_preview = 0 " プレビューウインドウを閉じない

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = { 'default' : '', 'vimshell' : $HOME.'/.vimshell_hist', 'scheme' : $HOME.'/.gosh_completions' }

"補完するためのキーワードパターンを指定
if !exists('g:neocomplete#keyword_patterns')
	let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*' "日本語を補完候補として取得しない

" omni補完
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd FileType cpp set omnifunc=cppcomplete#Complete
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType typescript setlocal omnifunc=TSScompleteFunc
autocmd FileType cs set omnifunc=OmniSharp#Complete
autocmd FileType ruby,eruby setlocal omnifunc=rubycomplete#Complete
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1

" heavy omni補完
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.cs = '.*[^=\);]'
let g:neocomplete#sources#omni#input_patterns.typescript = '.*[^=\);]'
let g:neocomplete#sources#omni#input_patterns.javascript = '[^. *\t]\.\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.objc = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplete#sources#omni#input_patterns.objcpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.go = '[^.[:digit:] *\t]\.\w*'

" clang系 と併用して使用する場合は以下の設定も行う
if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_overwrite_completefunc = 1
let g:neocomplete#force_omni_input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#force_omni_input_patterns.objc = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.objcpp = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'

"##########plugin:neosnippet####################
"標準のsnippetを消したら、初めて挿入モードになった時にエラー(直ぐ消える)
" For snippet_complete marker.
if has('conceal')
set conceallevel=2 concealcursor=i
endif
" スニペットファイルの保存ディレクトリのパスを登録
let g:neosnippet#snippets_directory='~/.vim/bundle/my-vim-snippets/snippets'
let g:neosnippet#enable_preview = 1

filetype plugin indent on
syntax on

NeoBundleCheck

"##########colorscheme####################
colorscheme muzzl

hi Normal		guifg=#e4e4ef ctermbg=gray
hi Folded		guifg=#eeeeec guibg=#555753 ctermfg=Black ctermbg=gray
hi FoldColumn	guifg=#fce94f guibg=#2e3436 ctermfg=3 ctermbg=Black cterm=bold
hi Statement    guifg=#fce94f gui=bold cterm=bold
hi Type			guifg=#8ae234 gui=bold cterm=bold
hi Identifier	cterm=bold
hi Constant		ctermfg=3
hi Comment		ctermfg=blue cterm=bold

hi PreProc		guifg=#eeeeec cterm=bold " generic Preprocessor
hi Include		guifg=#eeeeec  " #include
hi Define		guifg=#eeeeec  " #define
hi Macro		guifg=#eeeeec  " same as Define
hi PreCondit	guifg=#eeeeec gui=bold " #if, #else, #endif, section(tex)
