" ==============================================================================
"                                  Entry Point
" ==============================================================================

filetype on

" ===== Dein settings =====
" Set dein cache path and dein config dir
let g:dein_cache_path = expand('~/.cache/nvim/dein/')
let g:dein_config_dir = expand('~/.config/nvim/dein/')

" Set dein.vim directory
let s:dein_dir = g:dein_cache_path . 'repos/github.com/Shougo/dein.vim'

" Clone dein.vim if it does not exist
if !isdirectory(s:dein_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_dir
endif

" Add dein.vim to runtime path
execute 'set runtimepath+=' . fnamemodify(s:dein_dir, ':p')

" Load plugins
if dein#load_state(g:dein_cache_path)
    call dein#begin(g:dein_cache_path)

    " Basic plugins
    call dein#load_toml(g:dein_config_dir . 'dein.toml', {'lazy': 0})
    " Lazy plugins
    call dein#load_toml(g:dein_config_dir . 'dein_lazy.toml', {'lazy': 1})

    call dein#end()
    call dein#save_state()
endif

" Install plugins
if dein#check_install()
    call dein#install()
endif

" Install Python3 for nvim
if has("nvim") && !has("python3")
    echo 'install neovim python3'
    call system('sudo pip install neovim')
endif

syntax on
filetype plugin indent on

" ===== Common settings =====
execute 'source' g:dein_config_dir . 'common.vim'

" ===== Color Scheme =====
colorscheme tango_lx
