" ==============================================================================
"                                  Entry Point
" ==============================================================================

filetype on

" ===== Dein settings =====
let g:dein#auto_recache = 1   " Enable auto re-cache
let g:dein#lazy_rplugins = 1  " Enable lazy load of remote plugins

" Set dein cache path and dein config dir
let g:dein_cache_path = expand('~/.cache/nvim/dein/')
let g:dein_config_dir = expand('~/.config/nvim/dein/')
let g:dein_plugin_config_dir = expand('~/.config/nvim/dein/plugin_config/')

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

" Check python3
if has("nvim") && !has("python3")
    echo 'Python3 and neovim package is needed, but not installed'
endif

syntax on
filetype plugin indent on

" ===== Common settings =====
execute 'source' g:dein_config_dir . 'common.vim'

" ===== Color Scheme =====
colorscheme tango_lx
