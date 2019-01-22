" ==============================================================================
"                                    gtags
" ==============================================================================
let g:gen_tags#use_cache_dir = 0  " Create tag files in .git directory
let g:gen_tags#blacklist = ['$HOME']

" Tag generation
let g:gen_tags#ctags_auto_gen = 1
let g:gen_tags#gtags_auto_gen = 1
if executable("gtags")
    autocmd BufWritePost *.cpp,*.cc,*.c :GenGTAGS
endif
if executable("ctags")
    autocmd BufWritePost *.cpp,*.cc,*.c :GenCtags
endif

" Key mapping
let g:gen_tags#gtags_default_map = 0
if executable("gtags")
    " Jump to definition
    nmap <Leader>d :cs find g <C-R>=expand('<cword>')<CR><CR>
    " Jump to reference
    nmap <Leader>r :cs find c <C-R>=expand('<cword>')<CR><CR>
elseif executable("ctags")
    nmap <Leader>d <C-]>
endif
