" ==============================================================================
"                                    dps-asyngrep
" ==============================================================================
" Check search binary
if !executable('rg')
   echomsg  'ripgrep is not installed (for :Agp)'
endif

" Shortcut
map <F3> :Agp<CR>
