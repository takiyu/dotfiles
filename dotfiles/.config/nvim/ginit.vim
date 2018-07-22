" ==============================================================================
"                               GUI Settings for Neovim-Qt
" ==============================================================================

" Font
Guifont! DejaVu Sans Mono:h11
GuiLinespace 1

" Kill the extra empty buffer
if @% == ""
    bd
endif
