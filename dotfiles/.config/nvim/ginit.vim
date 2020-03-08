" ==============================================================================
"                               GUI Settings for Neovim-Qt
" ==============================================================================

" Font
Guifont! DejaVu Sans Mono:h11
GuiLinespace 1

" Tabline menu
GuiTabline 0

" Popup menu
GuiPopupmenu 0

" Kill the extra empty buffer
if @% == ""
    bd
endif
