" ==============================================================================
"                               GUI Settings for Neovim-Qt
" ==============================================================================

" Tabline menu
GuiTabline 0

" Popup menu
GuiPopupmenu 0

" Kill the extra empty buffer
if @% == ""
    bd
endif

" Font
let s:gui_font_size = 11
let s:gui_font_name = "DejavuNotoNerdPowerline"
" let s:gui_font_name = "DejaVu Sans Mono"
function! SetGuiFont()
    :execute "Guifont! " . s:gui_font_name . ":h" . s:gui_font_size
    GuiLinespace 1
endfunction
call SetGuiFont()  " Set initial font
" Font size changing
function! ChangeGuiFontSize(step)
    let s:gui_font_size += a:step
    call SetGuiFont()
endfunction
noremap <C-+> :call ChangeGuiFontSize(1)<CR>
noremap <C--> :call ChangeGuiFontSize(-1)<CR>
noremap <C-=> :call ChangeGuiFontSize(1)<CR>
