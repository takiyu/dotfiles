local vimscript_code = [[
" ==============================================================================
"                                    fern
" ==============================================================================
" Disable default mappings
let g:fern#disable_default_mappings = 1
" Use icons
let g:fern#renderer = 'nerdfont'

" Global key maps
noremap <C-e> :Fern . -drawer -toggle<CR>
noremap <C-f> :FernDo close -drawer<CR>:Fern . -drawer -reveal=% -wait<CR>
nnoremap <Plug>(fern-close-drawer) :<C-u>FernDo close -drawer -stay<CR>

function! s:init_fern() abort
    " Define NERDTree like mappings in fern buffer
    nmap <buffer> o <Plug>(fern-action-open:edit)<Plug>(fern-close-drawer)
    nmap <buffer> O <Plug>(fern-action-open:edit)<C-w>p
    nmap <buffer> t <Plug>(fern-action-open:tabedit)gT<Plug>(fern-close-drawer)gt
    nmap <buffer> T <Plug>(fern-action-open:tabedit)gT
    nmap <buffer> i <Plug>(fern-action-open:split)<Plug>(fern-close-drawer)
    nmap <buffer> s <Plug>(fern-action-open:vsplit)<Plug>(fern-close-drawer)
    nmap <buffer> S <Plug>(fern-action-open:vsplit)<C-w>p

    nmap <buffer> l <Plug>(fern-action-expand:in)
    nmap <buffer> h <Plug>(fern-action-collapse)

    nmap <buffer> u <Plug>(fern-action-leave)
    nmap <buffer> U <Plug>(fern-action-leave)
    nmap <buffer> r <Plug>(fern-action-reload)
    nmap <buffer> R gg<Plug>(fern-action-reload)<C-o>
    nmap <buffer> cd <Plug>(fern-action-cd)
    nmap <buffer> CD gg<Plug>(fern-action-cd)<C-o>
    nmap <buffer> I <Plug>(fern-action-hidden:toggle)
    nmap <buffer> <C-h> <Plug>(fern-action-hidden:toggle)
    nmap <buffer> q :<C-u>quit<CR>
    nmap <buffer> ? <Plug>(fern-action-help)

    " Enter key to open or expand
    nmap <buffer><silent><expr>
            \ <Plug>(fern-my-open-or-expand)
            \ fern#smart#leaf(
            \   "\<Plug>(fern-action-open)\<Plug>(fern-close-drawer)",
            \   "\<Plug>(fern-action-expand)",
            \ )
    nmap <buffer><nowait><CR> <Plug>(fern-my-open-or-expand)

    " Apply color to icons
    call glyph_palette#apply()
endfunction
autocmd FileType fern call s:init_fern()
]]

return {
  {'lambdalisue/fern.vim',
-- --    config = function()
-- --     vim.api.nvim_exec(vimscript_code, true)
-- --    end
  },
  {'lambdalisue/glyph-palette.vim'},
  {'lambdalisue/nerdfont.vim'},
--   {'lambdalisue/fern-renderer-nerdfont.vim'},
}
