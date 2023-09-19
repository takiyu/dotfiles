local vimscript_code = [[
" ==============================================================================
"                                  comfortable-motion
" ==============================================================================
let g:comfortable_motion_no_default_key_mappings = 1
let g:comfortable_motion_friction = 120.0
let g:comfortable_motion_air_drag = 3.0
let g:comfortable_motion_scroll_down_key = "\<C-d>"  " Move cursor too
let g:comfortable_motion_scroll_up_key = "\<C-u>"
" Large motion
nmap <silent> <Space>   :call comfortable_motion#flick(120)<CR>
nmap <silent> <S-Space> :call comfortable_motion#flick(-120)<CR>

nmap <silent> <PageUp>     :call comfortable_motion#flick(-120)<CR>
nmap <silent> <PageDown>   <Space>
" nmap <silent> <PageDown> :call comfortable_motion#flick(120)<CR>

nmap <silent> <S-Up>       :call comfortable_motion#flick(-120)<CR>
nmap <silent> <S-Down>     <Space>
" nmap <silent> <S-Down>   :call comfortable_motion#flick(120)<CR>

" Small motion
noremap <silent> <ScrollWheelDown> :call comfortable_motion#flick(40)<CR>
noremap <silent> <ScrollWheelUp>   :call comfortable_motion#flick(-40)<CR>
]]

return {}
