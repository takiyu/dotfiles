" ==============================================================================
"                                  comfortable-motion
" ==============================================================================
let g:comfortable_motion_no_default_key_mappings = 1
let g:comfortable_motion_friction = 120.0
let g:comfortable_motion_air_drag = 3.0
" Large motion
nnoremap <silent> <C-d> :call comfortable_motion#flick(120)<CR>
nnoremap <silent> <C-u> :call comfortable_motion#flick(-120)<CR>
noremap <silent> <PageDown> :call comfortable_motion#flick(120)<CR>
noremap <silent> <PageUp>   :call comfortable_motion#flick(-120)<CR>
" Medium motion
nnoremap <silent> <Space>   :call comfortable_motion#flick(100)<CR>
nnoremap <silent> <S-Space> :call comfortable_motion#flick(-100)<CR>
" Small motion
noremap <silent> <ScrollWheelDown> :call comfortable_motion#flick(40)<CR>
noremap <silent> <ScrollWheelUp>   :call comfortable_motion#flick(-40)<CR>
