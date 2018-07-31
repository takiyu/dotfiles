" ==============================================================================
"                                  comfortable-motion
" ==============================================================================
let g:comfortable_motion_no_default_key_mappings = 1
nnoremap <silent> <C-d> :call comfortable_motion#flick(100)<CR>
nnoremap <silent> <C-u> :call comfortable_motion#flick(-100)<CR>
noremap <silent> <PageDown> :call comfortable_motion#flick(100)<CR>
noremap <silent> <PageUp>   :call comfortable_motion#flick(-100)<CR>
noremap <silent> <ScrollWheelDown> :call comfortable_motion#flick(40)<CR>
noremap <silent> <ScrollWheelUp>   :call comfortable_motion#flick(-40)<CR>
let g:comfortable_motion_friction = 80.0
let g:comfortable_motion_air_drag = 2.0
