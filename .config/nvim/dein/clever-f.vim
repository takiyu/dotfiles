" ==============================================================================
"                                  clever-f
" ==============================================================================
" 大文字入力時のみ考慮
let g:clever_f_ignore_case = 1
let g:clever_f_smart_case = 1
" 日本語
autocmd FileType text let g:clever_f_use_migemo = 1
autocmd FileType tex let g:clever_f_use_migemo = 1
autocmd FileType plaintex let g:clever_f_use_migemo = 1
" 移動方向を修正
let g:clever_f_fix_key_direction = 1
" 記号の代用文字
let g:clever_f_chars_match_any_signs = ';'
