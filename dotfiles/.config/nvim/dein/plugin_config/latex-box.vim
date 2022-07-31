" ==============================================================================
"                                  LaTeX-Box
" ==============================================================================
let g:tex_flavor = 'latex'
let g:tex_conceal=''
" let g:tex_conceal='abgms'
" let g:tex_conceal='adbmgs'

" autocmd BufWritePost *.tex :Latexmk
" autocmd BufWritePost *.tex :!latexmk
let g:LatexBox_personal_latexmkrc = 1  " To disable g:LatexBox_output_type
" let g:LatexBox_output_type = 'pdf'
" let g:LatexBox_latexmk_options = ''
let g:LatexBox_viewer = 'xdg-open'
let g:LatexBox_quickfix = 2  " cursor stays in the main window
let g:LatexBox_autojump = 0
let g:LatexBox_complete_inlineMath = 1
let g:LatexBox_Folding = 1
let g:LatexBox_latexmk_async = 1
let g:LatexBox_build_dir = "build"  " common with .latexmkrc
