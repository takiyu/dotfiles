" ==============================================================================
"                                  vim-ref
" ==============================================================================

" Webdict sources
let g:ref_source_webdict_sites = {
\   'alc': {
\     'url': 'https://eow.alc.co.jp/search?q=%s',
\   },
\   'longman': {
\     'url': 'http://www.ldoceonline.com/search/?q=%s',
\   },
\   'wiki': {
\     'url': 'http://ja.wikipedia.org/wiki/%s',
\   },
\ }
let g:ref_source_webdict_sites.default = 'alc'

function! s:replace(text, src, dst)
  return join(split(a:text, a:src), a:dst)
endfunction

" Filtering functions for viewing
function! g:ref_source_webdict_sites.alc.filter(output)
  return join(split(a:output, "\n")[88 :], "\n")
endfunction
function! g:ref_source_webdict_sites.longman.filter(output)
  return join(split(a:output, "\n")[8 :], "\n")
endfunction
function! g:ref_source_webdict_sites.wiki.filter(output)
  return join(split(a:output, "\n")[17 :], "\n")
endfunction

" Selection utility function
function! s:GetVisualSelection() abort
    try
        let a_save = @a
        silent! normal! gv"ay
        echo @a
        return @a
    finally
        let @a = a_save
    endtry
endfunction

" Key binding for selecting word
function! g:RefSelectingWord(source, query)
    let word = s:GetVisualSelection()
    call ref#open(a:source, a:query . ' ' . word)
endfunction
vnoremap <silent><c-d> :call g:RefSelectingWord('webdict', 'alc')<CR>
vnoremap <silent><c-d><c-d> :call g:RefSelectingWord('webdict', 'longman')<CR>
