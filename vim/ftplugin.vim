if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

command! -buffer GwRainbow call s:ToggleRainbow()

function! s:ToggleRainbow()
  let g:gw_rainbow = !get(g:, 'gw_rainbow', 1)
  set syntax=gl
endfunction

nnoremap <buffer> <LocalLeader>r :GwRainbow<CR>
