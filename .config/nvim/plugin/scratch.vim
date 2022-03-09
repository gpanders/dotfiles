if exists('g:loaded_scratch')
  finish
endif
let g:loaded_scratch = 1

command! -nargs=1 -complete=command Scratch call scratch#open(<q-args>, <q-mods>)
command! -nargs=? Marks <mods> Scratch marks <args>
command! -nargs=0 Messages <mods> Scratch messages
command! -nargs=? Registers <mods> Scratch registers <args>
command! -nargs=? Display <mods> Scratch display <args>
command! -nargs=? -complete=highlight Highlight <mods> Scratch highlight <args>
command! -nargs=0 Jumps <mods> Scratch jumps
command! -nargs=0 Changes <mods> Scratch changes
command! -nargs=0 Digraphs <mods> Scratch digraphs
command! -nargs=0 Scriptnames <mods> Scratch scriptnames
