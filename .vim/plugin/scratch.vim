" Provide a :Scratch command to view output of any command in a scratch
" buffer

if exists('g:loaded_scratch')
  finish
endif
let g:loaded_scratch = 1

command! -nargs=1 -complete=command Scratch call scratch#open(<q-args>, <q-mods>)
command! -nargs=? Marks call scratch#open('marks ' . <q-args>, <q-mods>)
command! -nargs=0 Messages call scratch#open('messages', <q-mods>)
command! -nargs=? Registers call scratch#open('registers ' . <q-args>, <q-mods>)
command! -nargs=? Display call scratch#open('display ' . <q-args>, <q-mods>)
