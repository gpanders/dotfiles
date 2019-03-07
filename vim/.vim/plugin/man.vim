" Define Man command if it doesn't already exist
if exists(":Man") == 2
  finish
endif

command -nargs=+ -complete=shellcmd Man call man#get_page(<q-mods>, <f-args>)
nnoremap <Plug>(ManPreGetPage) :call man#pre_get_page(0)<CR>
nmap <Leader>K <Plug>(ManPreGetPage)
