(let [items [" "
             "%{luaeval(\"require'statusline'.obsession()\")}"
             "%<%f %{&filetype !=# '' ? '[' . &filetype . '] ' : ''}"
             "%{luaeval(\"require'statusline'.lsp()\")}"
             "%m%r%="
             "%{luaeval(\"require'statusline'.diagnostics()\")}"
             "%{&fileformat != 'unix' ? '[' . &fileformat . '] ' : ''}"
             "%{&fileencoding != 'utf-8' && &fileencoding != '' ? '[' . &fileencoding . '] ' : ''}"
             "%10.(%l:%c%V%)%6.P"
             " "]]
  (set vim.o.statusline (table.concat items)))
