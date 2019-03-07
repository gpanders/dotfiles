" Man command autoloads
" Modified By: Greg Anders <greg@gpanders.com>
" Original Author: SungHyun Nam <goweol@gmail.com>
" Date: 2019-03-06

let s:man_tag_depth = 0

let s:man_sect_arg = ""
let s:man_find_arg = "-w"
try
  if !has("win32") && $OSTYPE !~ 'cygwin\|linux' && system('uname -s') =~ "SunOS" && system('uname -r') =~ "^5"
    let s:man_sect_arg = "-s"
    let s:man_find_arg = "-l"
  endif
catch /E145:/
  " Ignore the error in restricted mode
endtry

function! man#pre_get_page(cnt)
  if a:cnt == 0
    let old_isk = &iskeyword
    if &ft == 'man'
      setlocal iskeyword+=(,)
    endif
    let str = expand("<cword>")
    let &l:iskeyword = old_isk
    let page = substitute(str, '(*\(\k\+\).*', '\1', '')
    let sect = substitute(str, '\(\k\+\)(\([^()]*\)).*', '\2', '')
    if match(sect, '^[0-9 ]\+$') == -1
      let sect = ""
    endif
    if sect == page
      let sect = ""
    endif
  else
    let sect = a:cnt
    let page = expand("<cword>")
  endif
  call man#get_page(sect, page)
endfunction

function! man#pop_page()
  if s:man_tag_depth > 0
    let s:man_tag_depth = s:man_tag_depth - 1
    exec "let s:man_tag_buf=s:man_tag_buf_".s:man_tag_depth
    exec "let s:man_tag_lin=s:man_tag_lin_".s:man_tag_depth
    exec "let s:man_tag_col=s:man_tag_col_".s:man_tag_depth
    exec s:man_tag_buf."b"
    exec s:man_tag_lin
    exec "norm! ".s:man_tag_col."|"
    exec "unlet s:man_tag_buf_".s:man_tag_depth
    exec "unlet s:man_tag_lin_".s:man_tag_depth
    exec "unlet s:man_tag_col_".s:man_tag_depth
    unlet s:man_tag_buf s:man_tag_lin s:man_tag_col
  endif
endfunction

function! s:GetCmdArg(sect, page)
  if a:sect == ''
    return a:page
  endif
  return s:man_sect_arg.' '.a:sect.' '.a:page
endfunction

function! s:FindPage(sect, page)
  let where = system("man ".s:man_find_arg.' '.s:GetCmdArg(a:sect, a:page))
  if where !~ "^/"
    if matchstr(where, " [^ ]*$") !~ "^ /"
      return 0
    endif
  endif
  return 1
endfunction

function! man#get_page(cmdmods, ...)
  if a:0 >= 2
    let sect = a:1
    let page = a:2
  elseif a:0 >= 1
    let sect = ""
    let page = a:1
  else
    return
  endif

  " To support:	    nmap K :Man <cword>
  if page == '<cword>'
    let page = expand('<cword>')
  endif

  if sect != "" && s:FindPage(sect, page) == 0
    let sect = ""
  endif
  if s:FindPage(sect, page) == 0
    echo "\nCannot find a '".page."'."
    return
  endif
  exec "let s:man_tag_buf_".s:man_tag_depth." = ".bufnr("%")
  exec "let s:man_tag_lin_".s:man_tag_depth." = ".line(".")
  exec "let s:man_tag_col_".s:man_tag_depth." = ".col(".")
  let s:man_tag_depth = s:man_tag_depth + 1

  " Use an existing "man" window if it exists, otherwise open a new one.
  if &filetype != "man"
    let thiswin = winnr()
    exe "norm! \<C-W>b"
    if winnr() > 1
      exe "norm! " . thiswin . "\<C-W>w"
      while 1
	if &filetype == "man"
	  break
	endif
	exe "norm! \<C-W>w"
	if thiswin == winnr()
	  break
	endif
      endwhile
    endif
    if &filetype != "man"
      if exists("g:ft_man_open_mode")
        if g:ft_man_open_mode == "vert"
          vnew
        elseif g:ft_man_open_mode == "tab"
          tabnew
        else
          new
        endif
      else
	if a:cmdmods != ''
	  exe a:cmdmods . ' new'
	else
	  new
	endif
      endif
      setlocal nonu fdc=0
    endif
  endif
  silent exec "edit $HOME/".page.".".sect."~"
  " Avoid warning for editing the dummy file twice
  setlocal buftype=nofile noswapfile

  setlocal ma nonu nornu nofen
  silent exec "norm! 1GdG"
  let unsetwidth = 0
  if empty($MANWIDTH)
    let $MANWIDTH = winwidth(0)
    let unsetwidth = 1
  endif

  " Ensure Vim is not recursively invoked (man-db does this) when doing ctrl-[
  " on a man page reference by unsetting MANPAGER.
  " Some versions of env(1) do not support the '-u' option, and in such case
  " we set MANPAGER=cat.
  if !exists('s:env_has_u')
    call system('env -u x true')
    let s:env_has_u = (v:shell_error == 0)
  endif
  let env_cmd = s:env_has_u ? 'env -u MANPAGER' : 'env MANPAGER=cat'
  let man_cmd = env_cmd . ' man ' . s:GetCmdArg(sect, page) . ' | col -b'
  silent exec "r !" . man_cmd

  if unsetwidth
    let $MANWIDTH = ''
  endif
  " Remove blank lines from top and bottom.
  while line('$') > 1 && getline(1) =~ '^\s*$'
    silent keepj norm! ggdd
  endwhile
  while line('$') > 1 && getline('$') =~ '^\s*$'
    silent keepj norm! Gdd
  endwhile
  1
  setlocal ft=man nomod
  setlocal bufhidden=hide
  setlocal nobuflisted
  setlocal noma
endfunction
