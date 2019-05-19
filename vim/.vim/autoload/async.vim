" Wrapper functions for asynchronous APIs for nvim and vim
" Author: Greg Anders
" Date: 2019-04-16

" Neovim and Vim 8 both have a jobs API, but the syntax for each is slightly
" different. This function aims to provide a consistent interface for both to
" be used by other plugins throughout (n)vim.

function! s:callback(...)
  if exists('s:cb') && !empty(s:cb)
    let msg = s:chunks
    if len(msg) == 1
      " If output is a single line return the result as a string instead of a
      " list
      let msg = msg[0]
    endif

    if type(s:cb) == type({-> 1})
      call s:cb(msg)
    elseif type(s:cb) == type('')
      if exists('*' . s:cb)
        let F = function(s:cb)
        call F(msg)
      else
        if type(msg) == type([])
          let msg = join(msg, "\n")
        endif
        " Apparently the substitute() function removes escaped characters, so
        " for example \" becomes ", so we have to escape the escaped
        " characters, hence the `escape(..., '\')` in the line below
        let command = substitute(s:cb, '\C\<v:val\>', '"' . escape(fnameescape(msg), '\') . '"', 'g')
        " After double escaping (explained above) the newline characters are
        " represented as \\n (a backslash followed by an actual newline). The
        " following replaces these with the literal '\n' character
        execute join(split(command, '\\\n'), '\n')
      endif
    endif
  endif
endfunction

function! s:stdout(channel, msg)
  if type(a:msg) == type([])
    call extend(s:chunks, a:msg)
  else
    call add(s:chunks, a:msg)
  endif
endfunction

function! s:error(channel, msg)
  echohl ErrorMsg
  let msg = a:msg
  if type(msg) == type([])
    let msg = join(msg)
  endif
  echom msg
  echohl None
endfunction

" Vim's jobs API requires a function name for the callback, so get the name of
" our script-local functions using `function`
let s:Callback = function('s:callback')
let s:Stdout = function('s:stdout')
let s:Error = function('s:error')

function! async#run(cmd, cb)
  let s:cb = a:cb
  let s:chunks = []
  if has('nvim')
    let opts = {
          \ 'on_stdout': {_, d, e -> s:stdout(e, d[:-2])},
          \ 'on_stderr': {_, d, e -> s:error(e, d[:-2])},
          \ 'on_exit': function('s:callback'),
          \ 'stdout_buffered': 1,
          \ 'stderr_buffered': 1,
          \ }
    let s:job = jobstart(a:cmd, opts)
  elseif has('job')
    let opts = {
          \ 'out_cb': s:Stdout,
          \ 'err_cb': s:Error,
          \ 'exit_cb': s:Callback,
          \ 'in_io': 'null',
          \ }
    let s:job = job_start(a:cmd, opts)
  else
    echohl ErrorMsg
    echom 'Jobs API not supported'
    echohl None
    return
  endif
  return s:job
endfunction
