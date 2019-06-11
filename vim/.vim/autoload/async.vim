" Wrapper functions for asynchronous APIs for nvim and vim
" Author: Greg Anders
" Date: 2019-04-16

" Neovim and Vim 8 both have a jobs API, but the syntax for each is slightly
" different. This function aims to provide a consistent interface for both to
" be used by other plugins throughout (n)vim.

let s:jobs = {}

function! s:callback(id, msg)
  let msg = a:msg
  let job = s:jobs[a:id]
  if type(job.cb) == type({-> 1})
    call job.cb(msg)
  elseif type(job.cb) == type('')
    if exists('*' . job.cb)
      let F = function(job.cb)
      call F(msg)
    else
      if type(msg) == type([])
        let msg = join(msg, "\n")
      endif
      " Apparently the substitute() function removes escaped characters, so
      " for example \" becomes ", so we have to escape the escaped
      " characters, hence the `escape(..., '\')` in the line below
      let command = substitute(job.cb, '\C\<v:val\>', '"' . escape(fnameescape(msg), '\') . '"', 'g')
      " After double escaping (explained above) the newline characters are
      " represented as \\n (a backslash followed by an actual newline). The
      " following replaces these with the literal '\n' character
      execute join(split(command, '\\\n'), '\n')
    endif
  endif
endfunction

function! s:stdout(channel, msg, ...)
  if a:0
    let id = a:1
  else
    let id = ch_info(a:channel).id
  endif

  let job = s:jobs[id]
  let msg = a:msg
  if type(msg) == type('')
    let msg = split(msg, "\n", 1)
  endif
  let job.chunks[-1] .= msg[0]
  call extend(job.chunks, msg[1:])
  if !job.buffered && len(job.chunks) > 1
    call s:callback(id, remove(job.chunks, 0, -2))
  end
endfunction

function! s:error(channel, msg, ...)
  echohl ErrorMsg
  let msg = a:msg
  if type(msg) == type([])
    let msg = join(msg[:-2])
  endif
  echom msg
  echohl None
endfunction

function! s:exit(channel, msg, ...)
  if a:0
    let id = a:1
  else
    let id = ch_info(a:channel).id
  endif

  let job = s:jobs[id]
  if job.buffered
    if job.chunks[-1] == ''
      call remove(job.chunks, -1)
    endif
    call s:callback(id, job.chunks)
  endif
  call job.completed(a:msg)
  call remove(s:jobs, id)
endfunction

function! s:shellsplit(str)
  return map(split(a:str, '\%(^\%("[^"]*"\|[^"]\)*\)\@<= '), {_, v -> substitute(v, '^"\|"$', '', 'g')})
endfunction

if has('nvim')
  let s:opts = {
        \ 'on_stdout': {i, d, e -> s:stdout(e, d, i)},
        \ 'on_stderr': {i, d, e -> s:error(e, d, i)},
        \ 'on_exit': {i, d, e -> s:exit(e, d, i)},
        \ 'stderr_buffered': 1,
        \ }
else
  let s:opts = {
        \ 'out_cb': function('s:stdout'),
        \ 'err_cb': function('s:error'),
        \ 'exit_cb': function('s:exit'),
        \ 'in_io': 'null',
        \ 'out_mode': 'raw'
        \ }
endif

function! async#run(cmd, cb, ...)
  let cmd = a:cmd

  let opts = {}
  if a:0
    let opts = a:1
  endif

  if get(opts, 'shell', 0) " Run command in a subshell
    " Convert command into a string if it is in list form
    if type(cmd) == type([])
      let cmd = join(cmd)
    endif

    if !has('nvim')
      " Neovim's jobstart() uses 'shell' by default when the command argument
      " is a string. For Vim, we have to explicitly add the shell command part
      let cmd = split(&shell) + split(&shellcmdflag) + [cmd]
    endif
  elseif type(cmd) == type('')
    " If the 'shell' option is not specified and the cmd argument is a string,
    " convert it into a list
    let cmd = s:shellsplit(cmd)
  endif

  if has('nvim')
    let jobid = jobstart(cmd, s:opts)
  elseif has('job')
    let job = job_start(cmd, s:opts)
    let jobid = ch_info(job_info(job).channel).id
  else
    echohl ErrorMsg
    echom 'Jobs API not supported'
    echohl None
    return
  endif

  let s:jobs[jobid] = {
        \ 'cb': a:cb,
        \ 'chunks': [''],
        \ 'buffered': get(opts, 'buffered', 1),
        \ 'completed': get(opts, 'completed', {_ -> 0}),
        \ }
  return jobid
endfunction
