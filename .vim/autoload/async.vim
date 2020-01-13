" Wrapper functions for asynchronous APIs for nvim and vim
" Author: Greg Anders

" Neovim and Vim 8 both have a jobs API, but the syntax for each is slightly
" different. This function aims to provide a consistent interface for both to
" be used by other plugins throughout (n)vim.

let s:jobs = {}

function! s:callback(id, msg)
  let job = s:jobs[a:id]
  if type(job.cb) == type({-> 1})
    call job.cb(a:msg)
  elseif type(job.cb) == type('')
    for m in a:msg
      execute substitute(job.cb, '\C\<v:val\>', shellescape(m), 'g')
    endfor
  endif
endfunction

function! s:stdout(channel, msg, ...)
  if a:0
    let id = a:1
  else
    let id = ch_info(a:channel).id
  endif

  if has_key(s:jobs, id)
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
  endif
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
    if job.chunks[-1] ==# ''
      call remove(job.chunks, -1)
    endif
    call s:callback(id, job.chunks)
  endif
  call job.completed(a:msg)
  call remove(s:jobs, id)
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
  let opts = get(a:, 1, {})
  if has('nvim')
    let jobid = jobstart(a:cmd, s:opts)
  elseif has('job')
    let job = job_start(a:cmd, s:opts)
    let jobid = ch_info(job_info(job).channel).id
  else
    throw 'Jobs API not supported'
  endif

  let s:jobs[jobid] = {
        \ 'cb': a:cb,
        \ 'chunks': [''],
        \ 'buffered': get(opts, 'buffered', 1),
        \ 'completed': get(opts, 'completed', {_ -> 0}),
        \ }
  return jobid
endfunction
