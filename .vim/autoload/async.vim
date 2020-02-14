" Wrapper functions for asynchronous APIs for nvim and vim
" Author: Greg Anders

" Neovim and Vim 8 both have a jobs API, but the syntax for each is slightly
" different. This function aims to provide a consistent interface for both to
" be used by other plugins throughout (n)vim.

let s:jobs = {}

function! s:id(chan)
  return has('nvim') ? a:chan : ch_info(a:chan).id
endfunction

function! s:jobstart(cmd)
  if has('nvim')
    return jobstart(a:cmd, {
                \ 'on_stdout': function('s:stdout'),
                \ 'on_stderr': function('s:error'),
                \ 'on_exit': function('s:exit')
                \ })
  elseif has('job')
    let job = job_start(a:cmd, {
                \ 'out_cb': function('s:stdout'),
                \ 'err_cb': function('s:error'),
                \ 'exit_cb': function('s:exit'),
                \ 'in_io': 'null',
                \ 'out_mode': 'raw',
                \ })
    return ch_info(job_info(job).channel).id
  else
    throw 'Jobs API not supported'
  endif
endfunction

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
  let id = s:id(a:channel)
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
  let msg = a:msg
  if type(msg) == type([])
    let msg = join(msg[:-2])
  endif
  echohl ErrorMsg
  echom msg
  echohl None
endfunction

function! s:exit(channel, msg, ...)
  let id = s:id(a:channel)
  if has_key(s:jobs, id)
    let job = s:jobs[id]
    if job.buffered
      if job.chunks[-1] ==# ''
        call remove(job.chunks, -1)
      endif
      call s:callback(id, job.chunks)
    endif
    if has_key(job, 'completed')
      call job.completed(a:msg)
    endif
    call remove(s:jobs, id)
  endif
endfunction

function! async#run(cmd, cb, ...)
  let opts = get(a:, 1, {})
  let id = s:jobstart(a:cmd)
  let s:jobs[id] = extend({'cb': a:cb, 'chunks': [''], 'buffered': 1}, opts)
  return id
endfunction
