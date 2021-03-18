" Wrapper functions for asynchronous APIs for nvim and vim
" Author: Gregory Anders

" Neovim and Vim 8 both have a jobs API, but the syntax for each is slightly
" different. This function aims to provide a consistent interface for both to
" be used by other plugins throughout (n)vim.

if !has('nvim') && !has('job')
    let async#enabled = 0
    finish
endif
let async#enabled = 1

let s:jobs = {}

function! s:chanid(chan) abort
  return has('nvim') ? a:chan : ch_info(a:chan).id
endfunction

function! s:jobstart(cmd) abort
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
                \ 'close_cb': function('s:exit'),
                \ 'in_io': 'null',
                \ 'out_mode': 'raw',
                \ })
    return job_getchannel(job)
  endif
endfunction

function! s:callback(id, msg) abort
  let job = s:jobs[a:id]
  if type(job.cb) == v:t_func
    call job.cb(a:msg)
  elseif type(job.cb) == v:t_string
    if empty(a:msg)
      silent execute job.cb
    else
      for m in a:msg
        silent execute substitute(job.cb, '\C\<v:val\>', shellescape(m), 'g')
      endfor
    endif
  endif
endfunction

function! s:stdout(channel, msg, ...) abort
  let id = s:chanid(a:channel)
  if has_key(s:jobs, id)
    let job = s:jobs[id]
    let msg = a:msg
    if type(msg) == v:t_string
      let msg = split(msg, "\n", 1)
    endif
    if len(job.chunks)
        let job.chunks[-1] .= msg[0]
    else
        call add(job.chunks, msg[0])
    endif
    call extend(job.chunks, msg[1:])
    if !job.buffered && len(job.chunks) > 1
      call s:callback(id, remove(job.chunks, 0, -2))
    end
  endif
endfunction

function! s:error(channel, msg, ...) abort
  let msg = a:msg
  if type(msg) == v:t_list
    let msg = join(msg[:-2])
  endif
  echohl ErrorMsg
  echom msg
  echohl None
endfunction

function! s:exit(channel, ...) abort
  let id = s:chanid(a:channel)
  if has_key(s:jobs, id)
    let job = s:jobs[id]
    if job.buffered
      if job.chunks[-1] ==# ''
        call remove(job.chunks, -1)
      endif
      call s:callback(id, job.chunks)
    endif
    if has_key(job, 'exit')
      if a:0
        let code = a:1
      elseif exists('*job_info')
        let code = job_info(a:channel).exitval
      else
        echohl ErrorMsg
        echom 'Couldn''t get exit code for job ' . a:channel
        echohl None
        call remove(s:jobs, id)
        return
      endif

      if type(job.exit) == v:t_func
        call job.exit(code)
      elseif type(job.exit) == v:t_string
        silent execute substitute(job.exit, '\C\<v:val\>', code, 'g')
      endif
    endif
    call remove(s:jobs, id)
  endif
endfunction

function! async#run(cmd, cb, ...) abort
  let opts = get(a:, 1, {})
  let chan = s:jobstart(a:cmd)
  let id = s:chanid(chan)
  let s:jobs[id] = extend({'cb': a:cb, 'chunks': [''], 'buffered': 1, 'chan': chan}, opts)
  return id
endfunction

function! async#cancel(id) abort
  if has('nvim')
    call jobstop(a:id)
  elseif has('job')
    let chan = s:jobs[a:id].chan
    call job_stop(ch_getjob(chan))
  endif
endfunction
