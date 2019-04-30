" Wrapper functions for asynchronous APIs for nvim and vim
" Author: Greg Anders
" Date: 2019-04-16

" Neovim and Vim 8 both have a jobs API, but the syntax for each is slightly
" different. This function aims to provide a consistent interface for both to
" be used by other plugins throughout (n)vim.

function! s:SID()
  return matchstr(expand('<sfile>'), '<SNR>\d\+_')
endfunc

function! async#run(cmd, cb)
  let s:cb = a:cb
  if has('nvim')
    let opts = {
          \ 'on_stdout': {_, d, e -> s:callback(e, d[0])},
          \ 'on_stderr': {_, d, e -> s:error(e, d[0])},
          \ 'stdout_buffered': 1, 'stderr_buffered': 1,
          \ }
    let job = jobstart(a:cmd, opts)
  elseif has('job')
    let opts = {
          \ 'out_cb': s:SID() . 'callback',
          \ 'err_cb': s:SID(). 'error',
          \ 'in_io': 'null',
          \ }
    call job_start(a:cmd, opts)
  else
    echohl ErrorMsg
    echom 'Jobs API not supported'
    echohl None
  endif
endfunction

function! s:callback(channel, msg)
  if exists('s:cb') && !empty(s:cb) && !empty(a:msg)
    if type(s:cb) == type({-> 1})
      call s:cb(a:msg)
    elseif type(s:cb) == type('')
      if exists('*' . s:cb)
        let F = function(s:cb)
        call F(a:msg)
      else
        execute substitute(s:cb, '\C\<v:val\>', shellescape(a:msg), 'g')
      endif
    endif
    unlet s:cb
  endif
endfunction

function! s:error(channel, msg)
  echohl ErrorMsg
  echom a:msg
  echohl None
endfunction
