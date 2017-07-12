let g:lightline = {}
let g:lightline.colorscheme = 'solarized'
let g:lightline.active = {}
let g:lightline.active.left = [['mode', 'paste'], [], ['caps']]
let g:lightline.active.right = [['lineinfo'], ['percent'], ['fugitive']]

let g:lightline.tabline = { 'left': [['buffers']], 'right': [[]] }

let g:lightline.component_expand = { 'buffers': 'lightline#bufferline#buffers' }
let g:lightline.component_type = { 'buffers': 'tabsel' }
let g:lightline.component_function = { 
            \ 'fugitive': 'LightlineFugitive',
            \ 'filename': 'LightlineFilename',
            \ 'caps': 'LightlineCapsLock'
            \ }

let g:lightline#bufferline#filename_modifier = ':p:t'
let g:lightline#bufferline#show_number = 1
let g:lightline#bufferline#read_only = "\ue0a2"
let g:lightline#bufferline#unnamed = '[No Name]'

set laststatus=2
set showtabline=2
set noshowmode

function! LightlineModified()
  if &filetype == 'help'
    return ''
  elseif &modified
    return '+'
  elseif &modifiable
    return ''
  else
    return ''
  endif
endfunction

function! LightlineReadonly()
  if &filetype == 'help'
    return ''
  elseif &readonly || !&modifiable
    return "\ue0a2"
  else
    return ''
  endif
endfunction

function! LightlineFilename()
  return ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
       \ ('' != LightlineReadonly() ? ' ' . LightlineReadonly() : '') .
       \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineFugitive()
    if exists('*fugitive#head')
        let branch = fugitive#head()
        return branch !=# '' ? "\ue0a0 ".branch : ''
    endif
    return ''
endfunction

function! LightlineCapsLock()
    if exists('*CapsLockStatusline')
        return CapsLockStatusline('CAPS')
    endif
    return ''
endfunction

