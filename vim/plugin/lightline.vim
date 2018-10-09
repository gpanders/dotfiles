if exists('plugs') && has_key(plugs, 'lightline.vim')
  let g:lightline = {}
  let g:lightline.colorscheme = 'solarized'
  let g:lightline.active = {}
  let g:lightline.active.left = [['mode', 'paste'], ['filename'], ['caps']]
  let g:lightline.active.right = [['lineinfo'], ['percent'], ['filetype'], ['fugitive']]

  let g:lightline.component_function = {
        \ 'fugitive': 'LightlineFugitive',
        \ 'filename': 'LightlineFilename',
        \ 'caps': 'LightlineCapsLock',
        \ }

  " lightline-bufferline config
  let g:lightline.tabline = { 'left': [['buffers']], 'right': [[]] }
  let g:lightline.component_expand = { 'buffers': 'lightline#bufferline#buffers' }
  let g:lightline.component_type = { 'buffers': 'tabsel' }

  let g:lightline#bufferline#filename_modifier = ':p:t'
  let g:lightline#bufferline#show_number = 1
  let g:lightline#bufferline#read_only = "\ue0a2"
  let g:lightline#bufferline#unnamed = '[No Name]'

  " lightline-buffer config
  " let g:lightline.tabline = {}
  " let g:lightline.tabline.left = [['bufferinfo'], ['bufferbefore', 'buffercurrent', 'bufferafter']]
  " let g:lightline.tabline.right = [[]]
  " let g:lightline.component_expand = {
  "       \ 'buffercurrent': 'lightline#buffer#buffercurrent2',
  "       \ }
  " let g:lightline.component_type = { 'buffercurrent': 'tabsel' }
  " let g:lightline.component_function.bufferbefore = 'lightline#buffer#bufferbefore'
  " let g:lightline.component_function.bufferafter = 'lightline#buffer#bufferafter'
  " let g:lightline.component_function.bufferinfo = 'lightline#buffer#bufferinfo'
  
  " lightline-buffer ui settings
  " replace these symbols with ascii characters if your environment does not support unicode
  " let g:lightline_buffer_readonly_icon = "\ue0a2"
  " let g:lightline_buffer_modified_icon = '*'
  " let g:lightline_buffer_git_icon = "\ue0a0 "
  " let g:lightline_buffer_ellipsis_icon = '..'
  " let g:lightline_buffer_expand_left_icon = '◀ '
  " let g:lightline_buffer_expand_right_icon = ' ▶'
  " let g:lightline_buffer_active_buffer_left_icon = ''
  " let g:lightline_buffer_active_buffer_right_icon = ''
  " let g:lightline_buffer_separator_icon = ' '

  " lightline-buffer function settings
  " let g:lightline_buffer_show_bufnr = 1
  " let g:lightline_buffer_rotate = 1
  " let g:lightline_buffer_fname_mod = ':p:t'
  " let g:lightline_buffer_excludes = ['vimfiler']

  " let g:lightline_buffer_maxflen = 30
  " let g:lightline_buffer_maxfextlen = 3
  " let g:lightline_buffer_minflen = 16
  " let g:lightline_buffer_minfextlen = 3
  " let g:lightline_buffer_reservelen = 20

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
    return ('' != expand('%:~:.') ? expand('%:~:.') : '[No Name]') .
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
endif
