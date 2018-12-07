" Install and remove packages from vim's pack directory
" Author: Greg Anders <greg@gpanders.com>

if has('win64') || has('win32') || has('win16')
  let g:pack_install#pack_dir = $HOME . '\vimfiles\pack\'
else
  let g:pack_install#pack_dir = $HOME . '/.vim/pack/'
endif

function! pack_install#Install(opt, ...) abort
  let installed_packs = pack_install#FindPackages()
  silent! clear
  for pkg in a:000
    try
      let [author, repo] = split(pkg, '/')
    catch
      echom 'Invalid package name: ' . pkg
      continue
    endtry

    let repo = substitute(repo, '\(^vim\-\|[-.]vim$\|\.git$\)', '', 'g')
    let installed = 0
    for installed_pkg in (installed_packs.start + installed_packs.opt)
      if installed_pkg.author ==# author && installed_pkg.name ==# repo
        let installed = 1
        break
      endif
    endfor

    if installed
      echom 'Package already installed: ' . author . '/' . repo
      continue
    endif

    let dir = g:pack_install#pack_dir . author . '/' . (a:opt ? 'opt' : 'start') . '/' . repo
    silent execute '!echo -n Installing package: ' . author . '/' . repo . '...'
    silent execute '!git clone -q https://github.com/' . pkg . ' ' . dir
    silent! execute 'helptags ' . dir . '/doc'
    silent !echo ' Done.'
  endfor
  redraw!

  if a:0 > 1
    echom "Done!"
  endif
endfunction

function! pack_install#Remove(...) abort
  let installed_packs = pack_install#FindPackages()
  silent! clear
  for pkg in a:000
    let author = ''
    let tmp = split(pkg, "/")
    if len(tmp) == 1
      " Could be just a package name
      let repo = substitute(tmp[0], '\(^vim\-\|[-.]vim$\)', '', 'g')
      for installed_pkg in (installed_packs.start + installed_packs.opt)
        if installed_pkg.name ==? repo
          let author = installed_pkg.author
          break
        endif
      endfor
      if empty(author)
        echom 'Package not found: ' . repo
        continue
      endif
    elseif len(tmp) == 2
      let author = tmp[0]
      let repo = substitute(tmp[1], '\(^vim\-\|[-.]vim$\)', '', 'g')
    else
      echom 'Invalid package name: ' . pkg
      continue
    endif

    let root_dir = g:pack_install#pack_dir . author
    let opt_dir = root_dir . '/opt/' . repo
    let start_dir = root_dir . '/start/' . repo
    if isdirectory(opt_dir)
      if confirm('Remove package ' author . '/' . repo . '?', "&Yes\n&No", 2) == 1
        silent execute '!rm -rf ' . opt_dir
        silent execute '!echo Removed package: ' . author . '/' . repo
      endif
    elseif isdirectory(start_dir)
      if confirm('Remove package ' . author . '/' . repo . '?', "&Yes\n&No", 2) == 1
        silent execute '!rm -rf ' . start_dir
        silent execute '!echo Removed package: ' . author . '/' . repo
      endif
    else
      echom 'Package not found: ' . author . '/' . repo
    endif
  endfor
  redraw!

  if a:0 > 1
    echom "Done!"
  endif
endfunction

function! pack_install#FindPackages() abort
  let start_packs = []
  let opt_packs = []
  let authors = []
  let paths = globpath(g:pack_install#pack_dir, "*", 0, 1)
  for path in paths
    let author = split(path, '/')[-1]
    for pkg in globpath(path, "start/*", 0, 1)
      let pkg_name = split(split(pkg, author)[-1], "/")[-1]
      let start_packs += [{'author': author, 'name': pkg_name}]
    endfor
    for pkg in globpath(path, "opt/*", 0, 1)
      let pkg_name = split(split(pkg, author)[-1], "/")[-1]
      let opt_packs += [{'author': author, 'name': pkg_name}]
    endfor
  endfor

  return {'start': start_packs, 'opt': opt_packs}
endfunction

function! pack_install#List() abort
  let packs = pack_install#FindPackages()
  echom 'Installed packages:'
  echom 'start/'
  for pkg in packs.start
    echom '    ' . pkg.author . '/' . pkg.name
  endfor
  echom ''
  echom 'opt/'
  for pkg in packs.opt
    echom '    ' . pkg.author . '/' . pkg.name
  endfor
endfunction

