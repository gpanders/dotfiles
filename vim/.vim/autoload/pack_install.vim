if !has('packages')
  " Requires vim's native package management feature
  finish
endif

function! pack_install#Install(opt, ...) abort
  for pkg in a:000
    let [author, repo] = split(pkg, "/")
    let author = substitute(author, "\(^vim\-\|[-.]vim$\)", "", "g")
    let repo = substitute(repo, "\(^vim\-\|[-.]vim$\|\.git$\)", "", "g")
    let dir = "~/.vim/pack/" . author . "/" . (a:opt ? "opt" : "start") . "/" . repo
    silent! clear
    silent execute "!git clone https://github.com/" . pkg . " " .dir
    silent! execute "helptags " . dir . "/doc"
    echom "Installed package: " . author . "/" . repo
  endfor
endfunction

function! pack_install#Remove(...) abort
  for pkg in a:000
    let [author, repo] = split(pkg, "/")
    let author = substitute(author, "\(^vim\-\|[-.]vim$\)", "", "g")
    let repo = substitute(repo, "\(^vim\-\|[-.]vim$\)", "", "g")
    silent! clear
    if isdirectory($HOME . "/.vim/pack/" . author . "/opt/" . repo)
      if confirm("Remove package ~/.vim/pack/" . author . "/opt/" . repo . "?", "&Yes\n&No", 2) == 1
        silent execute "!rm -rf ~/.vim/pack/" . author . "/opt/" . repo
        echom "Removed package: " . author . "/" . repo
      endif
    elseif isdirectory($HOME . "/.vim/pack/" . author . "/start/" . repo)
      if confirm("Remove package ~/.vim/pack/" . author . "/start/" . repo . "?", "&Yes\n&No", 2) == 1
        silent execute "!rm -rf ~/.vim/pack/" . author . "/start/" . repo
        echom "Removed package: " . author . "/" . repo
      endif
    else
      echom "Package not found: " . author . "/" . repo
    endif
  endfor
endfunction

function! pack_install#List() abort
  let start_packs = []
  let opt_packs = []
  let authors = []
  let paths = globpath($HOME . "/.vim/pack", "*", 0, 1)
  for path in paths
    let author = split(path, "/")[-1]
    for pkg in globpath(path, "start/*", 0, 1)
      let pkg_name = split(split(pkg, author)[-1], "/")[-1]
      let start_packs += [author . "/" . pkg_name]
    endfor
    for pkg in globpath(path, "opt/*", 0, 1)
      let pkg_name = split(split(pkg, author)[-1], "/")[-1]
      let opt_packs += [author . "/" . pkg_name]
    endfor
  endfor

  echom "Installed packages:"
  echom "start/"
  for pkg in start_packs
    echom "    " . pkg
  endfor
  echom ""
  echom "opt/"
  for pkg in opt_packs
    echom "    " . pkg
  endfor
endfunction

