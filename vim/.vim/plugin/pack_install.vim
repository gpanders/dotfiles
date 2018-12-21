" Disable this plugin for now
if 0

if exists('g:loaded_pack_install')
  finish
endif
let g:loaded_pack_install = 1

command! -nargs=+ PackInstall call pack_install#Install(0, <f-args>)
command! -nargs=+ PackInstallOpt call pack_install#Install(1, <f-args>)
command! -nargs=+ PackRemove call pack_install#Remove(<f-args>)
command! -nargs=0 Packages call pack_install#List()

endif
