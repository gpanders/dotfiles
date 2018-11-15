if exists('plugs') && has_key(plugs, 'vim-sneak')
  " vim-sneak tries to map this to , but since that is the leader, it won't do
  " it. So use - instead
  " nmap - <Plug>Sneak_,
  " omap - <Plug>Sneak_,
  " xmap - <Plug>Sneak_,
endif
