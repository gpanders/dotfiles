if !exists(':Limelight')
  finish
endif

if exists(':Goyo')
  autocmd! User GoyoEnter Limelight
  autocmd! User GoyoLeave Limelight!
endif
