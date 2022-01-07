syntax match oldfilesNumber "^\d\+: " nextgroup=oldfilesFileName
syntax match oldfilesFileName ".*" contained

hi def link oldfilesNumber Normal
hi def link oldfilesFileName Directory
