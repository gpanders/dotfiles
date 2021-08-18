syn match gitcommitNewFile "^new file .*$" containedin=gitcommitDiff
hi gitcommitNewFile cterm=bold gui=bold
