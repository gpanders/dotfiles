hi clear gitcommitSummary
hi gitcommitSummary cterm=bold gui=bold

syn match gitcommitNewFile "^new file .*$" containedin=gitcommitDiff

hi gitcommitNewFile cterm=bold gui=bold
