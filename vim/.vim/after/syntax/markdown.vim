" See https://github.com/tpope/vim-markdown/pull/140
syntax clear markdownCodeBlock
syn region markdownCodeBlock start="\n\(    \|\t\)" end="\v^((\t|\s{4})@!|$)" contained
