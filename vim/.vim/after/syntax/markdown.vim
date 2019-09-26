" See https://github.com/tpope/vim-markdown/pull/140
syntax clear markdownCodeBlock
syn region markdownCodeBlock start="\n\(    \|\t\)" end="\v^((\t|\s{4})@!|$)" contained

syn region markdownLinkText matchgroup=markdownLinkTextDelimiter start="!\=\[\%(\_[^]]*]\%( \=[[(]\)\)\@=" end="\]\%( \=[[(]\)\@=" nextgroup=markdownLink,markdownId skipwhite contains=@markdownInline,markdownLineStart concealends
syn region markdownLink matchgroup=markdownLinkDelimiter start="(" end=")" contains=markdownUrl keepend contained conceal
syn region markdownId matchgroup=markdownIdDelimiter start="\[" end="\]" keepend contained conceal

" https://github.com/tpope/vim-markdown/pull/141/
hi def link markdownLinkDelimiter     Delimiter
hi def link markdownLinkTextDelimiter Delimiter

" Don't highlight pure whitespace lines, as these are sometimes useful in
" Markdown
let b:highlight_trailing_whitespace = 0
syn match TrailingWhitespace /\S\+\zs\\\@<!\s\+\%#\@<!$/ containedin=ALL
