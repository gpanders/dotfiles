" See https://github.com/tpope/vim-markdown/pull/140
syntax clear markdownCodeBlock
syn region markdownCodeBlock start="\n\(    \|\t\)" end="\v^((\t|\s{4})@!|$)" contained

" https://github.com/tpope/vim-markdown/pull/141/
hi def link markdownLinkDelimiter     Delimiter
hi def link markdownLinkTextDelimiter Delimiter

syn region markdownYamlMetadata matchgroup=markdownYamlMetadataDelimiter start="\%^---$" end="^\%(---\|\.\.\.\)$" keepend

hi link markdownYamlMetadataDelimiter Comment
hi link markdownYamlMetadata          Normal

let b:current_syntax = 'markdown'
