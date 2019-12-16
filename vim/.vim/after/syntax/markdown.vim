" See https://github.com/tpope/vim-markdown/pull/140
syntax clear markdownCodeBlock
syn region markdownCodeBlock start="\n\(    \|\t\)" end="\v^((\t|\s{4})@!|$)" contained

" https://github.com/tpope/vim-markdown/pull/141/
hi def link markdownLinkDelimiter     Delimiter
hi def link markdownLinkTextDelimiter Delimiter

unlet! b:current_syntax
syn include @yaml syntax/yaml.vim

syn region markdownYamlMetadata matchgroup=markdownYamlMetadataDelimiter start="\%^---$" end="^\%(---\|\.\.\.\)$" keepend contains=@yaml

hi link markdownYamlMetadataDelimiter PreProc

let b:current_syntax = 'markdown'
