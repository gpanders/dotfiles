syn region mailItalic matchgroup=mailItalicDelimiter start="\S\@<=\\\@<!\*\|\\\@<!\*\S\@=" end="\S\@<=\\\@<!\*\|\\\@<!\*\S\@=" keepend contains=@Spell
syn region mailItalic matchgroup=mailItalicDelimiter start="\S\@<=\\\@<!_\|\\\@<!_\S\@=" end="\S\@<=\\\@<!_\|\\\@<!_\S\@=" keepend contains=@Spell
syn region mailBold matchgroup=mailBoldDelimiter start="\S\@<=\*\*\|\*\*\S\@=" end="\S\@<=\*\*\|\*\*\S\@=" keepend contains=mailItalic,@Spell
syn region mailBold matchgroup=mailBoldDelimiter start="\S\@<=__\|__\S\@=" end="\S\@<=__\|__\S\@=" keepend contains=mailItalic,@Spell
syn region mailBoldItalic matchgroup=mailBoldItalicDelimiter start="\S\@<=\*\*\*\|\*\*\*\S\@=" end="\S\@<=\*\*\*\|\*\*\*\S\@=" keepend contains=@Spell
syn region mailBoldItalic matchgroup=mailBoldItalicDelimiter start="\S\@<=___\|___\S\@=" end="\S\@<=___\|___\S\@=" keepend contains=@Spell

syn region mailCode matchgroup=mailCodeDelimiter start="`" end="`" keepend
syn region mailCode matchgroup=mailCodeDelimiter start="`` \=" end=" \=``" keepend
syn region mailCode matchgroup=mailCodeDelimiter start="^\s*````*.*$" end="^\s*````*\ze\s*$" keepend

hi def mailBold                term=bold cterm=bold gui=bold
hi def mailBoldUnderline       term=bold,underline cterm=bold,underline gui=bold,underline
hi def mailBoldItalic          term=bold,italic cterm=bold,italic gui=bold,italic
hi def mailBoldUnderlineItalic term=bold,italic,underline cterm=bold,italic,underline gui=bold,italic,underline
hi def mailUnderline           term=underline cterm=underline gui=underline
hi def mailUnderlineItalic     term=italic,underline cterm=italic,underline gui=italic,underline
hi def mailItalic              term=italic cterm=italic gui=italic

hi def link mailItalicDelimiter       mailItalic
hi def link mailBoldDelimiter         mailBold
hi def link mailBoldItalicDelimiter   mailBoldItalic
hi def link mailCodeDelimiter         Delimiter
