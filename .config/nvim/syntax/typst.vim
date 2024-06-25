if exists('b:current_syntax')
  finish
endif
let b:current_syntax = 'typst'

syn region typstH1 matchgroup=typstH1Delimiter start="^=\s" end="$" oneline excludenl keepend
syn region typstH2 matchgroup=typstH2Delimiter start="^==\s" end="$" oneline excludenl keepend
syn region typstH3 matchgroup=typstH3Delimiter start="^===\s" end="$" oneline excludenl keepend
syn region typstH4 matchgroup=typstH4Delimiter start="^====\s" end="$" oneline excludenl keepend

syn region typstEmphasis start="\w\@<!_\S\@=" end="\S\@<=_\w\@!\|^$" skip="\\_" contains=@Spell
syn region typstStrong start="\w\@<!\*\S\@=" end="\S\@<=\*\w\@!\|^$" skip="\\\*" contains=@Spell
syn region typstRaw start="`" end="`"

syn region typstString start=+"+ skip=+\\"+ end=+"+

syn match typstUnits "[0-9]\+\%(\.[0-9]\+\)\?\%(%\|cm\|em\|pt\|in\|fr\)\?" contained

syn region typstMath matchgroup=typstMathDelimiter start="\$" end="\$" skip="\\\$" contains=typstString,typstSymbol,typstFunction

syn match typstSymExpression "#sym.\w\+" contains=typstSymbol

syn match typstCode "#\w\+"

syn region typstFunctionCall matchgroup=typstFunction start="#\w\+("rs=e-1 end=")$"re=e+1 contains=typstString,typstFunction,typstBoolean,typstUnits,typstContent
syn keyword typstFunction contained
      \ align block columns document heading image let page par set smallcaps sym text vec

syn region typstContent start="\[" end="\]" contains=TOP
syn region typstSetExpression matchgroup=typstSet start="#set\s\+\w\+("rs=s+4 end=")$"re=e+1 keepend contains=typstString,typstFunction,typstBoolean,typstUnits
syn region typstShowExpression matchgroup=typstShow start=/#show\%(\s\+[^:]\+\)\?:\s\+/rs=s+5 end=/$/ contains=typstString,typstFunction,typstBoolean,typstContent,typstUnits

syn keyword typstSymbol contained
      \ AA Alpha BB Beta CC Chi DD Delta EE Epsilon Eta FF GG Gamma
      \ HH II Im Iota JJ KK Kai Kappa LL Lambda MM Mu NN Nu OO Omega
      \ Omicron PP Phi Pi Psi QQ RR Re Rho SS Sigma TT Tau Theta UU
      \ Upsilon VV WW XX Xi YY ZZ Zeta acute alef aleph alpha amp
      \ and angle angstrom approx arrow ast at backslash ballot bar
      \ because bet beta beth bitcoin bot brace breve bullet caret
      \ checkmark chi circle co colon comma complement compose
      \ convolve dagger dalet daleth dash degree delta diameter
      \ diamond diff divides dollar dotless dots ell emptyset
      \ epsilon eq equiv eta euro exists floral forall forces franc
      \ gamma gimel gimmel gradient grave gt harpoon harpoons hat
      \ hexa in infinity integral iota join kai kappa kelvin lambda
      \ laplace lira lozenge lt macron maltese minus models mu
      \ multimap nabla not notes nothing nu ohm omega omicron oo or
      \ parallel partial penta perp peso phi pi planck plus pound
      \ prec product prop psi qed rect refmark rho ruble rupee sect
      \ servicemark shin sigma slash smash square star subset suit
      \ sum sum tack tau therefore theta tilde top triangle turtle
      \ union upsilon without won wreath xi xor yen zeta

syn keyword typstBoolean contained true false

syn match typstReference "@\w\+"
syn match typstLabel "<\w\+>"

syn region typstLineComment start="//" end="$"
syn region typstBlockComment start="/\*" end="\*/" keepend

hi def link typstHeader Title
hi def link typstH1 typstHeader
hi def link typstH2 typstH1
hi def link typstH3 typstH1
hi def link typstH4 typstH1

hi def link typstHeaderDelimiter Delimiter
hi def link typstH1Delimiter typstHeaderDelimiter
hi def link typstH2Delimiter typstHeaderDelimiter
hi def link typstH3Delimiter typstHeaderDelimiter
hi def link typstH4Delimiter typstHeaderDelimiter

hi def link typstMathDelimiter Character
hi def link typstSymbol Special
hi def link typstSymExpression typstSymbol

hi def link typstSet Macro
hi def link typstShow Macro

hi def link typstEmphasis Italic
hi def link typstStrong Bold
hi def link typstRaw Special

hi def link typstString String
hi def link typstBoolean Boolean
hi def link typstUnits Number

hi def link typstFunction Function

hi def link typstReference Label
hi def link typstLabel Label

hi def link typstLineComment Comment
hi def link typstBlockComment Comment
