syn include @gitcommitDiff syntax/diff.vim
syn region gitcommitDiff start=/\%(^diff --\%(git\|cc\|combined\) \)\@=/ end=/^\%(diff --\|$\|@@\@!\|[^[:alnum:]\ +-]\S\@!\)\@=/ fold contains=@gitcommitDiff
unlet! b:current_syntax
