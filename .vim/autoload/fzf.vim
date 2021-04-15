" Autoload fzf plugin file
unlet! g:loaded_fzf
runtime! plugin/fzf.vim

function! fzf#files() abort
    if !exists('g:loaded_fzf')
        echo 'FZF not installed'
        return
    endif

    FZF --layout=default --tiebreak=end,length
endfunction

function! fzf#buffers(bang, mod) abort
    if !exists('g:loaded_fzf')
        ls
        call feedkeys(':b ')
        return
    endif

    let digits = (bufnr('$') / 10) + 1
    let buffers = map(reverse(getbufinfo({'buflisted': 1})), {_, v -> 
                \ printf('%-*d %-1s %-1s %s',
                \       digits,
                \       v.bufnr,
                \       bufnr('') == v.bufnr ? '%' : bufnr('#') == v.bufnr ? '#' : '',
                \       getbufvar(v.bufnr, '&modified') ? '+' : '',
                \       fnamemodify(v.name, ':.')
                \ )})

    call fzf#run(fzf#wrap('Buffers', {
                \ 'source': buffers,
                \ 'sink': {b -> execute(a:mod . 'b' . split(b)[0])},
                \ 'options': '-n -1 --layout=default --no-info --no-multi --tiebreak=index',
                \ }, a:bang))
endfunction
