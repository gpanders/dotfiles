" Autoload fzf plugin file
unlet! g:loaded_fzf
runtime! plugin/fzf.vim

function! fzf#files() abort
    if !exists('g:loaded_fzf')
        echo 'FZF not installed'
        return
    endif

    FZF --tiebreak=length,end
endfunction

function! fzf#buffers(bang, mod) abort
    if !exists('g:loaded_fzf')
        ls
        call feedkeys(':b ')
        return
    endif

    let buffers = reverse(getbufinfo({'buflisted': 1}))
    let curbuf = bufnr('')
    let lastbuf = bufnr('#')
    let header_lines = '--header-lines=' . (buffers[0].bufnr == curbuf ? 1 : 0)
    let lines = map(buffers, {_, v ->
                \ printf("%d\t%-1s%-1s\x1b[0m %s",
                \       v.bufnr,
                \       curbuf == v.bufnr ? "\x1b[34m%" : lastbuf == v.bufnr ? "\x1b[35m#" : '',
                \       v.changed ? "\x1b[31m+" : '',
                \       empty(v.name) ? '[No Name]' : fnamemodify(v.name, ':p:~:.')
                \ )})

    call fzf#run(fzf#wrap('Buffers', {
                \ 'source': lines,
                \ 'sink': {b -> execute(a:mod . 'b' . split(b)[0])},
                \ 'options': '+m ' . header_lines . ' -d \\t --ansi --tiebreak=index -n 1 --with-nth=2..',
                \ }, a:bang))
endfunction
