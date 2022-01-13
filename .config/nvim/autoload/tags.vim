function! tags#qftf(args) abort
    let list = getqflist(#{id: a:args.id, items: 1, context: 1})
    let items = list.items
    let context = list.context
    let l = []
    for i in range(a:args.start_idx - 1, a:args.end_idx - 1)
        let item = items[i]
        call add(l, printf("%s|%s| %s", context[i].filename, context[i].type, item.text))
    endfor
    return l
endfunction
