function fb -d "Open buku bookmarks in fzf"
    set -l website (buku -p -f 5 | column -ts'$'\t | fzy)

    for i in $website
        set -l index (echo "$i" | awk '{print $1}')
        buku -p "$index"
        buku -o "$index"
    end
end

