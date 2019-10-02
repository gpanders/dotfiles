function buku --description "Alias for buku --suggest. If no argument given, fuzzy find all bookmarks"
    if test (count $argv) -eq 0; and command -sq fzy
        set -l website (buku --print --format 5 | column -ts'$'\t | fzy)

        for i in $website
            set -l index (echo "$i" | awk '{print $1}')
            buku --print "$index"
            buku --open "$index"
        end
    else
        command buku --suggest $argv
    end
end

