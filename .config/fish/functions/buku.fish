function buku --description "Alias for buku --suggest. If no argument given, fuzzy find all bookmarks"
    set -q BUKU_COLORS; or set -Ux BUKU_COLORS "gCdxe"
    if test (count $argv) -eq 0; and command -sq fzf
        set -l website (buku --print --format 5 | column -ts'$'\t | fzf -m --with-nth 2..)

        for i in $website
            set -l index (echo "$i" | awk '{print $1}')
            buku --print "$index"
            buku --open "$index"
        end
    else
        command buku --suggest $argv
    end
end

