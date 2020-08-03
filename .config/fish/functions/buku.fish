function buku
    set -q BUKU_COLORS; or set -Ux BUKU_COLORS "gCdxe"

    if test (count $argv) -eq 0
        command buku
    else
        command buku --suggest $argv
    end
end

