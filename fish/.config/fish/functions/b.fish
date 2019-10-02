function b -d "Alias for buku --suggest"
    if not command -sq buku
        functions -e b
    else
        function b -w buku
            command buku --suggest $argv
        end
    end
    b $argv
end

