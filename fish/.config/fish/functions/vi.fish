function vi
    if not command -sq nvim
        functions -e vi
    else
        function vi
            command nvim $argv
        end
    end
    vi $argv
end

