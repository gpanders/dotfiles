function vi -d "Alias for nvim, if installed"
    if not command -sq nvim
        functions -e vi
    else
        function vi -w nvim
            command nvim $argv
        end
    end
    vi $argv
end

