function conda -d "Lazy load conda"
    functions -e conda
    if command -sq conda
        source (command conda info --root)/etc/fish/conf.d/conda.fish
        conda $argv
    else
        __fish_command_not_found_handler conda
        return 127
    end
end

