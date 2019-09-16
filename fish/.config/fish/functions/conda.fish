function conda
    if command -sq conda
        source (command conda info --root)/etc/fish/conf.d/conda.fish
        conda $argv
    end
end

