function todo.sh -d "Alias for todo.sh with custom config file location"
    if test (count $argv) -eq 0
        set argv ls
    end
    command todo.sh -d ~/.config/todo/config -c -n $argv
end

