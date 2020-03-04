function mkdcd -d "Create a new directory and immediately cd into it"
    mkdir -p -- $argv
    if test $status = 0
        cd $argv
    end
end
