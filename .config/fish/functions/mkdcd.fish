function mkdcd -d "Create a new directory and immediately cd into it"
    mkdir $argv
    if test $status = 0
        cd $argv
    end
end
