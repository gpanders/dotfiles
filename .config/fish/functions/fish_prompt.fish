function fish_prompt
    set -l last_status $status
    set -l pwd_color (set_color blue)
    set -l git_color (set_color brblack)
    set -l delim_color (set_color magenta)

    if test $last_status -ne 0
        set delim_color (set_color red)
    end

    set -l delim '❯'
    set -l git_branch (command git symbolic-ref --short HEAD 2>/dev/null)
    set -l pwd (fish_prompt_pwd_dir_length=0 prompt_pwd)

    echo $pwd_color$pwd $git_color$git_branch
    echo -n $delim_color$delim' '
    echo -n (set_color normal)
end

