function fish_prompt
    set -l last_status $status
    set -l pwd_color (set_color blue)
    set -l git_color (set_color brblack)
    set -l jobs_color (set_color white)
    set -l cmd_duration_color (set_color yellow)
    set -l delim_color (set_color magenta)

    if test $last_status -ne 0
        set delim_color (set_color red)
    end

    set -l delim '❯'

    set -l jobs
    set -l njobs (count (jobs -p))
    if test $njobs -gt 0
        set jobs "[$njobs]"
    end

    set -l cmd_duration
    if test -n "$CMD_DURATION" -a "$CMD_DURATION" -ge 5000
        set cmd_duration (humanize_duration $CMD_DURATION)
        set -e CMD_DURATION
    end

    set -l git_info (command git symbolic-ref --short HEAD 2>/dev/null)
    if test -n "$git_info"
        set -l git_upstream_status (command git rev-list --left-right --count 'HEAD...@{upstream}' 2>/dev/null)
        if test -n "$git_upstream_status"
            echo $git_upstream_status | read -l -a git_status
            set -l commit_to_push $git_status[1]
            set -l commit_to_pull $git_status[2]

            if test $commit_to_push -gt 0 -o $commit_to_pull -gt 0
                set git_info $git_info' '
            end

            if test $commit_to_push -gt 0
                set git_info $git_info(set_color cyan)'⇡'
            end

            if test $commit_to_pull -gt 0
                set git_info $git_info(set_color cyan)'⇣'
            end
        end
    end

    set -l pwd (fish_prompt_pwd_dir_length=0 prompt_pwd)

    echo $pwd_color$pwd $git_color$git_info $cmd_duration_color$cmd_duration
    echo -n $jobs_color$jobs $delim_color$delim' '
    echo -n (set_color normal)
end

