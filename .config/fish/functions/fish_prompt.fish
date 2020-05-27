function fish_prompt
    set -l last_status $status
    set -l pwd_color (set_color $fish_color_cwd)
    set -l git_color (set_color $__fish_git_prompt_color)
    set -l venv_color (set_color $fish_color_venv)
    set -l jobs_color (set_color $fish_color_jobs)
    set -l cmd_duration_color (set_color $fish_color_cmd_duration)
    set -l delim_color (set_color $fish_color_prompt_delim)

    if test $last_status -ne 0
        set delim_color (set_color $fish_color_error)
    end

    set -l pwd (fish_prompt_pwd_dir_length=0 prompt_pwd)

    set -l git_info (git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if test -n "$git_info"
        set -l git_action (fish_print_git_action)
        if test -n "$git_action"
            set git_info $git_info" ($git_action)"
        end

        # Upstream status
        set count (git rev-list --count --left-right @{u}...HEAD 2>/dev/null)
        switch "$count"
            case ''
            case '0'\t'0'
            case '0'\t'*'
                set git_info $git_info' '(set_color cyan)'⇡'
            case '*'\t'0'
                set git_info $git_info' '(set_color cyan)'⇣'
            case '*'
                set git_info $git_info' '(set_color cyan)'⇡⇣'
        end
    end

    set -l cmd_duration
    if test -n "$CMD_DURATION" -a "$CMD_DURATION" -ge 5000
        set cmd_duration (humanize_duration $CMD_DURATION)
        set -e CMD_DURATION
    end

    set -l jobs
    set -l njobs (count (jobs -p))
    if test $njobs -gt 0
        set jobs "[$njobs]"
    end

    set -l virtualenv
    set -q VIRTUAL_ENV; and set virtualenv (basename $VIRTUAL_ENV)

    set -l delim '❯'

    # Clear existing line
    echo -e -n '\r\033[K'
    echo $pwd_color$pwd $git_color$git_info $cmd_duration_color$cmd_duration
    echo -n $jobs_color$jobs $venv_color$virtualenv $delim_color$delim' '
    echo -n (set_color $fish_color_normal)
end
