function fish_right_prompt
    set -l jobs_color (set_color white)
    set -l cmd_duration_color (set_color yellow)

    set -l jobs
    set -l njobs (count (jobs -p))
    if test $njobs -gt 0
        set -l last_job (jobs -l -c)
        set jobs "[$njobs:$last_job]"
    end

    set -l cmd_duration
    if test $CMD_DURATION -ge 5000
        set cmd_duration (humanize_duration $CMD_DURATION)
    end

    # Move cursor up so that right prompt aligns with first line of left prompt
    echo -e "\e[A"
    begin
        echo $cmd_duration_color$cmd_duration $jobs_color$jobs
        echo -n (set_color normal)
    end
    echo -e "\e[B"
end
