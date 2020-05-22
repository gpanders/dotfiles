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
    if test -n "$CMD_DURATION" -a "$CMD_DURATION" -ge 5000
        set -l seconds (math -s0 "$CMD_DURATION / 1000 % 60")
        set -l minutes (math -s0 "$CMD_DURATION / 60000 % 60")
        set -l hours (math -s0 "$CMD_DURATION / 3600000 % 24")
        set -l days (math -s0 "$CMD_DURATION / 86400000")
        set -l time

        if test $days -gt 0
            set time $time (printf '%sd' $days)
        end

        if test $hours -gt 0
            set time $time (printf '%sh' $hours)
        end

        if test $minutes -gt 0
            set time $time (printf '%sm' $minutes)
        end

        set time $time (printf "%ss" $seconds)

        set cmd_duration (string join ' ' $time)
        set -e CMD_DURATION
    end

    # Move cursor up so that right prompt aligns with first line of left prompt
    echo -e "\e[A"
    begin
        echo $cmd_duration_color$cmd_duration $jobs_color$jobs
        echo -n (set_color normal)
    end
    echo -e "\e[B"
end
