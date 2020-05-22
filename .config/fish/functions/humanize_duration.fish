function humanize_duration --argument-names milliseconds
    set -l seconds (math -s0 "$milliseconds / 1000 % 60")
    set -l minutes (math -s0 "$milliseconds / 60000 % 60")
    set -l hours (math -s0 "$milliseconds / 3600000 % 24")
    set -l days (math -s0 "$milliseconds / 86400000")
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

    set time $time (printf '%ss' $seconds)

    echo -n (string join ' ' $time)
end

