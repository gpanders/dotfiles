# Shamelessly ripped off from Jorge Bucaran (@jorgebucaran)'s Hydro prompt:
# https://github.com/jorgebucaran/hydro

status is-interactive; or exit

set -q fish_prompt_delim; or set -g fish_prompt_delim '❯'

set -g __prompt_git __prompt_git_$fish_pid

function $__prompt_git --on-variable $__prompt_git
    commandline -f repaint
end

function __prompt_update_git_branch --on-variable PWD --on-event fish_postexec
    if test -n "$argv[1]"
        set -l HEAD
        if test -f .git/HEAD
            set HEAD .git/HEAD
        else
            set HEAD (command git rev-parse --git-path HEAD 2>/dev/null)
        end

        if test -z "$HEAD"
            set -g __prompt_git_branch
        else
            set -l os
            set -g __prompt_git_branch (string replace -r '^ref: refs/heads/' '' < $HEAD; set os $status)
            if test $os -ne 0
                set -g __prompt_git_branch (string sub -l 7 $__prompt_git_branch)
            end
        end

        set -U $__prompt_git $__prompt_git_branch
    end
end

function __prompt_update_pwd --on-variable PWD
    set -g __prompt_pwd (string replace -r -- '^'$HOME \~ $PWD)
end

function __prompt_venv --on-variable VIRTUAL_ENV
    if set -q VIRTUAL_ENV
        set -g __prompt_venv (basename $VIRTUAL_ENV)' '
    else
        set -g __prompt_venv
    end
end

function __prompt_fish_postexec_handler --on-event fish_postexec
    if test "$CMD_DURATION" -lt 1000
        set -g __prompt_cmd_duration
    else
        set -l secs (math --scale=1 $CMD_DURATION/1000 % 60)
        set -l mins (math --scale=0 $CMD_DURATION/60000 % 60)
        set -l hours (math --scale=0 $CMD_DURATION/3600000)

        test $hours -gt 0; and set -l -a out $hours"h"
        test $mins -gt 0; and set -l -a out $mins"m"
        test $secs -gt 0; and set -l -a out $secs"s"

        set -g __prompt_cmd_duration "$out "
    end

    set CMD_DURATION 0

    set -l njobs (count (jobs -p))
    if test $njobs -eq 0
        set -g __prompt_jobs
    else
        set -g __prompt_jobs "[$njobs] "
    end
end

function __prompt_fish_prompt_handler --on-event fish_prompt
    set -l last_status $status
    if test $last_status -ne 0
        set __prompt_color_prompt_delim (set_color $fish_color_error)
    else
        set __prompt_color_prompt_delim (set_color $fish_color_prompt_delim)
    end

    set -q __prompt_pwd; or __prompt_update_pwd
    set -q __prompt_git_branch; or __prompt_update_git_branch true

    command kill $__prompt_last_pid 2>/dev/null

    fish -P -c "
        set git_info (git rev-parse --short HEAD 2>/dev/null; git rev-parse --abbrev-ref HEAD 2>/dev/null)
        if test \"\$git_info[2]\" != HEAD
            set git_info \$git_info[2]
        else
            set git_info \$git_info[1]
        end

        if test -z \"\$git_info\"
            set $__prompt_git
            exit
        end

        set git_action (fish_print_git_action)
        if test -n \"\$git_action\"
            set git_info \$git_info\" (\$git_action)\"
        end

        # Upstream status
        set count (git rev-list --count --left-right @{u}...HEAD 2>/dev/null)
        switch \"\$count\"
            case ''
            case '0'\t'0'
            case '0'\t'*'
                set git_info \$git_info' '(set_color cyan)'⇡'
            case '*'\t'0'
                set git_info \$git_info' '(set_color cyan)'⇣'
            case '*'
                set git_info \$git_info' '(set_color cyan)'⇡⇣'
        end

        set -U $__prompt_git \"\$git_info \"
    " &

    set -g __prompt_last_pid (jobs -l -p)
    disown $__prompt_last_pid
end

function __prompt_fish_exit_handler --on-event fish_exit
    set -q __prompt_last_pid; and command kill $__prompt_last_pid 2>/dev/null
    set -e $__prompt_git
end

for type in cwd venv jobs git cmd_duration prompt_delim
    function __prompt_color_$type --on-variable fish_color_$type --inherit-variable type
        set -l color fish_color_$type
        set -g __prompt_color_$type (set_color $$color)
    end
    __prompt_color_$type
end

