# Shamelessly ripped off from Jorge Bucaran (@jorgebucaran)'s Hydro prompt:
# https://github.com/jorgebucaran/hydro

status is-interactive; or exit

set -q fish_prompt_delim; or set -g fish_prompt_delim '❯'

set -g __prompt_git __prompt_git_$fish_pid

for type in cwd venv jobs git cmd_duration prompt_delim
    set -l color fish_color_$type
    set -g __prompt_color_$type (set_color $$color)
end

function $__prompt_git --on-variable $__prompt_git
    commandline -f repaint
end

function __prompt_update_pwd --on-variable PWD
    set -g __prompt_pwd (string replace -r -- '^'$HOME \~ $PWD)
    set -e __prompt_git_head
end

function __prompt_venv --on-variable VIRTUAL_ENV
    if set -q VIRTUAL_ENV
        set -g __prompt_venv (basename $VIRTUAL_ENV)' '
    else
        set -g __prompt_venv
    end
end

function __prompt_update_jobs
    set -l njobs (count (jobs -p))
    if test $njobs -eq 0
        set -g __prompt_jobs
    else
        set -g __prompt_jobs "[$njobs] "
    end
end

function __prompt_fish_postexec_handler --on-event fish_postexec
    if test "$CMD_DURATION" -gt 1000
        set -l secs (math --scale=1 $CMD_DURATION/1000 % 60)
        set -l mins (math --scale=0 $CMD_DURATION/60000 % 60)
        set -l hours (math --scale=0 $CMD_DURATION/3600000)

        set -l dur
        test $hours -gt 0; and set -a dur $hours"h"
        test $mins -gt 0; and set -a dur $mins"m"
        test $secs -gt 0; and set -a dur $secs"s"
        set -g __prompt_cmd_duration_tmp "$dur "
    end

    set -l last_job (jobs -l -g)
    if test -n "$last_job"; and test "$last_job" != "$__prompt_last_job"
        set -g __prompt_last_job $last_job
        __prompt_update_jobs
        function _notify_job_$last_job --on-job-exit $last_job --inherit-variable last_job
            functions -e _notify_job_$last_job
            __prompt_update_jobs
        end
    end
end

function __prompt_exit_status --on-event fish_postexec
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status
    if test $__fish_last_status -eq 0
        set -g __prompt_status
        return
    end

    set -q __prompt_status_generation; or set -g __prompt_status_generation $status_generation

    set -l bold_flag --bold
    if test $__prompt_status_generation = $status_generation
        set bold_flag
    end
    set __prompt_status_generation $status_generation
    set -g __prompt_status (__fish_print_pipestatus '[' '] ' '|' (set_color $fish_color_status) (set_color $bold_flag $fish_color_status) $last_pipestatus)
end

function __prompt_fish_prompt_handler --on-event fish_prompt
    if test $status -ne 0
        set __prompt_color_prompt_delim (set_color $fish_color_error)
    else
        set __prompt_color_prompt_delim (set_color $fish_color_prompt_delim)
    end

    set -q __prompt_pwd; or __prompt_update_pwd

    if set -q IN_NIX_SHELL
        set -g __prompt_venv 'nix-shell '
    end

    if set -q __prompt_cmd_duration_tmp
        set -g __prompt_cmd_duration $__prompt_cmd_duration_tmp
        set -e __prompt_cmd_duration_tmp
    else
        set -g __prompt_cmd_duration
    end

    if not set -q __prompt_git_head
        if test -n "$GIT_DIR" && test -f $GIT_DIR/HEAD
            set -g __prompt_git_head $GIT_DIR/HEAD
        else if test -f .git/HEAD
            set -g __prompt_git_head .git/HEAD
        else
            set -g __prompt_git_head (command git rev-parse --git-path HEAD 2>/dev/null)
        end
    end

    if test -z "$__prompt_git_head"
        set -g __prompt_git_branch
        set -U $__prompt_git
    else
        set -l os
        set -l branch (string replace -r '^ref: refs/heads/' '' < $__prompt_git_head; set os $status)
        if test $os -ne 0
            set branch (string sub -l 7 $branch)
        end

        if test "$branch" != "$__prompt_git_branch"
            set -g __prompt_git_branch $branch
            set -U $__prompt_git "$branch "
        end
    end

    command kill $__prompt_last_pid 2>/dev/null

    if test -z "$__prompt_git_branch"
        return
    end

    fish -P -c "
        set -l action (fish_print_git_action)
        if test -n \"\$action\"
            set action \"(\$action) \"
        end

        set -l dirty
        if not command git diff-index --no-ext-diff --quiet HEAD
            set dirty '*'
        end

        # Upstream status
        set count (git rev-list --count --left-right @{u}...HEAD 2>/dev/null)
        set upstream
        switch \"\$count\"
            case ''
            case '0'\t'0'
            case '0'\t'*'
                set upstream (set_color cyan)'⇡ '
            case '*'\t'0'
                set upstream (set_color cyan)'⇣ '
            case '*'
                set upstream (set_color cyan)'⇡⇣ '
        end

        set -U $__prompt_git \"$__prompt_git_branch\$dirty \$action\$upstream\"
    " &

    set -g __prompt_last_pid (jobs -l -p)
    disown $__prompt_last_pid
end

function __prompt_fish_exit_handler --on-event fish_exit
    set -q __prompt_last_pid; and command kill $__prompt_last_pid 2>/dev/null
    set -e $__prompt_git
end
