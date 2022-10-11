# Shamelessly ripped off from Jorge Bucaran (@jorgebucaran)'s Hydro prompt:
# https://github.com/jorgebucaran/hydro
# For OSC commands, see https://gitlab.freedesktop.org/Per_Bothner/specifications/blob/master/proposals/semantic-prompts.md

status is-interactive; or exit

set -q fish_prompt_delim; or set -g fish_prompt_delim '❯'

for type in cwd venv jobs git cmd_duration prompt_delim
    set -l color fish_color_$type
    set -g __prompt_color_$type (set_color $$color)
end

function __prompt_update_git --on-variable __prompt_git_$fish_pid
    set v __prompt_git_$fish_pid
    if set -q $v
        if test "$__prompt_git" != "$$v"
            set -g __prompt_git $$v
            commandline -f repaint
        end
        set -e $v
    end
end

function __prompt_update_pwd --on-variable PWD
    set -g __prompt_pwd (string replace -r -- '^'$HOME \~ $PWD)
    set -e __prompt_git_head
end

function __prompt_venv --on-variable VIRTUAL_ENV --on-variable IN_NIX_SHELL
    if set -q VIRTUAL_ENV
        set -g __prompt_venv (basename $VIRTUAL_ENV)' '
    else if set -q IN_NIX_SHELL
        set -g __prompt_venv 'nix-shell '
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
    commandline -f repaint
end

function __prompt_fish_preexec_handler --on-event fish_preexec
    # End of input, start of output
    printf '\e]133;C\e\\'
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
        end 2>/dev/null
    end
end

function __prompt_exit_status --on-event fish_postexec
    set -l last_pipestatus $pipestatus
    set -l __fish_last_status $status
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

    # End of current command (report status code)
    printf '\e]133;D;%d;aid=%d\e\\' $__fish_last_status $fish_pid
end

function __prompt_fish_prompt_handler --on-event fish_prompt
    if test $status -ne 0
        set __prompt_color_prompt_delim (set_color $fish_color_error)
    else
        set __prompt_color_prompt_delim (set_color $fish_color_prompt_delim)
    end

    set -q __prompt_pwd; or __prompt_update_pwd

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
        set -g __prompt_git
    else
        set -l os
        set -l branch (string replace -r '^ref: refs/heads/' '' < $__prompt_git_head; set os $status)
        if test $os -ne 0
            set branch (string sub -l 7 $branch)
        end

        if test "$branch" != "$__prompt_git_branch"
            set -g __prompt_git_branch $branch
            set -g __prompt_git "$branch "
        end
    end

    if test -z "$__prompt_git_branch"
        return
    end

    fish -P -c "
        set -l action (fish_print_git_action)
        if test -n \"\$action\"
            set action \"(\$action) \"
        end

        set -l dirty
        if not command git diff-index --no-ext-diff --quiet HEAD 2>/dev/null
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

        set -U __prompt_git_$fish_pid \"$__prompt_git_branch\$dirty \$action\$upstream\"
    " &

    # Fresh line and enter prompt mode
    printf '\e]133;A;cl=m;aid=%d\e\\' $fish_pid
end
