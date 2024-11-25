# Originally based off of Jorge Bucaran (@jorgebucaran)'s Hydro prompt: https://github.com/jorgebucaran/hydro
# For OSC commands, see https://gitlab.freedesktop.org/Per_Bothner/specifications/blob/master/proposals/semantic-prompts.md

status is-interactive; or exit

set -q fish_prompt_delim; or set -g fish_prompt_delim '❯'

set -g fish_handle_reflow 1

if set -q SSH_TTY
    set -g __prompt_host (set_color $fish_color_host_remote)"$hostname"(set_color normal)":"
end

function __prompt_update_git --on-variable __prompt_git_branch --on-variable __prompt_git_action --on-variable __prompt_git_dirty --on-variable __prompt_git_upstream
    set -g __prompt_git (set_color $fish_color_git)"$__prompt_git_branch$__prompt_git_dirty $__prompt_git_action$__prompt_git_upstream"
    commandline -f repaint
end

function __prompt_update_pwd --on-variable PWD
    set -g __prompt_pwd (set_color $fish_color_cwd)(string replace -r -- '^'$HOME \~ $PWD)

    # Reset the location of the Git directory so that it can be rediscovered
    set -e __prompt_git_dir

    # ...but if current directory contains a .jj directory then skip Git
    # discovery by setting the variable to an empty string
    if test -d $PWD/.jj
        set -g __prompt_git_dir
    end
end

function __prompt_venv --on-variable VIRTUAL_ENV --on-variable IN_NIX_SHELL
    if set -q VIRTUAL_ENV
        set -g __prompt_venv (set_color $fish_color_venv)(basename $VIRTUAL_ENV)' '
    else if set -q IN_NIX_SHELL
        set -g __prompt_venv (set_color $fish_color_venv)'nix-shell '
    else
        set -g __prompt_venv
    end
end

functions -c fish_job_summary __fish_job_summary
function fish_job_summary
    __fish_job_summary $argv
    __prompt_update_jobs
end

function __prompt_update_jobs
    set -l njobs (count (jobs -g))
    if test $njobs -eq 0
        set -g __prompt_jobs
    else
        set -g __prompt_jobs (set_color $fish_color_jobs)"[$njobs] "
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

    __prompt_update_jobs
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
end

function __prompt_fish_prompt_handler --on-event fish_prompt
    if test $status -ne 0
        set -g __prompt_delim (set_color $fish_color_error)$fish_prompt_delim
    else
        set -g __prompt_delim (set_color $fish_color_prompt_delim)$fish_prompt_delim
    end

    set -q __prompt_pwd; or __prompt_update_pwd

    if set -q __prompt_cmd_duration_tmp
        set -g __prompt_cmd_duration (set_color $fish_color_cmd_duration)$__prompt_cmd_duration_tmp
        set -e __prompt_cmd_duration_tmp
    else
        set -g __prompt_cmd_duration
    end

    if not set -q __prompt_git_dir
        if test -n "$GIT_DIR"
            set -g __prompt_git_dir "$GIT_DIR"
        else if test -d .git
            set -g __prompt_git_dir $PWD/.git
        else if test -f .git
            set -g __prompt_git_dir (string match -r -g '^gitdir: (.*)$' < .git)
        else
            set -g __prompt_git_dir (command git rev-parse --show-toplevel 2>/dev/null)
        end
    end

    if test -z "$__prompt_git_dir"; or test -d $__prompt_git_dir/.jj
        set -g __prompt_git
    else
        set -l HEAD
        if test -f $__prompt_git_dir/HEAD
            set HEAD $__prompt_git_dir/HEAD
        else
            set HEAD (command git rev-parse --git-path HEAD 2>/dev/null)
        end

        set -g __prompt_git_branch (string match -r -g '^ref: refs/heads/(.*)|([0-9a-f]{8})[0-9a-f]+$' < $HEAD)
        set -l action (fish_print_git_action "$__prompt_git_dir")
        if test -n "$action"
            set -g __prompt_git_action "($action)"
        else
            set -e __prompt_git_action
        end

        begin
            block -l
            fish -P -c "command git diff-index --no-ext-diff --quiet HEAD 2>/dev/null" &
            function __prompt_git_dirty_$last_pid --on-process-exit $last_pid --inherit-variable $last_pid
                functions -e __prompt_git_dirty_$last_pid
                set -l ret $argv[3]
                switch $ret
                    case 0
                        set -g __prompt_git_dirty ''
                    case '*'
                        set -g __prompt_git_dirty '*'
                end
            end
        end

        begin
            block -l
            fish -P -c "
                # Upstream status
                set count (git rev-list --count --left-right @{u}...HEAD 2>/dev/null; or true)
                switch \"\$count\"
                    case '' '0'\t'0'
                        return 0
                    case '0'\t'*'
                        return 1
                    case '*'\t'0'
                        return 2
                    case '*'
                        return 3
                end
            " &
            function __prompt_git_upstream_$last_pid --on-process-exit $last_pid --inherit-variable $last_pid
                functions -e __prompt_git_upstream_$last_pid
                set -l ret $argv[3]
                switch $ret
                    case 0
                        set -g __prompt_git_upstream ''
                    case 1
                        set -g __prompt_git_upstream (set_color cyan)'⇡ '
                    case 2
                        set -g __prompt_git_upstream (set_color cyan)'⇣ '
                    case 3
                        set -g __prompt_git_upstream (set_color cyan)'⇡⇣ '
                end
            end
        end
    end
end
