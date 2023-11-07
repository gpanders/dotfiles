function __fish_shell_integration_update_pwd --on-variable PWD
    # Fish already does this automatically for some terminals, but it doesn't
    # hurt to do it again
    if not status --is-command-substitution; and not set -q INSIDE_EMACS
        printf '\e]7;file://%s%s\e\\' $hostname (string escape --style=url $PWD)
    end
end

function __fish_shell_integration_preexec --on-event fish_preexec
    # Reset cursor shape
    printf '\e[0 q'

    # End of input, start of output
    printf '\e]133;C\a'
end

function __fish_shell_integration_postexec --on-event fish_postexec
    # End of current command (report status code)
    printf '\e]133;D;%d\a' $status
end

function __fish_shell_integration_prompt_handler --on-event fish_prompt
    # Change the cursor to a beam on prompt
    printf '\e[5 q'
end

functions -c fish_prompt __fish_prompt

function fish_prompt
    printf '\e]133;A\a%b\e]133;B\a' (string join '\n' (__fish_prompt))
end
