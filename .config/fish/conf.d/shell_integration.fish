function __fish_shell_integration_update_pwd --on-variable PWD
    # Fish already does this automatically for some terminals, but it doesn't
    # hurt to do it again
    if not status --is-command-substitution; and not set -q INSIDE_EMACS
        printf '\x1b]7;file://%s%s\x1b\x5c' $hostname (string escape --style=url $PWD)
    end
end

function __fish_shell_integration_preexec --on-event fish_preexec
    # Reset cursor shape
    printf '\x1b[0 q'

    # End of input, start of output
    printf '\x1b]133;C\x1b\x5c'
end

function __fish_shell_integration_postexec --on-event fish_postexec
    # End of current command (report status code)
    printf '\x1b]133;D;%d\x1b\x5c' $status
end

function __fish_shell_integration_prompt_handler --on-event fish_prompt
    # Change the cursor to a beam on prompt
    printf '\x1b[5 q'

    # Start of prompt
    printf '\x1b]133;A\x1b\x5c'
end

functions -c fish_prompt __fish_prompt

function fish_prompt
    printf '\x1b]133;P\x1b\x5c%b\x1b]133;B\x1b\x5c' (string join '\n' (__fish_prompt))
end
