function fish_user_key_bindings
    bind -M insert ! __history_previous_command
    bind -M insert '$' __history_previous_command_arguments

    bind \ew __fish_whatis_current_token
    bind -M insert __fish_whatis_current_token

    bind \es __fish_prepend_sudo
    bind -M insert \es __fish_prepend_sudo

    bind \ct __fzy
    bind -M insert \ct __fzy

    bind \cr __fzy_history
    bind -M insert \cr __fzy_history
end
