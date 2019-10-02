function fish_user_key_bindings
    bind ! bind_bang
    bind -M insert ! bind_bang

    bind '$' bind_dollar
    bind -M insert '$' bind_dollar

    bind \ew __fish_whatis_current_token
    bind -M insert __fish_whatis_current_token

    bind \es __fish_prepend_sudo
    bind -M insert \es __fish_prepend_sudo
end
