function fish_user_key_bindings
    bind \ef nextd-or-forward-word
    bind -M insert \ef nextd-or-forward-word

    bind \eb prevd-or-backward-word
    bind -M insert \eb prevd-or-backward-word

    bind \ew __fish_whatis_current_token
    bind -M insert __fish_whatis_current_token

    bind \es __fish_prepend_sudo
    bind -M insert \es __fish_prepend_sudo
end
