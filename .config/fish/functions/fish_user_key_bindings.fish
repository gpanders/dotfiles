function fish_user_key_bindings
    bind \ef nextd-or-forward-word
    bind \eb prevd-or-backward-word
    bind \ew __fish_whatis_current_token
    bind \es __fish_prepend_sudo
    bind \ct __fzy
    bind \cr __fzy_history
    bind \ec __fzy_cd
end
