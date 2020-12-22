function fish_user_key_bindings
    bind \ef nextd-or-forward-word
    bind \eb prevd-or-backward-word

    bind \ct '__fuzzy_find fzy'
    bind \cr '__fuzzy_history fzy'
    bind \ec '__fuzzy_cd fzy'
end
