function fish_title
    if not set -q INSIDE_EMACS
        if set -q fish_user_title
            echo $fish_user_title
        else
            echo (status current-command) (__fish_pwd)
        end
    end
end

