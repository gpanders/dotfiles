if status is-login
    if command -sq rbenv
        source (rbenv init - fish | psub)
    end
end
