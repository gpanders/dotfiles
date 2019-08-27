if status is-login
    if command -sq rbenv
        source (rbenv init - --no-rehash fish | psub)
    end
end
