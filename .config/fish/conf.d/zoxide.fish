if not status is-interactive; or not command -sq zoxide
    exit
end

# Initialize hook to add new entries to the database.
function __zoxide_hook --on-variable PWD
    test -z "$fish_private_mode"
    and command zoxide add -- $PWD
end
