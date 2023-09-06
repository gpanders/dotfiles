if not status is-interactive
    exit
end

abbr --add g git
abbr --add vi nvim

if command -sq fossil
    abbr --add f fossil
end
