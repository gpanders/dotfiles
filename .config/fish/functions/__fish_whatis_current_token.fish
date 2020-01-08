# Defined in /Users/greande/.config/fish/functions/__fish_whatis_current_token.fish @ line 1
function __fish_whatis_current_token
    set -l tok (commandline -pt)

    if test $tok[1]
        printf "\n"
        whatis $tok[1]

        set -l line_count (count (fish_prompt))
        for x in (seq 2 $line_count)
            printf "\n"
        end

        commandline -f repaint
    end
end
