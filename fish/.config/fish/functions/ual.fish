function ual
    if test (count $argv) -lt 1
        echo "Which ual page do you want?"
        return
    end

    if set -q NOTES_PATH
        set NOTES "$NOTES_PATH"
    else
        set NOTES "$HOME/.notes"
    end

    switch $argv[1]
        case edit
            if test (count $argv) -lt 2
                echo "Which ual page do you want to edit?"
                return
            end
            mkdir -p "$NOTES"
            set NOTE "$NOTES"/$argv[2].md
            if not test -f "$NOTE"
                echo "# NAME\n\n$argv[2]\n" > "$NOTE"
            end
            $EDITOR "$NOTE"
        case rm
            if test (count $argv) -lt 2
                echo "Which ual page do you want to remove?"
                return
            end
            set NOTE "$NOTES"/$argv[2].md
            if not test -f "$NOTE"
                echo "No ual entry for $argv[2]" >&2
                return 1
            end
            rm "$NOTE"
        case ls
            printf "%s\n" (command ls $NOTES | sed 's/\.[^.]*$//')
        case sync
            echo -n "Syncing notes... "
            cd "$NOTES"; and git pull >/dev/null 2>&1; and git add .; and git commit -n >/dev/null 2>&1; and git commit -qv 2>/dev/null; and git push >/dev/null 2>&1
            echo "Done."
        case '*'
            set NOTE "$NOTES"/$argv[1].md

            if not test -f "$NOTE"
                echo "No ual entry for $argv[1]" >&2
                return 1
            end

            set TITLE (echo $argv[1] | tr '[:lower:]' '[:upper:]')
            set SECTION ual
            set AUTHOR "Greg Anders"
            set DATE (date +'%B %d, %Y' -r "$NOTE")

            pandoc \
                -s \
                -t man \
                -M title="$TITLE" \
                -M author="$AUTHOR" \
                -M section="$SECTION" \
                -M date="$DATE" \
                "$NOTE" | groff -T utf8 -man | less -is -+F -+X
    end
end
