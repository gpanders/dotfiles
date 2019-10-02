function fmail -w mutt -d "Use fzf with khard to find email recipients"
    set -l addrs (khard email -p --remove-first-line | column -ts\t | fzy | awk '{print $1}')
    if test (count $addrs) -eq 0
        return 1
    end
    mutt $argv -- $addrs
end
