status is-interactive; or exit

set -gx LESS_TERMCAP_mb (printf "\e[1;31m")
set -gx LESS_TERMCAP_md (printf "\e[1;31m")
set -gx LESS_TERMCAP_me (printf "\e[0m")
set -gx LESS_TERMCAP_se (printf "\e[0m")
set -gx LESS_TERMCAP_so (printf "\e[1;44;33m")
set -gx LESS_TERMCAP_ue (printf "\e[0m")
set -gx LESS_TERMCAP_us (printf "\e[1;32m")
