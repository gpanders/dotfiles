set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_DATA_HOME $HOME/.local/share
set -gx XDG_CACHE_HOME $HOME/.cache
set -gx XDG_STATE_HOME $HOME/.local/state

if command -sq nvim
    set -qx EDITOR; or set -Ux EDITOR nvim
    set -qx VISUAL; or set -Ux VISUAL nvim
    set -qx MANPAGER; or set -Ux MANPAGER 'nvim +Man!'
end
