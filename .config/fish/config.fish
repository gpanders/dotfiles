set -qx XDG_CONFIG_HOME; or set -Ux XDG_CONFIG_HOME $HOME/.config
set -qx XDG_DATA_HOME; or set -Ux XDG_DATA_HOME $HOME/.local/share
set -qx XDG_CACHE_HOME; or set -Ux XDG_CACHE_HOME $HOME/.cache

if command -sq nvim
    set -qx EDITOR; or set -Ux EDITOR nvim
    set -qx VISUAL; or set -Ux VISUAL nvim
    set -qx MANPAGER; or set -Ux MANPAGER 'nvim +Man!'
end
