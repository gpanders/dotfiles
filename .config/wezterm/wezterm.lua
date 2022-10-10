local wezterm = require "wezterm"
local hostname = wezterm.hostname():lower()
local action = wezterm.action

local function merge(a, b)
    for k, v in pairs(b) do
        if a[k] == nil then
            a[k] = v
        end
    end
end

local config = {
    color_scheme = "nord",
    font = wezterm.font("Iosevka Fixed SS05", {weight="Medium"}),
    font_size = 15,
    bold_brightens_ansi_colors = false,
    term = "wezterm",
    set_environment_variables = {
        TERMINFO_DIRS = wezterm.home_dir .. "/.local/share/terminfo",
    },
    hide_tab_bar_if_only_one_tab = true,
    use_resize_increments = true,
    colors = {
        compose_cursor = "cyan",
        tab_bar = {
            background = "#373e4d",
            new_tab = {
                bg_color = "#373e4d",
                fg_color = "#aeb3bb",
            },
        },
    },
    keys = {
        {key="[", mods="CMD", action=action.ActivatePaneDirection "Prev"},
        {key="]", mods="CMD", action=action.ActivatePaneDirection "Next"},
        {key="d", mods="CMD", action=action.SplitHorizontal {domain="CurrentPaneDomain"}},
        {key="d", mods="CMD|SHIFT", action=action.SplitVertical {domain="CurrentPaneDomain"}},
        {key="h", mods="CMD", action=action.ActivatePaneDirection "Left"},
        {key="j", mods="CMD", action=action.ActivatePaneDirection "Down"},
        {key="k", mods="CMD", action=action.ActivatePaneDirection "Up"},
        {key="l", mods="CMD", action=action.ActivatePaneDirection "Right"},
        {key="Enter", mods="CMD", action=action.SplitHorizontal {domain="CurrentPaneDomain"}},
        {key="Enter", mods="CMD|SHIFT", action="TogglePaneZoomState"},
        {key="w", mods="CMD", action=action.CloseCurrentPane {confirm=true}},
    },
}

do
    local ok, localconf = pcall(require, hostname)
    if ok then
        merge(config, localconf)
    end
end

if not config.ssh_domains then
    config.ssh_domains = {}
end

for host in pairs(wezterm.enumerate_ssh_hosts()) do
    table.insert(config.ssh_domains, {
        name = host,
        remote_address = host,
        assume_shell = "Posix",
    })
end

return config
