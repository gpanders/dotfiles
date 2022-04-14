local wezterm = require "wezterm"

local hostname
do
    local f = io.popen("uname -n")
    hostname = f:read("*l")
end

local domains = {}
do
    local ok, d = pcall(require, "domains")
    if ok then
        domains = d
    end
end

wezterm.on("update-right-status", function(window, pane)
    local workspace = window:active_workspace()
    local leader = ""
    if window:leader_is_active() then
        leader = " ^A "
    end
    window:set_right_status(wezterm.format({
        {Background={Color="#8fbcbb"}},
        {Foreground={Color="#3b4252"}},
        {Text=leader},
        {Background={Color="#373e4d"}},
        {Foreground={Color="#616e88"}},
        {Text=string.format(" %s / %s ", hostname, workspace)},
    }))
end)

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    local bg = "#373e4d"
    local fg = "#e5e9f0"
    local index = "#81a1c1"
    if tab.is_active then
        bg = "#4c566a"
        fg = "#eceff4"
        index = "#88c0d0"
    end

    local active_pane = tab.active_pane
    local title = string.match(active_pane.title, "^%s*(%S+)")

    return {
        {Background={Color=bg}},
        {Foreground={Color=index}},
        {Text=" "},
        {Text=tostring(tab.tab_index + 1)},
        {Text=" "},
        {Foreground={Color=fg}},
        {Text=title},
        {Text=active_pane.is_zoomed and " Z" or ""},
        {Text=" "},
    }
end)

local config = {
    color_scheme = "nord",
    font = wezterm.font("Iosevka Fixed SS05", {weight="Medium"}),
    font_size = 16,
    enable_csi_u_key_encoding = true,
    bold_brightens_ansi_colors = false,
    term = "wezterm",
    set_environment_variables = {
        TERMINFO_DIRS = os.getenv("HOME") .. "/.local/share/terminfo",
    },
    default_prog = {"/usr/local/bin/fish", "-l"},
    tab_bar_at_bottom = true,
    use_fancy_tab_bar = false,
    use_resize_increments = true,
    leader = { key="a", mods="CTRL" },
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
        {key="UpArrow", mods="SHIFT", action=wezterm.action{ScrollToPrompt=-1}},
        {key="DownArrow", mods="SHIFT", action=wezterm.action{ScrollToPrompt=1}},
        {key="Tab", mods="CTRL", action="DisableDefaultAssignment"},

        {key="a", mods="LEADER|CTRL", action={SendKey={key="a", mods="CTRL"}}},
        {key="a", mods="LEADER", action="ActivateLastTab"},
        {key="c", mods="LEADER", action=wezterm.action{SpawnTab="CurrentPaneDomain"}},
        {key="[",  mods="LEADER", action="ActivateCopyMode"},
        {key="\\", mods="LEADER", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
        {key="-",  mods="LEADER", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
        {key="x",  mods="LEADER", action=wezterm.action{CloseCurrentPane={confirm=true}}},
        {key="h",  mods="LEADER", action=wezterm.action{ActivatePaneDirection="Left"}},
        {key="h",  mods="LEADER|CTRL", action=wezterm.action{ActivatePaneDirection="Left"}},
        {key="j",  mods="LEADER", action=wezterm.action{ActivatePaneDirection="Down"}},
        {key="j",  mods="LEADER|CTRL", action=wezterm.action{ActivatePaneDirection="Down"}},
        {key="k",  mods="LEADER", action=wezterm.action{ActivatePaneDirection="Up"}},
        {key="k",  mods="LEADER|CTRL", action=wezterm.action{ActivatePaneDirection="Up"}},
        {key="l",  mods="LEADER", action=wezterm.action{ActivatePaneDirection="Right"}},
        {key="l",  mods="LEADER|CTRL", action=wezterm.action{ActivatePaneDirection="Right"}},
        {key="H", mods="LEADER|SHIFT", action=wezterm.action{AdjustPaneSize={"Left", 5}}},
        {key="J", mods="LEADER|SHIFT", action=wezterm.action{AdjustPaneSize={"Down", 5}}},
        {key="K", mods="LEADER|SHIFT", action=wezterm.action{AdjustPaneSize={"Up", 5}}},
        {key="L", mods="LEADER|SHIFT", action=wezterm.action{AdjustPaneSize={"Right", 5}}},
        {key="n", mods="LEADER", action={ActivateTabRelative=1}},
        {key="n", mods="LEADER|CTRL", action={ActivateTabRelative=1}},
        {key="p", mods="LEADER", action={ActivateTabRelative=-1}},
        {key="p", mods="LEADER|CTRL", action={ActivateTabRelative=-1}},
        {key="z", mods="LEADER", action="TogglePaneZoomState"},
        {key="s", mods="LEADER", action=wezterm.action{ShowLauncherArgs={flags="WORKSPACES"}}},
        {key="g", mods="LEADER", action=wezterm.action{ShowLauncherArgs={flags="FUZZY|WORKSPACES"}}},
    },

    ssh_domains = domains.ssh_domains,

    unix_domains = {
        {
            name = "unix",
        },
    },

    default_gui_startup_args = {"connect", "unix"},
}

for i = 1, 9 do
    table.insert(config.keys, {key=tostring(i), mods="LEADER", action={ActivateTab=i-1}})
end

return config
