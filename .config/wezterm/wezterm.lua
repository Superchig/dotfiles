local wezterm = require 'wezterm';

-- https://wezfurlong.org/wezterm/config/keys.html

return {
    keys = {
        {key="t", mods="ALT", action=wezterm.action{SpawnTab="CurrentPaneDomain"}},
        {key="h", mods="ALT", action=wezterm.action{ActivatePaneDirection="Left"}},
        {key="l", mods="ALT", action=wezterm.action{ActivatePaneDirection="Right"}},
        {key="1", mods="ALT", action=wezterm.action{ActivateTab=1}},
        {key="2", mods="ALT", action=wezterm.action{ActivateTab=2}},
        {key="H", mods="CTRL", action=wezterm.action{ActivateTabRelative=-1}},
        {key="L", mods="CTRL", action=wezterm.action{ActivateTabRelative=1}},
        {key="\"", mods="CTRL|ALT", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
        {key="%", mods="CTRL|ALT", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}}
    },

    font = wezterm.font("Inconsolata"),
    font_size = 12.0, -- This is the default font size anyway
    color_scheme = "Gruvbox Dark",
    hide_tab_bar_if_only_one_tab = true,
    window_background_opacity = 0.95,
    warn_about_missing_glyphs = false
}
