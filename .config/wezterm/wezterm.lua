local wezterm = require 'wezterm';

-- https://wezfurlong.org/wezterm/config/keys.html

return {
    keys = {
        {key="t", mods="ALT", action=wezterm.action{SpawnTab="CurrentPaneDomain"}},
        {key="h", mods="ALT", action=wezterm.action{ActivatePaneDirection="Left"}},
        {key="l", mods="ALT", action=wezterm.action{ActivatePaneDirection="Right"}},
        {key="j", mods="ALT", action=wezterm.action{ActivatePaneDirection="Down"}},
        {key="k", mods="ALT", action=wezterm.action{ActivatePaneDirection="Up"}},
        {key="1", mods="ALT", action=wezterm.action{ActivateTab=1}},
        {key="2", mods="ALT", action=wezterm.action{ActivateTab=2}},
        {key="H", mods="CTRL", action=wezterm.action{ActivateTabRelative=-1}},
        {key="L", mods="CTRL", action=wezterm.action{ActivateTabRelative=1}},
        {key="\"", mods="CTRL|ALT", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
        {key="%", mods="CTRL|ALT", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}}
    },

    colors = {
        -- Tab bar colors are roughly based off of the Gruvbox Dark colors found at:
        -- https://github.com/mbadolato/iTerm2-Color-Schemes/blob/master/wezterm/Gruvbox%20Dark.toml
        tab_bar = {

            -- The color of the strip that goes along the top of the window
            background = "#1e1e1e",

            -- The active tab is the one that has focus in the window
            active_tab = {
                -- The color of the background area for the tab
                bg_color = "#1e1e1e",
                -- The color of the text for the tab
                fg_color = "#aab01e",

                -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
                -- label shown for this tab.
                -- The default is "Normal"
                intensity = "Bold",

                -- Specify whether you want "None", "Single" or "Double" underline for
                -- label shown for this tab.
                -- The default is "None"
                underline = "Single",

                -- Specify whether you want the text to be italic (true) or not (false)
                -- for this tab.  The default is false.
                italic = false,

                -- Specify whether you want the text to be rendered with strikethrough (true)
                -- or not for this tab.  The default is false.
                strikethrough = false,
            },

            -- Inactive tabs are the tabs that do not have focus
            inactive_tab = {
                bg_color = "#2f2f2f",
                fg_color = "#999",

                -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
                -- label shown for this tab.
                -- The default is "Normal"
                intensity = "Normal",

                -- The same options that were listed under the `active_tab` section above
                -- can also be used for `inactive_tab`.
            },

            -- You can configure some alternate styling when the mouse pointer
            -- moves over inactive tabs
            inactive_tab_hover = {
                bg_color = "#484848",
                fg_color = "#f7b125",
                italic = true,

                -- The same options that were listed under the `active_tab` section above
                -- can also be used for `inactive_tab_hover`.
            }
        }
    },

    font = wezterm.font("Inconsolata"),
    font_size = 12.0, -- This is the default font size anyway
    color_scheme = "Gruvbox Dark",
    hide_tab_bar_if_only_one_tab = true,
    tab_bar_at_bottom = true,
    window_background_opacity = 0.95,
    warn_about_missing_glyphs = false
}
