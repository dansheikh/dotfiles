local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.color_scheme = "Catppuccin Mocha"
config.enable_wayland = true
config.font = wezterm.font_with_fallback({ "Iosevka Nerd Font Mono", "VictorMono Nerd Font", "JetBrainsMono Nerd Font" })
config.font_size = 16
config.front_end = "OpenGL"
config.initial_cols = 360
config.initial_rows = 120
config.window_background_opacity = 1.0

return config
