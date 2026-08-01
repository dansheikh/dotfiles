-- Hyprland Native Lua Configuration

-- ── Variables ─────────────────────────────────────────────────────────────
local mainMod = "SUPER"
local terminal = "uwsm app -- kitty"
local fileManager = "uwsm app -- nemo"
local ipc = "noctalia msg"
local hyprshot = "hyprshot"

-- ── Event Hooks ───────────────────────────────────────────────────────────
hl.on("hyprland.start", function()
	hl.exec_cmd("uwsm finalize")
	hl.exec_cmd("uwsm app -- hypridle")
	hl.exec_cmd("uwsm app -- noctalia")
	hl.exec_cmd("uwsm app -- wl-paste --type text --watch cliphist store")
	hl.exec_cmd("uwsm app -- wl-paste --type image --watch cliphist store")
end)

-- ── Environment Variables ─────────────────────────────────────────────────
hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
hl.env("XCURSOR_THEME", "catppuccin-macchiato-dark-cursors")

-- ── Monitor Setup ─────────────────────────────────────────────────────────
hl.config({
	monitor = {
		-- Format: name, resolution@refresh, position, scale
		{ "eDP-1", "1920x1080@60", "0x0", "1" },
		-- Global fallback
		{ ",",     "preferred",    "0x0", "1" },
	},
})

-- ── General & Look/Feel ───────────────────────────────────────────────────
hl.config({
	general = {
		gaps_in = 5,
		gaps_out = 10,
		border_size = 2,
		col = {
			active_border = { colors = { "rgba(c6a0f6ee)", "rgba(8aadf4ee)" }, angle = 45 },
			inactive_border = "rgba(494d64aa)",
		},
		resize_on_border = true,
		allow_tearing = false,
		layout = "dwindle",
	},
	decoration = {
		rounding = 20,
		rounding_power = 2,
		active_opacity = 1.0,
		inactive_opacity = 0.95,
		shadow = {
			enabled = true,
			range = 4,
			render_power = 3,
			color = "rgba(1a1a1aee)",
		},
		blur = {
			enabled = true,
			size = 3,
			passes = 2,
			vibrancy = 0.1696,
		},
	},
	animations = {
		enabled = true,
	},
	dwindle = {
		preserve_split = true,
	},
	master = {
		new_status = "master",
	},
	misc = {
		force_default_wallpaper = 0,
		disable_hyprland_logo = true,
		initial_workspace_tracking = 2,
	},
	input = {
		kb_layout = "us",
		follow_mouse = 1,
		sensitivity = 0,
		touchpad = {
			natural_scroll = true,
		},
	},
})

-- ── Curves & Animations ───────────────────────────────────────────────────
hl.curve("easeOutQuint", { type = "bezier", points = { { 0.23, 1.0 }, { 0.32, 1.0 } } })
hl.curve("easeInOutCubic", { type = "bezier", points = { { 0.65, 0.05 }, { 0.36, 1.0 } } })
hl.curve("almostLinear", { type = "bezier", points = { { 0.5, 0.5 }, { 0.75, 1.0 } } })
hl.curve("quick", { type = "bezier", points = { { 0.15, 0.0 }, { 0.1, 1.0 } } })

hl.animation({ leaf = "global", enabled = true, speed = 10.0, bezier = "default" })
hl.animation({ leaf = "border", enabled = true, speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows", enabled = true, speed = 4.79, bezier = "easeOutQuint" })
hl.animation({ leaf = "windowsIn", enabled = true, speed = 4.1, bezier = "easeOutQuint", style = "popin 87%" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 1.49, bezier = "quick", style = "popin 87%" })
hl.animation({ leaf = "fade", enabled = true, speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers", enabled = true, speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn", enabled = true, speed = 4.0, bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut", enabled = true, speed = 1.5, bezier = "quick", style = "fade" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })

-- ── Gestures & Layer Rules ────────────────────────────────────────────────
hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })

hl.layer_rule({
	name = "noctalia-blur",
	match = { namespace = "noctalia" },
	blur = true,
})
hl.layer_rule({
	name = "noctalia-alpha",
	match = { namespace = "noctalia-background-.*" },
	ignore_alpha = 0.5,
})
hl.layer_rule({
	name = "noctalia-lock-center",
	match = { namespace = "^(noctalia-lock|ext-session-lock)$" },
	no_anim = true,
	blur = true,
})
-- ── Keybindings ───────────────────────────────────────────────────────────
hl.bind(mainMod .. " + Return", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + E", hl.dsp.exec_cmd(fileManager))
hl.bind(mainMod .. " + C", hl.dsp.window.close())
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + P", hl.dsp.window.pseudo())
hl.bind(mainMod .. " + J", hl.dsp.layout("togglesplit"))
hl.bind(mainMod .. " + Space", hl.dsp.exec_cmd(ipc .. " panel-toggle launcher"))
hl.bind(mainMod .. " + S", hl.dsp.exec_cmd(ipc .. " panel-toggle control-center"))
hl.bind(mainMod .. " + Comma", hl.dsp.exec_cmd(ipc .. " panel-toggle settings"))
hl.bind(mainMod .. " + CTRL + L", hl.dsp.exec_cmd(ipc .. " session lock"))
hl.bind(mainMod .. " + SHIFT + S", hl.dsp.exec_cmd(ipc .. " session lock-and-suspend"))

hl.bind(mainMod .. " + Escape", hl.dsp.exec_cmd(ipc .. " session lock && sleep 1 && hyprctl dispatch dpms off"))
hl.bind(mainMod .. " + SHIFT + Escape", hl.dsp.exec_cmd(ipc .. " session lock-and-suspend"))
hl.bind(mainMod .. " + CTRL + I", hl.dsp.exec_cmd("idle-toggle"))

hl.bind("Print", hl.dsp.exec_cmd(hyprshot .. " -m window"))
hl.bind(mainMod .. " + SHIFT + Print", hl.dsp.exec_cmd(hyprshot .. " -m region"))
hl.bind(mainMod .. " + Q", hl.dsp.exit())

-- Focus & Movement Controls
local dirs = { H = "left", L = "right", K = "up", j = "down", left = "left", right = "right", up = "up", down = "down" }
for key, dir in pairs(dirs) do
	hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ direction = dir }))
	hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ direction = dir }))
end

-- Workspaces
for i = 1, 9 do
	hl.bind(mainMod .. " + " .. i, hl.dsp.focus({ workspace = i }))
	hl.bind(mainMod .. " + SHIFT + " .. i, hl.dsp.window.move({ workspace = i }))
end
hl.bind(mainMod .. " + 0", hl.dsp.focus({ workspace = 10 }))
hl.bind(mainMod .. " + SHIFT + 0", hl.dsp.window.move({ workspace = 10 }))

hl.bind(mainMod .. " + grave", hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + SHIFT + grave", hl.dsp.window.move({ workspace = "special:magic" }))

hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up", hl.dsp.focus({ workspace = "e-1" }))

-- Media & System Keys
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd(ipc .. " volume-up"), { repeating = true, locked = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd(ipc .. " volume-down"), { repeating = true, locked = true })
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd(ipc .. " brightness-up"), { repeating = true, locked = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd(ipc .. " brightness-down"), { repeating = true, locked = true })
hl.bind("XF86AudioMute", hl.dsp.exec_cmd(ipc .. " volume-mute"), { locked = true })
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd(ipc .. " mic-mute"), { locked = true })
hl.bind("XF86AudioNext", hl.dsp.exec_cmd(ipc .. " media-next"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd(ipc .. " media-prev"), { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd(ipc .. " media-toggle"), { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd(ipc .. " media-toggle"), { locked = true })

hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- ── Window Rules ──────────────────────────────────────────────────────────
hl.window_rule({
	name = "suppress-maximize-events",
	match = { class = ".*", xwayland = true },
	suppress_event = "maximize",
})
hl.window_rule({
	name = "fix-xwayland-drags",
	match = { class = "^$", title = "^$" },
	no_focus = true,
})
