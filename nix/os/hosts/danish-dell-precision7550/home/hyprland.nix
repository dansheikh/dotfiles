{ pkgs, lib, ... }:

{
  # ── Hypridle ──────────────────────────────────────────────────────────────
  services.hypridle = {
    enable = true;
    settings = {
      general = {
        # Trigger Noctalia's native lock screen before system suspension begins
        before_sleep_cmd = "${pkgs.noctalia}/bin/noctalia msg session lock";
        # Cleanly ensure display output is powered on when wake-up occurs
        after_sleep_cmd = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
        # Safely respect browser video playback & audio stream inhibitors
        ignore_dbus_inhibit = false;
        ignore_systemd_inhibit = false;
      };

      listener = [
        # RULE 1: Fade / Power down display after 2 minutes (120 seconds) of idle
        {
          timeout = 120;
          on-timeout = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
          on-resume = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
        }
        # RULE 2: Command systemd to suspend deep state after 5 minutes (300 seconds) of idle
        {
          timeout = 300;
          on-timeout = "${pkgs.systemd}/bin/systemctl suspend";
        }
      ];
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = false;
    configType = "lua";

    settings = {
      # ── Monitors ──────────────────────────────────────────────────────────
      monitor = {
        output = "";
        mode = "preferred";
        position = "auto";
        scale = "auto";
      };

      # ── Variables (_var) ──────────────────────────────────────────────────
      mainMod = {
        _var = "SUPER";
      };
      terminal = {
        _var = "uwsm app -- ${pkgs.unstable.kitty}/bin/kitty";
      };
      fileManager = {
        _var = "uwsm app -- ${pkgs.nemo}/bin/nemo";
      };
      ipc = {
        _var = "noctalia msg";
      };
      hyprshot = {
        _var = "${pkgs.hyprshot}/bin/hyprshot";
      };

      # ── Native Lua Event Hooks ────────────────────────────────────────────
      on = [
        {
          _args = [
            "hyprland.start"
            (lib.generators.mkLuaInline ''
              function()
                hl.exec_cmd("uwsm finalize")
                hl.exec_cmd("uwsm app -- ${pkgs.hypridle}/bin/hypridle")
                hl.exec_cmd("uwsm app -- noctalia")
                hl.exec_cmd("uwsm app -- wl-paste --type text  --watch cliphist store")
                hl.exec_cmd("uwsm app -- wl-paste --type image --watch cliphist store")
              end
            '')
          ];
        }
      ];

      # ── Environment Variables ─────────────────────────────────────────────
      env = [
        { _args = [ "XCURSOR_SIZE" "24" ]; }
        { _args = [ "HYPRCURSOR_SIZE" "24" ]; }
        { _args = [ "XCURSOR_THEME" "catppuccin-macchiato-dark-cursors" ]; }
      ];

      # ── Look and Feel ─────────────────────────────────────────────────────
      config = [
        {
          _args = [
            {
              general = {
                gaps_in = 5;
                gaps_out = 10;
                border_size = 2;
                col = {
                  active_border = { colors = [ "rgba(c6a0f6ee)" "rgba(8aadf4ee)" ]; angle = 45; };
                  inactive_border = "rgba(494d64aa)";
                };
                resize_on_border = true;
                allow_tearing = false;
                layout = "dwindle";
              };
              decoration = {
                rounding = 20;
                rounding_power = 2;
                active_opacity = 1.0;
                inactive_opacity = 0.95;

                shadow = {
                  enabled = true;
                  range = 4;
                  render_power = 3;
                  color = "rgba(1a1a1aee)";
                };
                blur = {
                  enabled = true;
                  size = 3;
                  passes = 2;
                  vibrancy = 0.1696;
                };
              };
              animations = {
                enabled = true;
              };
            }
          ];
        }
        {
          _args = [
            {
              dwindle = {
                preserve_split = true;
              };
            }
          ];
        }
        {
          _args = [
            {
              master = {
                new_status = "master";
              };
            }
          ];
        }
        {
          _args = [
            {
              misc = {
                force_default_wallpaper = 0; # Set to 0 to let Noctalia handle wallpaper cleanups
                disable_hyprland_logo = true;
                initial_workspace_tracking = 2;
              };
            }
          ];
        }
        {
          _args = [
            {
              input = {
                kb_layout = "us";
                follow_mouse = 1;
                sensitivity = 0;
                touchpad = {
                  natural_scroll = true;
                };
              };
            }
          ];
        }
      ];

      # ── Curves ────────────────────────────────────────────────────────────
      curve = [
        { _args = [ "easeOutQuint" { type = "bezier"; points = [ [ 0.23 1.0 ] [ 0.32 1.0 ] ]; } ]; }
        { _args = [ "easeInOutCubic" { type = "bezier"; points = [ [ 0.65 0.05 ] [ 0.36 1.0 ] ]; } ]; }
        { _args = [ "almostLinear" { type = "bezier"; points = [ [ 0.5 0.5 ] [ 0.75 1.0 ] ]; } ]; }
        { _args = [ "quick" { type = "bezier"; points = [ [ 0.15 0.0 ] [ 0.1 1.0 ] ]; } ]; }
      ];

      # ── Animations ────────────────────────────────────────────────────────
      animation = [
        { _args = [{ leaf = "global"; enabled = true; speed = 10.0; bezier = "default"; }]; }
        { _args = [{ leaf = "border"; enabled = true; speed = 5.39; bezier = "easeOutQuint"; }]; }
        { _args = [{ leaf = "windows"; enabled = true; speed = 4.79; bezier = "easeOutQuint"; }]; }
        { _args = [{ leaf = "windowsIn"; enabled = true; speed = 4.1; bezier = "easeOutQuint"; style = "popin 87%"; }]; }
        { _args = [{ leaf = "windowsOut"; enabled = true; speed = 1.49; bezier = "quick"; style = "popin 87%"; }]; }
        { _args = [{ leaf = "fade"; enabled = true; speed = 3.03; bezier = "quick"; }]; }
        { _args = [{ leaf = "layers"; enabled = true; speed = 3.81; bezier = "easeOutQuint"; }]; }
        { _args = [{ leaf = "layersIn"; enabled = true; speed = 4.0; bezier = "easeOutQuint"; style = "fade"; }]; }
        { _args = [{ leaf = "layersOut"; enabled = true; speed = 1.5; bezier = "quick"; style = "fade"; }]; }
        { _args = [{ leaf = "workspaces"; enabled = true; speed = 1.94; bezier = "almostLinear"; style = "fade"; }]; }
      ];

      # ── Gestures ──────────────────────────────────────────────────────────
      gesture = [
        { _args = [{ fingers = 3; direction = "horizontal"; action = "workspace"; }]; }
      ];

      # ── Layer Rules ───────────────────────────────────────────────────────
      layer_rule = [
        { _args = [{ name = "noctalia-blur"; match = { namespace = "noctalia"; }; blur = true; }]; }
        { _args = [{ name = "noctalia-alpha"; match = { namespace = "noctalia-background-.*"; }; ignore_alpha = 0.5; }]; }
      ];

      # ── Unified Keybindings System ────────────────────────────────────────
      bind = [
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + Return\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(terminal)") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + E\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(fileManager)") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + C\"") (lib.generators.mkLuaInline "hl.dsp.window.close()") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + V\"") (lib.generators.mkLuaInline "hl.dsp.window.float({ action = \"toggle\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + P\"") (lib.generators.mkLuaInline "hl.dsp.window.pseudo()") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + J\"") (lib.generators.mkLuaInline "hl.dsp.layout(\"togglesplit\")") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + Space\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" panel-toggle launcher\")") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + S\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" panel-toggle control-center\")") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + Comma\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" panel-toggle settings\")") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + CTRL + L\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" session lock\")") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + S\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" session lock-and-suspend\")") ]; }

        # Immediate Screen Lock and Screen Power Down (Chained safely inside bash wrapper)
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + Escape\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" session lock && sleep 1 && hyprctl dispatch dpms off\")") ]; }

        # Immediate Lock and Suspend (Unified via native Noctalia v5 session commands)
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + Escape\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" session lock-and-suspend\")") ]; }

        # Correctly call 'idle-toggle' wrapper script matching declaration name
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + CTRL + I\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(\"idle-toggle\")") ]; }

        { _args = [ "Print" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(hyprshot .. \" -m window\")") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + Print\"") (lib.generators.mkLuaInline "hl.dsp.exec_cmd(hyprshot .. \" -m region\")") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + Q\"") (lib.generators.mkLuaInline "hl.dsp.exit()") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + H\"") (lib.generators.mkLuaInline "hl.dsp.focus({ direction = \"left\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + L\"") (lib.generators.mkLuaInline "hl.dsp.focus({ direction = \"right\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + K\"") (lib.generators.mkLuaInline "hl.dsp.focus({ direction = \"up\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + j\"") (lib.generators.mkLuaInline "hl.dsp.focus({ direction = \"down\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + left\"") (lib.generators.mkLuaInline "hl.dsp.focus({ direction = \"left\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + right\"") (lib.generators.mkLuaInline "hl.dsp.focus({ direction = \"right\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + up\"") (lib.generators.mkLuaInline "hl.dsp.focus({ direction = \"up\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + down\"") (lib.generators.mkLuaInline "hl.dsp.focus({ direction = \"down\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + H\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ direction = \"left\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + L\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ direction = \"right\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + K\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ direction = \"up\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + J\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ direction = \"down\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + left\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ direction = \"left\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + right\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ direction = \"right\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + up\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ direction = \"up\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + down\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ direction = \"down\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 1\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 1 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 1\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 1 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 2\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 2 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 2\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 2 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 3\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 3 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 3\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 3 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 4\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 4 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 4\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 4 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 5\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 5 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 5\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 5 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 6\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 6 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 6\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 6 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 7\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 7 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 7\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 7 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 8\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 8 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 8\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 8 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 9\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 9 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 9\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 9 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + 0\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = 10 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + 0\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = 10 })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + grave\"") (lib.generators.mkLuaInline "hl.dsp.workspace.toggle_special(\"magic\")") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + SHIFT + grave\"") (lib.generators.mkLuaInline "hl.dsp.window.move({ workspace = \"special:magic\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + mouse_down\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = \"e+1\" })") ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + mouse_up\"") (lib.generators.mkLuaInline "hl.dsp.focus({ workspace = \"e-1\" })") ]; }
        { _args = [ "XF86AudioRaiseVolume" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" volume-up\")") { repeating = true; locked = true; } ]; }
        { _args = [ "XF86AudioLowerVolume" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" volume-down\")") { repeating = true; locked = true; } ]; }
        { _args = [ "XF86MonBrightnessUp" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" brightness-up\")") { repeating = true; locked = true; } ]; }
        { _args = [ "XF86MonBrightnessDown" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" brightness-down\")") { repeating = true; locked = true; } ]; }
        { _args = [ "XF86AudioMute" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" volume-mute\")") { locked = true; } ]; }
        { _args = [ "XF86AudioMicMute" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" mic-mute\")") { locked = true; } ]; }
        { _args = [ "XF86AudioNext" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" media-next\")") { locked = true; } ]; }
        { _args = [ "XF86AudioPrev" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" media-prev\")") { locked = true; } ]; }
        { _args = [ "XF86AudioPlay" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" media-toggle\")") { locked = true; } ]; }
        { _args = [ "XF86AudioPause" (lib.generators.mkLuaInline "hl.dsp.exec_cmd(ipc .. \" media-toggle\")") { locked = true; } ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + mouse:272\"") (lib.generators.mkLuaInline "hl.dsp.window.drag()") { mouse = true; } ]; }
        { _args = [ (lib.generators.mkLuaInline "mainMod .. \" + mouse:273\"") (lib.generators.mkLuaInline "hl.dsp.window.resize()") { mouse = true; } ]; }
      ];

      # ── Window Rules ──────────────────────────────────────────────────────
      window_rule = [
        {
          _args = [
            {
              name = "suppress-maximize-events";
              match = { class = ".*"; xwayland = true; };
              suppress_event = "maximize";
            }
          ];
        }
        {
          _args = [
            {
              name = "fix-xwayland-drags";
              match = { class = "^$"; title = "^$"; };
              no_focus = true;
            }
          ];
        }
      ];
    };
  };
}
