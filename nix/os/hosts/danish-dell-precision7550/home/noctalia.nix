{ pkgs, ... }:

let
  idleTimeouts = {
    screenOff = 300;
    lock = 360;
    suspend = 1800;
  };
in

{
  xdg.configFile."noctalia/config.toml".source = (pkgs.formats.toml { }).generate "noctalia-config" {

    # ── General ─────────────────────────────────────────────────────────
    general = {
      avatar_image = "/home/danish/Pictures/Avatars/dan_sheikh.png";
      radius_ratio = 0.25;
    };

    # ── V5 Theme Architecture ───────────────────────────────────────────
    theme = {
      mode = "dark";
      source = "builtin";
      builtin = "Catppuccin";
    };

    # ── Location ────────────────────────────────────────────────────────
    location = {
      name = "Newburyport, MA";
      month_before_day = true;
    };

    # ── V5 Bar Profile Architecture ─────────────────────────────────────
    bar = {
      main = {
        position = "top";
        bar_type = "simple";
        density = "comfortable";
        background_opacity = 0.85;
        show_capsule = true;

        start = [
          "control-center"
          "network"
          "bluetooth"
        ];

        center = [
          "workspaces"
        ];

        end = [
          "taskbar"
          "volume"
          "brightness"
          "battery"
          "clock"
          "notifications"
          "tray"
        ];
      };
    };

    # ── V5 Widget Customizations ────────────────────────────────────────
    widget = {
      workspaces = {
        display = "none";
        hide_when_empty = true;
      };

      control-center = {
        # Use 'custom_image = "/path/to/logo.png"' or `U+` codepoints for a custom asset.
        glyph = "brand-nixos";
      };

      battery = {
        always_show_percentage = false;
        warning_threshold = 20;
      };

      clock = {
        format = "%H:%M";
        vertical_format = "%H %M";
        use_monospaced_font = true;
      };

      notifications = {
        hide_when_no_unread = false;
      };
    };

    # ── Wallpaper ───────────────────────────────────────────────────────
    wallpaper = {
      wallpapers_folder = "/home/danish/Pictures/Wallpapers";
      enable_overview_wallpaper = true;
      default = {
        path = "/home/danish/Pictures/Wallpapers/galaxy.jpg";
      };
      order = "random";
      change_interval = 1800;
    };

    # ── Idle & Lock ─────────────────────────────────────────────────────
    idle = {
      enabled = false;
      screen_off_timeout = idleTimeouts.screenOff;
      lock_timeout = idleTimeouts.lock;
      suspend_timeout = idleTimeouts.suspend;
    };
  };
}
