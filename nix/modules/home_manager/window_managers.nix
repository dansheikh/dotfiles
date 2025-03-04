{ config, inputs, lib, pkgs, ... }:
let
  inherit (lib) lists mkEnableOption mkIf;
  cfg = config.windowManagers;
  fontFamily = "VictorMono Nerd Font";
  wallpapersDir = "~/wallpapers";
  startupScript = pkgs.writeShellScriptBin "startup" ''
    waybar &
    dunst &
    nm-applet --indicator &
    swww-daemon &
    sleep 1
    swww img ${wallpapersDir} --transition-type random &
  '';
in
{
  options.windowManagers = {
    hyprland.enable = mkEnableOption "hyprland";
    rofi.enable = mkEnableOption "rofi";
  };
  config = {
    home.packages = with pkgs;
      (lists.optionals (cfg.hyprland.enable) [ inputs.hyprland.homeManagerModules.default dunst networkmanagerapplet waybar ]);
    wayland.windowManager.hyprland = {
      enable = mkIf cfg.hyprland.enable true;
      settings = {
        "$mod" = "SUPER";
        animations = {
          enabled = "true";
        };
        bind = [
          "$mod, RETURN, exec, kitty"
          "$mod SHIFT, RETURN, exec, rofi -show window"
          "$mod, SPACE, exec, rofi -show drun"
          "$mod SHIFT, SPACE, exec, rofi -show run"
          "$mod, E, exec, emacsclient -c -a emacs"
          "$mod, Q, killactive,"
          "$mod, H, movefocus, l"
          "$mod, L, movefocus, r"
          "$mod, J, movefocus, u"
          "$mod, K, movefocus, d"
          "$mod SHIFT, H, movewindow, l"
          "$mod SHIFT, L, movewindow, r"
          "$mod SHIFT, J, movewindow, u"
          "$mod SHIFT, K, movewindow, d"
          "$mod, 0, workspace, 1"
          "$mod, 1, workspace, 2"
          "$mod, 2, workspace, 3"
          "$mod, 3, workspace, 4"
          "$mod, 4, workspace, 5"
          "$mod, 5, workspace, 6"
          "$mod, 6, workspace, 7"
          "$mod, 7, workspace, 8"
          "$mod, 8, workspace, 9"
          "$mod, 9, workspace, 10"
          "$mod SHIFT, 0, movetoworkspace, 1"
          "$mod SHIFT, 1, movetoworkspace, 2"
          "$mod SHIFT, 2, movetoworkspace, 3"
          "$mod SHIFT, 3, movetoworkspace, 4"
          "$mod SHIFT, 4, movetoworkspace, 5"
          "$mod SHIFT, 5, movetoworkspace, 6"
          "$mod SHIFT, 6, movetoworkspace, 7"
          "$mod SHIFT, 7, movetoworkspace, 8"
          "$mod SHIFT, 8, movetoworkspace, 9"
          "$mod SHIFT, 9, movetoworkspace, 10"
          "$mod, mouse_up, workspace, e+1"
          "$mod, mouse_down, workspace, e-1"
          "ALT, F, fullscreen,"
          "ALT, S, togglesplit,"
          "ALT, T, togglefloating,"
        ];
        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];
        decoration = {
          rounding = "10";
          active_opacity = "1.0";
          inactive_opacity = "0.75";
          fullscreen_opacity = "1.0";
          blur = {
            enabled = "true";
          };
        };
        exec-once = "${startupScript}/bin/startup";
        general = {
          border_size = "3";
          "col.active_border" = "rgba(3fc380d9) rgba(b7f4d8d9) 0deg";
          "col.inactive_border" = "rgba(4d13d1d9) rgba(4871f7d9) 0deg";
          cursor_inactive_timeout = "5";
          gaps_in = "5";
          gaps_out = "5";
          layout = "dwindle";
          no_border_on_floating = "false";
        };
        gestures = {
          workspace_swipe = "true";
          workspace_swipe_create_new = "false";
          workspace_swipe_fingers = "3";
        };
        master = {
          new_is_master = "true";
        };
        misc = {
          disable_hyprland_logo = "true";
          disable_splash_rendering = "true";
        };
      };
      xwayland.enable = true;
    };
    programs.rofi = mkIf cfg.rofi.enable
      {
        enable = true;
        extraConfig = {
          display-drun = "Application:";
          display-window = "Window:";
          drun-display-format = "{icon} {name}";
          font = "${fontFamily} 16";
          max-history-size = "25";
          modi = "drun,run,window";
          show-icons = true;
        };
        package = pkgs.rofi-wayland;
        theme =
          let
            inherit (config.lib.formats.rasi) mkLiteral;
          in
          {
            "*" = {
              font = "${fontFamily} 14";
              background-color = mkLiteral "#000000";
              foreground-color = mkLiteral "rgba(fafbfc)";
              selected-color = mkLiteral "rgba(b7f4d8d9)";
              border-color = mkLiteral "#FFFFFF";
              border = 0;
              margin = 0;
              padding = 0;
              spacing = 0;
            };
            "#window" = {
              anchor = mkLiteral "center";
              location = mkLiteral "center";
              orientation = mkLiteral "vertical";
              padding = mkLiteral "1em";
              width = mkLiteral "33%";
            };
            "#mainbox" = {
              orientation = mkLiteral "vertical";
              padding = mkLiteral "1em";
              children = map mkLiteral [ "inputbar" "listview" ];
            };
            "#inputbar" = { children = map mkLiteral [ "prompt" "entry" ]; };
            "#entry" = {
              expand = mkLiteral "false";
              width = mkLiteral "10em";
            };
            "#textbox-prompt-colon" = {
              expand = false;
              margin = mkLiteral "0em 0.3em 0em 0em";
              str = ":";
              text-color = mkLiteral "@foreground-color";
            };
            "#listview" = {
              layout = mkLiteral "horizontal";
              spacing = mkLiteral "1.5em";
              lines = 10;
            };
            "#element" = {
              padding = mkLiteral "0em 1.5em";
            };
            "#element selected" = {
              background-color = mkLiteral "@selected-color";
            };
          };
      };
  };
}
