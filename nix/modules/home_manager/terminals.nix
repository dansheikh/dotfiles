{ config, lib, pkgs, pkgs-unstable, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.terminals;
  fontFamily = "VictorMono Nerd Font";
  fontSize = 16;
in
{
  options.terminals = {
    alacritty.enable = mkEnableOption "alacritty";
    # ghostty.enable = mkEnableOption "ghostty";
    kitty.enable = mkEnableOption "kitty";
    wezterm.enable = mkEnableOption "wezterm";
  };
  config = {
    programs.alacritty = mkIf cfg.alacritty.enable
      {
        enable = true;
        package = pkgs-unstable.alacritty;
        settings = {
          cursor = {
            style = {
              blinking = "On";
              shape = "Block";
            };
            blink_interval = 300;
            blink_timeout = 0;
            thickness = 0.15;
            unfocused_hollow = true;
          };
          bell = { duration = 0; };
          font = {
            bold = {
              family = "${fontFamily}";
              style = "Bold";
            };
            bold_italic = {
              family = "${fontFamily}";
              style = "Bold Italic";
            };
            italic = {
              family = "${fontFamily}";
              style = "Italic";
            };
            normal = { family = "${fontFamily}"; };
            size = fontSize + 0.0;
          };
          general = {
            live_config_reload = true;
          };
          keyboard = {
            bindings = [
              {
                key = "Copy";
                action = "Copy";
              }
              {
                key = "Paste";
                action = "Paste";
              }
              {
                key = "C";
                mods = "Command";
                action = "Copy";
              }
              {
                key = "V";
                mods = "Command";
                action = "Paste";
              }
            ];
          };
          selection = { save_to_clipboard = true; };
          scrolling = { history = 10000; };
          window = {
            decorations = "full";
            dimensions = {
              columns = 250;
              lines = 100;
            };
            dynamic_title = true;
            opacity = 1.0;
            option_as_alt = "Both";
            padding = {
              x = 5;
              y = 5;
            };
          };
        };
      };
    # programs.ghostty = mkIf cfg.ghostty.enable
    #   {
    #     enable = true;
    #     enableBashIntegration = true;
    #     enableFishIntegration = true;
    #     enableZshIntegration = true;
    #     package = pkgs-unstable.ghostty;
    #     settings = {
    #       font-family = "\"${fontFamily} Mono\"";
    #       font-size = fontSize;
    #     };
    #   };
    programs.kitty = mkIf cfg.kitty.enable
      {
        enable = true;
        font = {
          name = "family=\"${fontFamily} Mono\"";
          size = fontSize + 0.0;
        };
        package = pkgs-unstable.kitty;
        settings = {
          cursor_blink_interval = -1;
          cursor_shape = "block";
          cursor_stop_blinking_after = 30;
          initial_window_width = 1500;
          initial_window_height = 1200;
          macos_option_as_alt = "yes";
        };
        shellIntegration = {
          enableBashIntegration = true;
          enableFishIntegration = true;
          enableZshIntegration = true;
          mode = "no-cursor";
        };
      };
    programs.wezterm = mkIf cfg.wezterm.enable
      {
        enable = true;
        enableBashIntegration = true;
        enableZshIntegration = true;
        extraConfig = ''
          local config = {}
          config.color_scheme = 'Catppuccin Mocha'
          config.default_cursor_style = 'BlinkingBlock'
          config.font = wezterm.font_with_fallback {
            '${fontFamily}',
            'JetBrainsMono Nerd Font',
            'Noto Sans'
          }
          config.font_size = ${builtins.toString (fontSize + 0.0)}
          config.initial_cols = 250
          config.initial_rows = 100
          return config
        '';
        package = pkgs.wezterm;
      };
  };
}

