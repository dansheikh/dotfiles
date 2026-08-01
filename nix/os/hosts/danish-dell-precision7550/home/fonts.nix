{ pkgs, ... }:

{
  # ── User-level fontconfig ─────────────────────────────────────────────────
  # System-level defaults are set in configuration.nix fonts.fontconfig.
  # These user overrides reinforce the same choices and satisfy apps that
  # read ~/.config/fontconfig/ rather than /etc/fonts/.
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      sansSerif = [ "Iosevka Nerd Font" "DejaVu Sans" ];
      serif     = [ "Iosevka Nerd Font" "DejaVu Serif" ];
      monospace = [ "Iosevka Nerd Font Mono" "DejaVu Sans Mono" ];
      emoji     = [ "Noto Color Emoji" ];
    };
  };

  # ── GTK font ─────────────────────────────────────────────────────────────
  # gtk.font applies to GTK window chrome, dialogs, menus.
  # Iosevka Nerd Font is proportional — correct choice for UI text.
  gtk.font = {
    name = "Iosevka Nerd Font";
    size = 11;
  };
}
