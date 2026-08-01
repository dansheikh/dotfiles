{ pkgs, ... }:

{
  fonts = {
    packages = with pkgs.unstable; [
      nerd-fonts.iosevka
      nerd-fonts.jetbrains-mono
      nerd-fonts.victor-mono
      font-awesome
    ];

    fontconfig = {
      enable = true;
      defaultFonts = {
        # Proportional UI — Iosevka Nerd Font
        sansSerif = [ "Iosevka Nerd Font"      "DejaVu Sans"       ];
        serif     = [ "Iosevka Nerd Font"      "DejaVu Serif"      ];
        # Monospace / terminal — Iosevka Nerd Font Mono
        monospace = [ "Iosevka Nerd Font Mono" "DejaVu Sans Mono"  ];
        emoji     = [ "Noto Color Emoji"                           ];
      };
    };
  };
}
