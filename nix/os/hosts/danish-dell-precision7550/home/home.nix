{ pkgs
, ...
}:

{
  imports = [
    # Shared HM modules (cross-host config lives in modules/home/)

    # Host-local HM modules
    ./fonts.nix
    ./noctalia.nix
    ./hyprland.nix
  ];

  # ── Identity ──────────────────────────────────────────────────────────────
  home.username = "danish";
  home.homeDirectory = "/home/danish";

  # ── Wallpapers ────────────────────────────────────────────────────────────
  # Wallpapers are managed manually at ~/Pictures/Wallpapers/.
  # Noctalia's wallpaper manager rotates through that directory at runtime.
  # No Nix management — populate the directory with your own images.

  # ── Stow boundary — HM does NOT manage these paths ────────────────────────
  # ~/.config/kitty/        — managed by stow
  # ~/.config/wezterm/      — managed by stow
  # ~/.config/nvim/         — managed by stow
  # ~/.emacs.d/ / emacs.org — managed by stow
  # ~/.config/zsh/          — managed by stow
  # ~/.config/starship.toml — managed by stow
  # ~/.config/nushell/      — managed by stow
  # ~/.config/fish/         — managed by stow
  # ~/.config/xonsh/        — managed by stow
  # xonsh binary + xontribs — managed by programs.xonsh in system.nix

  # ── GTK Theme ─────────────────────────────────────────────────────────────
  gtk = {
    enable = true;
    theme = {
      name = "catppuccin-macchiato-mauve-standard";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "mauve" ];
        size = "standard";
        tweaks = [ "rimless" ];
        variant = "macchiato";
      };
    };
    cursorTheme = {
      name = "catppuccin-macchiato-dark-cursors";
      package = pkgs.catppuccin-cursors.macchiatoDark;
      size = 24;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
  };

  # Persist GTK settings to dconf so apps pick them up
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      cursor-theme = "catppuccin-macchiato-dark-cursors";
      cursor-size = 24;
      icon-theme = "Papirus-Dark";
      gtk-theme = "catppuccin-macchiato-mauve-standard";
    };
  };

  # ── XDG ───────────────────────────────────────────────────────────────────
  xdg.enable = true;
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };

  # ── State Version ─────────────────────────────────────────────────────────
  home.stateVersion = "26.05";
}
