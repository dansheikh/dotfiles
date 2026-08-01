{ inputs, pkgs, ... }:

{
  # ── Nix ───────────────────────────────────────────────────────────────────
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    trusted-users = [ "@wheel" ];
    extra-substituters = [ "https://noctalia.cachix.org" ];
    extra-trusted-public-keys = [
      "noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4="
    ];
  };

  # Deduplicate store paths via a weekly scheduled systemd timer rather than
  # at write-time (auto-optimise-store) — no build overhead, no interruption risk.
  nix.optimise = {
    automatic = true;
    dates = [ "weekly" ];
  };

  # ── Nixpkgs ───────────────────────────────────────────────────────────────
  nixpkgs = {
    config.allowUnfree = true;
    # pkgs.unstable available everywhere via overlay — no specialArgs threading.
    # hostPlatform is set via nixosSystem { system = arch; } in flake.nix.
    overlays = import ../overlays/default.nix { inherit inputs; };
  };

  # ── Locale & Time ─────────────────────────────────────────────────────────
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # ── dconf (GTK settings backend) ──────────────────────────────────────────
  programs.dconf.enable = true;

  # ── XDG portals ───────────────────────────────────────────────────────────
  # pkgs is available directly — no need to re-import nixpkgs inline.
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-hyprland
      xdg-desktop-portal-gtk
    ];
    config.common.default = "*";
    xdgOpenUsePortal = true;
  };

  # ── State Version ─────────────────────────────────────────────────────────
  system.stateVersion = "26.05";
}
