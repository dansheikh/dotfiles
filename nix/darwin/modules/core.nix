{ inputs, system, ... }: {
  nix.settings.experimental-features = "nix-command flakes";
  nixpkgs.config.allowUnfree = true;
  nixpkgs.hostPlatform = system;
  nixpkgs.overlays = [
    (import ../overlays/default.nix { inherit inputs; })
  ];
  nix.optimise.automatic = true;

  system.stateVersion = 6;
}

