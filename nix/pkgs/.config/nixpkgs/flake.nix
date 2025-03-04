# Flake-based Home Manager: https://nix-community.github.io/home-manager/index.html#ch-nix-flakes
{
  description = "Home Manager Configuration";

  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager?ref=release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/hyprland";
    # hyprland-plugins = {
    #   url = "github:hyprwm/hyprland-plugins";
    #   inputs.hyprland.follows = "hyprland";
    # };

    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = inputs@{ home-manager, hyprland, nixpkgs, nixpkgs-unstable, ... }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
    in
    {
      homeConfigurations = {
        "dan.sheikh" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = { inherit inputs pkgs-unstable; };
          modules = [
            {
              home = {
                username = "dan.sheikh";
                homeDirectory = "/Users/dan.sheikh";
                stateVersion = "24.11";
              };
            }
            {
              nixpkgs.config.allowUnfree = true;
            }
            ../../../modules/home_manager/home.nix
          ];
        };
      };
    };
}
